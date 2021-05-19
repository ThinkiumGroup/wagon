package exec

import (
	"errors"
	"fmt"
	"reflect"

	"github.com/ThinkiumGroup/wagon/disasm"
	"github.com/ThinkiumGroup/wagon/exec/internal/compile"
	"github.com/ThinkiumGroup/wagon/wasm"
)

type CompileVM struct {
	VM
}

type CompiledModule struct {
	RawModule *wasm.Module
	globals   []uint64
	memory    []byte
	funcs     []function
}

func CompileModule(module *wasm.Module) (*CompiledModule, error) {
	var compiled CompiledModule

	if module.Memory != nil && len(module.Memory.Entries) != 0 {
		if len(module.Memory.Entries) > 1 {
			return nil, ErrMultipleLinearMemories
		}

		memsize := uint(module.Memory.Entries[0].Limits.Initial) * wasmPageSize
		compiled.memory = make([]byte, memsize)
		copy(compiled.memory, module.LinearMemoryIndexSpace[0])
	}

	compiled.funcs = make([]function, len(module.FunctionIndexSpace))
	compiled.globals = make([]uint64, len(module.GlobalIndexSpace))
	compiled.RawModule = module

	nNatives := 0
	for i, fn := range module.FunctionIndexSpace {
		// Skip native methods as they need not be
		// disassembled; simply add them at the end
		// of the `funcs` array as is, as specified
		// in the spec. See the "host functions"
		// section of:
		// https://webassembly.github.io/spec/core/exec/modules.html#allocation
		if fn.IsHost() {
			compiled.funcs[i] = goFunction{
				typ:        fn.Host.Type(),
				val:        fn.Host,
				name:       fn.Name,
				checkGas:   fn.CheckGas,
				gasCounter: fn.GasCounter,
				fixedGas:   fn.FixedGas,
			}
			nNatives++
			continue
		}

		disassembly, err := disasm.NewDisassembly(fn, module)
		if err != nil {
			return nil, err
		}

		totalLocalVars := 0
		totalLocalVars += len(fn.Sig.ParamTypes)
		for _, entry := range fn.Body.Locals {
			totalLocalVars += int(entry.Count)
		}
		code, meta := compile.Compile(disassembly.Code)
		compiled.funcs[i] = compiledFunction{
			code:           code,
			branchTables:   meta.BranchTables,
			maxDepth:       disassembly.MaxDepth,
			totalLocalVars: totalLocalVars,
			args:           len(fn.Sig.ParamTypes),
			returns:        len(fn.Sig.ReturnTypes) != 0,
		}
	}

	for i, global := range module.GlobalIndexSpace {
		val, err := module.ExecInitExpr(global.Init)
		if err != nil {
			return nil, err
		}
		switch v := val.(type) {
		case int32:
			compiled.globals[i] = uint64(v)
		case int64:
			compiled.globals[i] = uint64(v)
			// case float32:
			// 	compiled.globals[i] = uint64(math.Float32bits(v))
			// case float64:
			// 	compiled.globals[i] = uint64(math.Float64bits(v))
		}
	}

	if module.Start != nil {
		// _, err := compiled.ExecCode(int64(module.Start.Index))
		// if err != nil {
		// 	return nil, err
		// }
		return nil, errors.New("start entry is not supported in smart contract")
	}

	return &compiled, nil
}

func NewVMWithCompiled(module *CompiledModule, memLimit uint64) (*CompileVM, error) {
	var vm CompileVM

	memsize := len(module.memory)
	if uint64(memsize) > memLimit {
		return nil, fmt.Errorf("memory is exceed the limitation of %d", memLimit)
	}
	vm.MemoryLimitation = memLimit
	vm.memory = make([]byte, memsize)
	copy(vm.memory, module.memory)

	vm.funcs = module.funcs
	vm.globals = make([]uint64, len(module.RawModule.GlobalIndexSpace))
	copy(vm.globals, module.globals)
	vm.newFuncTable()
	vm.module = module.RawModule

	return &vm, nil
}

var (
	ErrMissingProcess = errors.New("missing *Process parameter")
	ProcType          = reflect.TypeOf((*Process)(nil))
)

func getTypeOf(kind reflect.Kind) (wasm.ValueType, error) {
	switch kind {
	case reflect.Float64:
		return wasm.ValueTypeF64, nil
	case reflect.Float32:
		return wasm.ValueTypeF32, nil
	case reflect.Int32, reflect.Uint32:
		return wasm.ValueTypeI32, nil
	case reflect.Int64, reflect.Uint64:
		return wasm.ValueTypeI64, nil
	default:
		return 0x00, fmt.Errorf("invalid type: %s", kind.String())
	}
}

func getSignature(typ reflect.Type) (*wasm.FunctionSig, error) {
	var err error
	// the first parameter must be *Process
	if typ.NumIn() < 1 {
		return nil, ErrMissingProcess
	}
	if typ.In(0) != ProcType {
		return nil, ErrMissingProcess
	}
	in := make([]wasm.ValueType, typ.NumIn()-1)
	for i := 1; i < typ.NumIn(); i++ {
		in[i-1], err = getTypeOf(typ.In(i).Kind())
		if err != nil {
			return nil, err
		}
	}

	out := make([]wasm.ValueType, typ.NumOut())
	for i := range out {
		out[i], err = getTypeOf(typ.Out(i).Kind())
		if err != nil {
			return nil, err
		}
	}
	return &wasm.FunctionSig{Form: wasm.TypeFunc, ParamTypes: in, ReturnTypes: out}, nil
}

type ModuleBuilder struct {
	modules map[string]*wasm.Module
}

func NewModuleBuilder() *ModuleBuilder {
	return &ModuleBuilder{modules: make(map[string]*wasm.Module)}
}

func (m *ModuleBuilder) Done() map[string]*wasm.Module {
	return m.modules
}

// func (m *ModuleBuilder) Exporter(modName string) (*wasm.Module, error) {
// 	mod, ok := m.modules[modName]
// 	if !ok || mod == nil {
// 		return nil, fmt.Errorf("unknown module %s", modName)
// 	}
// 	return mod, nil
// }

func (m *ModuleBuilder) SetFunction(modName, funcName string, funcValue reflect.Value,
	checkGas bool, fixedGas uint64, gasCounter ...reflect.Value) error {
	mod, ok := m.modules[modName]
	if !ok {
		mod = wasm.NewModule()
		mod.Export.Entries = make(map[string]wasm.ExportEntry)
		m.modules[modName] = mod
	}

	sig, err := getSignature(funcValue.Type())
	if err != nil {
		return err
	}

	typesLen := len(mod.Types.Entries)
	mod.Types.Entries = append(mod.Types.Entries, *sig)

	if !checkGas {
		fixedGas = 0
	}
	function := wasm.Function{
		Sig:      &mod.Types.Entries[typesLen],
		Body:     &wasm.FunctionBody{},
		Host:     funcValue,
		Name:     funcName,
		CheckGas: checkGas,
		FixedGas: fixedGas,
	}
	if checkGas && len(gasCounter) > 0 && gasCounter[0].IsValid() && gasCounter[0].IsNil() == false {
		function.GasCounter = gasCounter[0]
	}

	funcLen := len(mod.FunctionIndexSpace)
	mod.FunctionIndexSpace = append(mod.FunctionIndexSpace, function)

	export := wasm.ExportEntry{FieldStr: funcName, Kind: wasm.ExternalFunction}
	export.Index = uint32(funcLen)
	mod.Export.Entries[funcName] = export

	return nil
}

func (m *ModuleBuilder) MustSetFunction(modName, funcName string, funcValue reflect.Value, gasCounter ...reflect.Value) {
	if err := m.SetFunction(modName, funcName, funcValue, true, 0, gasCounter...); err != nil {
		panic(err)
	}
}

func (m *ModuleBuilder) MustSetFunctionFixedGas(modName, funcName string, funcValue reflect.Value, fixedGas uint64) {
	if err := m.SetFunction(modName, funcName, funcValue, true, fixedGas); err != nil {
		panic(err)
	}
}

func (m *ModuleBuilder) MustSetFunctionNoGasCheck(modName, funcName string, funcValue reflect.Value) {
	if err := m.SetFunction(modName, funcName, funcValue, false, 0); err != nil {
		panic(err)
	}
}
