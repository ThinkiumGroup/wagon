// Copyright 2017 The go-interpreter Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package exec

import (
	"fmt"
	"math"
	"reflect"

	"github.com/ThinkiumGroup/wagon/exec/internal/compile"
)

type function interface {
	call(vm *VM, index int64)
}

type compiledFunction struct {
	code           []byte
	codeMeta       *compile.BytecodeMetadata
	branchTables   []*compile.BranchTable
	maxDepth       int  // maximum stack depth reached while executing the function body
	totalLocalVars int  // number of local variables used by the function
	args           int  // number of arguments the function accepts
	returns        bool // whether the function returns a value

	asm []asmBlock
}

type asmBlock struct {
	// Compiled unit in native machine code.
	nativeUnit compile.NativeCodeUnit
	// where in the instruction stream to resume after native execution.
	resumePC uint
}

type GasContext interface {
	UseGas(gas uint64) bool // use gas, return true if success, or it's an ErrOutOfGas
	GasUsed() uint64        // gas already used
	GasLeft() uint64        // gas left
	GasLimit() uint64       // gas limit
	GasInfo() string        // information string
}

type goFunction struct {
	val        reflect.Value
	typ        reflect.Type
	name       string        // method name
	checkGas   bool          // whether check gas out side of the function
	gasCounter reflect.Value // returns gas with params. same signature with function, but returns an uint64
	fixedGas   uint64        // gas of function if GasCounter not available
}

func (fn goFunction) call(vm *VM, index int64) {
	// numIn = # of call inputs + vm, as the function expects
	// an additional *VM argument
	numIn := fn.typ.NumIn()
	args := make([]reflect.Value, numIn)
	proc := NewProcess(vm)

	// Pass proc as an argument. Check that the function indeed
	// expects a *Process argument.
	if reflect.ValueOf(proc).Kind() != fn.typ.In(0).Kind() {
		panic(fmt.Sprintf("exec: the first argument of a host function was %s, expected %s", fn.typ.In(0).Kind(), reflect.ValueOf(vm).Kind()))
	}
	args[0] = reflect.ValueOf(proc)

	for i := numIn - 1; i >= 1; i-- {
		val := reflect.New(fn.typ.In(i)).Elem()
		raw := vm.popUint64()
		kind := fn.typ.In(i).Kind()

		switch kind {
		case reflect.Float64, reflect.Float32:
			val.SetFloat(math.Float64frombits(raw))
		case reflect.Uint32, reflect.Uint64:
			val.SetUint(raw)
		case reflect.Int32, reflect.Int64:
			val.SetInt(int64(raw))
		default:
			panic(fmt.Sprintf("exec: args %d invalid kind=%v", i, kind))
		}

		args[i] = val
	}

	if fn.checkGas {
		gas := uint64(0)
		if fn.gasCounter.IsValid() && fn.gasCounter.IsNil() == false {
			gasRts := fn.gasCounter.Call(args)
			gas = gasRts[0].Uint()
		} else if fn.fixedGas > 0 && fn.fixedGas < math.MaxUint64 {
			gas = fn.fixedGas
		}
		if gas > 0 {
			gasCtx, ok := vm.HostCtx().(GasContext)
			if !ok || gasCtx == nil {
				panic(fmt.Errorf("missing GasContext at %s", vm.ctx))
			}
			if !gasCtx.UseGas(gas) {
				panic(fmt.Errorf("out of gas at %s", vm.ctx))
			}
		}
	}

	rtrns := fn.val.Call(args)
	for i, out := range rtrns {
		kind := out.Kind()
		switch kind {
		case reflect.Float64, reflect.Float32:
			vm.pushFloat64(out.Float())
		case reflect.Uint32, reflect.Uint64:
			vm.pushUint64(out.Uint())
		case reflect.Int32, reflect.Int64:
			vm.pushInt64(out.Int())
		default:
			panic(fmt.Sprintf("exec: return value %d invalid kind=%v", i, kind))
		}
	}
}

func (compiled compiledFunction) call(vm *VM, index int64) {
	// Make space on the stack for all intermediate values and
	// a possible return value.
	newStack := make([]uint64, 0, compiled.maxDepth+1)
	locals := make([]uint64, compiled.totalLocalVars)

	for i := compiled.args - 1; i >= 0; i-- {
		locals[i] = vm.popUint64()
	}

	// save execution context
	prevCtxt := vm.ctx

	vm.ctx = context{
		stack:   newStack,
		locals:  locals,
		code:    compiled.code,
		asm:     compiled.asm,
		pc:      0,
		curFunc: index,
	}

	rtrn := vm.execCode(compiled)

	// restore execution context
	vm.ctx = prevCtxt

	if compiled.returns {
		vm.pushUint64(rtrn)
	}
}
