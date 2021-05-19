package compile

const (
	statusMask    = 15
	exitIndexMask = 0x00000000ffffff00
)

// JITExitSignal is the value returned from the execution of a native section.
// The bits of this packed 64bit value is encoded as follows:
// [00:04] Completion Status
// [04:08] Reserved
// [08:32] Index of the WASM instruction where the exit occurred.
// [32:64] Status-specific 32bit value.
type JITExitSignal uint64

// CompletionStatus decodes and returns the completion status of the exit.
func (s JITExitSignal) CompletionStatus() CompletionStatus {
	return CompletionStatus(s & statusMask)
}

// Index returns the index to the instruction where the exit happened.
// 0xffffff is returned if the exit was due to normal completion.
func (s JITExitSignal) Index() int {
	return (int(s) & exitIndexMask) >> 8
}
