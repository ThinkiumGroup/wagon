// +build !appengine

package compile

import "unsafe"

func jitcall(asm unsafe.Pointer, stack, locals, globals *[]uint64, mem *[]byte) uint64 {
	panic("not implemented for arm64")
}
