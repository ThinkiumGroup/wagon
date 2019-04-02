// Copyright 2019 The go-interpreter Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build !appengine

package compile

import "unsafe"

type asmBlock struct {
	mem unsafe.Pointer
}

func (b *asmBlock) Invoke(stack, locals *[]uint64) {
	jitcall(unsafe.Pointer(&b.mem), stack, locals)
}
