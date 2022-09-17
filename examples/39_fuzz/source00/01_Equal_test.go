package main

import (
	"testing"
)

func TestEqual(tt *testing.T) {
	if !(Equal([]byte{'f', 'u', 'z', 'z'}, []byte{'f', 'u', 'z', 'z'})) {
		tt.Error("expected true, got false")
	}
}
