package main

import (
	"testing"
)

func TestEqual(tt *testing.T) {
	if !(Equal([]byte{'f', 'u', 'z', 'z'}, []byte{'f', 'u', 'z', 'z'})) {
		tt.Error("expected true, got false")
	}
}
func FuzzEqual(f *testing.F) {
	// run this test with `go test -fuzz=. -fuzztime=5s .`
	// fuzzing can run out of memory, be careful when using it in CI environment
	f.Fuzz(func(tt *testing.T, a []byte, b []byte) {
		Equal(a, b)
	})
}
