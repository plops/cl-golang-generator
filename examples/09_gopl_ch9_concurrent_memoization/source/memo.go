package main

// concurrent memoization using shared variables and locks:
// 1) call to Get acquires mutex lock on the cache
// 2) is there an existing entry?
// 3a) yes: wait for ready condition
// 3b) no: write 'not ready' into the map, when result arrives replace with result
import (
	"sync"
)

type entry struct {
	res   result
	ready chan struct{}
}
