package main

// concurrent memoization using shared variables and locks:
// 1) call to Get acquires mutex lock on the cache
// 2) is there an existing entry?
// 3a) yes: wait for ready condition
// 3b) no: write 'not ready' into the map, when result arrives
//     replace with result and broadcast ready
import (
	"sync"
)

type Func func(key string) (interface{}, error)
type result struct {
	value interface{}
	err   error
}
type entry struct {
	res   result
	ready chan struct{}
}

func New(f Func) *Memo {
	return &Memo{f: f, cache: make(map[string]*entry)}
}

type Memo struct {
	f     Func
	mu    sync.Mutex
	cache map[string]*entry
}

func (memo *Memo) Get(key string) (interface{}, error) {
	memo.mu.Lock()
	e := memo.cache[key]
	if (e) == (nil) {
		// 3b) first request for key
		e = &entry{ready: make(chan struct{})}
		memo.cache[key] = e
		memo.mu.Unlock()
		e.res.value, e.res.err = memo.f(key)
		close(e.ready)
	} else {
		// 3a) repeat request
		memo.mu.Unlock()
		<-e.ready
	}
	return e.res.value, e.res.err
}
