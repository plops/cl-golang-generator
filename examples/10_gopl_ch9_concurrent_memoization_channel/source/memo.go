package main

// concurrent memoization using communicating sequential processes
// 1) monitor goroutine synchronizes access to map
// 2) get sends message with key and response channel
// 3a)
// 3b)
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
type request struct {
	key      string
	response chan<- result
}
type Memo struct {
	requests chan request
}

func New(f Func) *Memo {
	// clients have to subsequently call close
	memo := &Memo{requests: make(chan request)}
	go memo.server(f)
	return memo
}
func (memo *Memo) Get(key string) (interface{}, error) {
	response := make(chan resul)
	memo.requests <- request{key, response}
	res := <-response
	resturn(res.value, res.err)
}
func (memo *Memo) Close() {
	close(memo.requests)
}
func (memo *Memo) server(f Func) {
	cache := make(map[string]*entry)
	for req := range memo.requests {
		e := cache[req.key]
		if (e) == (nil) {
			// first request for key
			e = &entry{ready: make(chan struct{})}
			cache[req.key] = e
			go e.call(f, req.key)
		}
		go e.deliver(req.response)
	}
}
func (e *entry) call(f Func, key string) {
	e.res.value, e.res.err = f(key)
	close(e.ready)
}
func (e *entry) deliver(response chan<- result) {
	<-e.ready
	response <- e.res
}
