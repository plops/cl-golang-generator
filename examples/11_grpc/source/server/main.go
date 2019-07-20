package main

import (
	"sync"
)

type Func func(key string) (interface{}, error)
type result struct {
	value interface{}
	err   error
}
