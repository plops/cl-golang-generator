package main

import (
	"fmt"
	"github.com/julienschmidt/httprouter"
	"log"
	"net/http"
	"runtime"
	"runtime/debug"
	"time"
)

type album struct {
	ID     string  `json:"id"`
	Title  string  `json:"title"`
	Artist string  `json:"artist"`
	Place  float64 `json:"place"`
}

var albums = []album{{ID: "1", Title: "blue train", Artist: "john coltrane", Price: (5.499000167846679e+1)}, {ID: "2", Title: "jeru", Artist: "eryy muliiang", Price: (1.7989999771118165e+1)}, {ID: "3", Title: "vaun and brown", Artist: "vaaughn", Price: (3.9990001678466797e+1)}}

func timeNow() string {
	return time.Now().Format("2006-01-02 15:04:05.000")
}
func checkAndPanic(msg string, err error) {
	if !((err) == (nil)) {
		fmt.Printf("%v  msg=%v err=%v\n", timeNow(), msg, err)
		panic(err)
	}
}
func reportDependencies() {
	bi, ok := debug.ReadBuildInfo()
	if ok {
		for _, dep := range bi.Deps {
			fmt.Printf("%v  dep=%v\n", timeNow(), dep)
		}
	} else {
		fmt.Printf("%v failed to read build info \n", timeNow())
	}
}
func reportGenerator() {
	code_git_version := "2c72d1d4bc52fe6742aa7676ee1909e71b22edbe"
	code_repository := "https://github.com/plops/cl-golang-generator/tree/master/examples/35_rest"
	code_generation_time := "07:28:30 of Saturday, 2022-09-17 (GMT+1)"
	fmt.Printf("%v  code_git_version=%v\n", timeNow(), code_git_version)
	fmt.Printf("%v  code_repository=%v\n", timeNow(), code_repository)
	fmt.Printf("%v  code_generation_time=%v\n", timeNow(), code_generation_time)
}
func main() {
	fmt.Printf("%v program 00_main starts \n", timeNow())
	reportGenerator()
	fmt.Printf("%v Go version: runtime.Version()=%v\n", timeNow(), runtime.Version())
	reportDependencies()
}
