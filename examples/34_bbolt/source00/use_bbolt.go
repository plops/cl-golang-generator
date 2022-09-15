package main

import (
	"fmt"
	bolt "go.etcd.io/bbolt"
	"runtime/debug"
	"time"
)

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
	code_git_version := "581d4c2ef34c20cdb06251336d14d9d9f750e13f"
	code_repository := "https://github.com/plops/cl-golang-generator/tree/master/examples/34_bbolt"
	code_generation_time := "18:37:16 of Thursday, 2022-09-15 (GMT+1)"
	fmt.Printf("%v  code_git_version=%v\n", timeNow(), code_git_version)
	fmt.Printf("%v  code_repository=%v\n", timeNow(), code_repository)
	fmt.Printf("%v  code_generation_time=%v\n", timeNow(), code_generation_time)
}
func main() {
	fmt.Printf("%v program use_bbolt starts \n", timeNow())
	reportGenerator()
	reportDependencies()
	db_path := "data.db"
	fmt.Printf("%v open database db_path=%v\n", timeNow(), db_path)
	db, err00 := bolt.Open(db_path, 0666, nil)
	checkAndPanic("bolt.Open(db_path, 0666, nil)", err00)
	defer (func() {
		fmt.Printf("%v close database db_path=%v db=%v\n", timeNow(), db_path, db)
		err01 := db.Close()
		checkAndPanic("db.Close()", err01)
	})()
}
