package main

import (
	"encoding/binary"
	"fmt"
	bolt "go.etcd.io/bbolt"
	"os"
	"path/filepath"
	"runtime"
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
	code_git_version := "7b98d88a2c66e7abc5bc975c85de4d0384c0702b"
	code_repository := "https://github.com/plops/cl-golang-generator/tree/master/examples/34_bbolt"
	code_generation_time := "19:52:30 of Thursday, 2022-09-15 (GMT+1)"
	fmt.Printf("%v  code_git_version=%v\n", timeNow(), code_git_version)
	fmt.Printf("%v  code_repository=%v\n", timeNow(), code_repository)
	fmt.Printf("%v  code_generation_time=%v\n", timeNow(), code_generation_time)
}
func itob(v uint64) []byte {
	b := make([]byte, 8)
	binary.BigEndian.PutUint64(b, uint64(v))
	return b
}
func main() {
	fmt.Printf("%v program use_bbolt starts \n", timeNow())
	reportGenerator()
	fmt.Printf("%v Go version: runtime.Version()=%v\n", timeNow(), runtime.Version())
	reportDependencies()
	db_path := "data.db"
	fmt.Printf("%v open database db_path=%v\n", timeNow(), db_path)
	db, err00 := bolt.Open(db_path, 0666, &bolt.Options{Timeout: ((1) * (time.Second))})
	checkAndPanic("bolt.Open(db_path, 0666, &bolt.Options {Timeout: ((1)*(time.Second))})", err00)
	defer (func() {
		fmt.Printf("%v close database db_path=%v db=%v\n", timeNow(), db_path, db)
		err01 := db.Close()
		checkAndPanic("db.Close()", err01)
	})()
	fmt.Printf("%v collect path for all pdf files \n", timeNow())
	files := make([]string, 1, 4)
	(func(path string) {
		filepath.Walk(path, func(path string, info os.FileInfo, err error) error {
			if !((err) == (nil)) {
				fmt.Printf("%v Error while walking files err.Error()=%v\n", timeNow(), err.Error())
			}
			files = append(files, info.Name())
			return nil
		})
	})(("/home"))
	fmt.Printf("%v data collection finished len(files)=%v\n", timeNow(), len(files))
	db.Update(func(tx *bolt.Tx) error {
		b, err02 := tx.CreateBucketIfNotExists([]byte("files"))
		if !((err02) == (nil)) {
			return fmt.Errorf("tx.CreateBucketIfNotExists([]byte('files')) %s", err02)
		}
		fmt.Printf("%v create bucket b=%v\n", timeNow(), b)
		return nil
	})
	db.Update(func(tx *bolt.Tx) error {
		b := tx.Bucket([]byte("files"))
		for _, file := range files {
			id, err03 := b.NextSequence()
			checkAndPanic("b.NextSequence()", err03)
			err := b.Put(itob(id), []byte(file))
			if !((err) == (nil)) {
				return fmt.Errorf("b.Put(itob(id), []byte(file)) %s", err)
			}
		}
		return nil
	})
	db.View(func(tx *bolt.Tx) error {
		b := tx.Bucket([]byte("files"))
		c := b.Cursor()
		for k, v := c.First(); (k) != (nil); k, v = c.Next() {
			fmt.Printf("%v  k=%v v=%v\n", timeNow(), k, v)
		}
		return nil
	})
}
