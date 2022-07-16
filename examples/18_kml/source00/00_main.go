package main

import (
	"encoding/xml"
	"fmt"
	"io/ioutil"
	"os"
	"satplan/schema"
	"time"
)

func main() {
	fmt.Printf("%v main \n", time.Now().Format("2006-01-02 15:04:05.000"))
	var fn = "S1A_MP_USER_20220715T160000_20220804T180000.kml"
	kml, err00 := os.Open(fn)
	if !((err00) == (nil)) {
		panic(err00)
	}
	defer kml.Close()
	kmlbytes, err01 := ioutil.ReadAll(kml)
	if !((err01) == (nil)) {
		panic(err01)
	}

	var doc schema.Document
	xml.Unmarshal(kmlbytes, &doc)
	fmt.Printf("%v  doc=%v\n", time.Now().Format("2006-01-02 15:04:05.000"), doc)
}
