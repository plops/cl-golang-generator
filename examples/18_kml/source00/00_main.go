package main

import (
	"bytes"
	"encoding/xml"
	"fmt"
	"golang.org/x/net/html/charset"
	"io/ioutil"
	"os"
	"satplan/schema"
	"time"
)

func timeNow() {
	time.Now().Format("2006-01-02 15:04:05.000")
}
func main() {
	fmt.Printf("%v main \n", timeNow())
	var fn = "S1A_MP_USER_20220715T160000_20220804T180000.kml"
	fmt.Printf("%v open KML file fn=%v\n", timeNow(), fn)
	kml, err00 := os.Open(fn)
	if !((err00) == (nil)) {
		panic(err00)
	}
	defer kml.Close()
	fmt.Printf("%v read KML file as bytes \n", timeNow())
	kmlbytes, err01 := ioutil.ReadAll(kml)
	if !((err01) == (nil)) {
		panic(err01)
	}

	reader := bytes.NewReader(kmlbytes)
	decoder := xml.NewDecoder(reader)
	decoder.CharsetReader = charset.NewReaderLabel
	fmt.Printf("%v unmarshall KML with go code based on kml21.xsd \n", timeNow())
	var kmldoc schema.Kml
	err02 := decoder.Decode(&kmldoc)
	if !((err02) == (nil)) {
		panic(err02)
	}

	fmt.Printf("%v  kmldoc=%v\n", timeNow(), kmldoc)
}
