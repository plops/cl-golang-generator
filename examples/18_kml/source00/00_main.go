package main

import (
	"encoding/xml"
	"fmt"
	"io/ioutil"
	"os"
	"satplan/schema"
	"time"
)

func timeNow() string {
	return time.Now().Format("2006-01-02 15:04:05.000")
}
func main() {
	fmt.Printf("%v main \n", timeNow())
	var fn = "KML_Samples.kml"
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

	fmt.Printf("%v unmarshall KML with go code based on kml21.xsd \n", timeNow())
	var kmldoc schema.Kml
	err02 := xml.Unmarshal(kmlbytes, &kmldoc)
	if !((err02) == (nil)) {
		panic(err02)
	}

	fmt.Printf("%v  kmldoc=%v\n", timeNow(), kmldoc)
}
