package main

import (
	"bytes"
	"fmt"
	"github.com/amundsentech/kml-decode"
	"io/ioutil"
	"os"
	"time"
)

func timeNow() string {
	return time.Now().Format("2006-01-02 15:04:05.000")
}
func main() {
	fmt.Printf("%v main \n", timeNow())
	var fn = "../source00/KML_Samples.kml"
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

	kmlbuf := bytes.NewBuffer(kmlbytes)
	var kmlstruct kmldecode.KML
	kmldecode.KMLDecode(kmlbuf, &kmlstruct)
	fmt.Printf("%v  kmlstruct=%v\n", timeNow(), kmlstruct)
}
