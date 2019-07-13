package main

import (
	"image"
	"image/color"
	"image/gif"
	"io"
	"math"
	"math/rand"
	"os"
)

var palette = []color.Color{color.White, color.Black}

const (
	cycles  = 5
	res     = nil
	size    = 100
	nframes = 64
	delay   = 8
)

func main() {
	flag.Parse()
	fmt.Print(strings.Join(flag.Args(), *sep))
	if !(*n) {
		fmt.Println()
	}
}
