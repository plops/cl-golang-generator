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
	witeIndex  = 0
	blackIndex = 1
)

func main() {
	lissajous(os.Stdout)
}
func lissajous(out io.Writer) {
	const (
		cycles  = 5
		res     = (1.0000000474974513e-3)
		size    = 100
		nframes = 64
		delay   = 8
	)
	freq := ((3.e+0) * (rand.Float64()))
	anim := gif.GIF{LoopCount: nframes}
	phase := (0.0e+0)
}
func main() {
	flag.Parse()
	fmt.Print(strings.Join(flag.Args(), *sep))
	if !(*n) {
		fmt.Println()
	}
}
