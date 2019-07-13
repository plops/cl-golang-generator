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
	for i := 0; i < nframes; (i)++ {
		rect := image.Rect(0, 0, ((1) + ((2) * (size))), ((1) + ((2) * (size))))
		img := image.NewPaletted(rect, palette)
		for t := (0.0e+0); t < ((cycles) * (2) * (math.Pi)); (t) += (res) {
			x := math.Sin(t)
			y := math.Sin((((t) * (freq)) + (phase)))
			img.SetColorIndex(((size) + (int(((5.e-1) + ((x) * (size)))))), ((size) + (int(((5.e-1) + ((y) * (size)))))), blackIndex)
		}
		(phase) += (1.0000000149011612e-1)
		anim.Delay = append(anim.Delay, delay)
		anim.Image = append(anim.Image, img)
	}
	gif.EncodeAll(out, &anim)
}
