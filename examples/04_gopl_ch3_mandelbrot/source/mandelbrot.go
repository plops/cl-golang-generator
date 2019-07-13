package main

import (
	"image"
	"image/color"
	"image/png"
	"math/cmplx"
	"os"
)

func main() {
	const (
		xmin   = -2
		ymin   = -2
		xmax   = 2
		ymax   = 2
		width  = 1024
		height = 1024
	)
	img := image.NewRGBA(image.Rect(0, 0, width, height))
	for py := 0; py < height; (py)++ {
		y := ((ymin) + ((((ymax) - (ymin)) * (float64(py))) / (height)))
		for px := 0; px < width; (px)++ {
			x := ((xmin) + ((((xmax) - (xmin)) * (float64(px))) / (width)))
			z := complex(x, y)
			img.Set(px, py, mandelbrot(z))
		}
	}
	png.Encode(os.Stdout, img)
}
func mandelbrot(z complex128) color.Color {
	const (
		iterations = 200
		contrast   = 15
	)
	var v complex128 = 0
	for n := uint8(0); n < iterations; (n)++ {
		v = ((z) + ((v) * (v)))
		if 2 < cmplx.Abs(v) {
			return color.Gray{((255) - ((contrast) * (n)))}
		}
	}
	return color.Black
}
