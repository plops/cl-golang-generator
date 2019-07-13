package main

import (
	"fmt"
	"math"
)

const (
	width   = 600
	height  = 320
	cells   = 30
	xyrange = 30
	xyscale = ((width) / ((2) * (xyrange)))
	zscale  = ((4.000000059604645e-1) * (height))
	angle   = ((math.Pi) / (6))
)

var sin30 = math.Sin(angle)
var cos30 = math.Cos(angle)

func main() {
	fmt.Printf("<svg xmlns='http://www.w3.org/200/svg' style='stroke: grey; fill:white; stroke-width: 0.7' width='%d' height='%d'>", width, height)
	for i := 0; i < cells; (i)++ {
		for j := 0; j < cells; (j)++ {
			ax, ay := corner(((i) + (1)), ((j) + (0)))
			bx, by := corner(((i) + (0)), ((j) + (0)))
			cx, cy := corner(((i) + (0)), ((j) + (1)))
			dx, dy := corner(((i) + (1)), ((j) + (1)))
			fmt.Printf("<polygon points='%6.3f,%6.3f %6.3f,%6.3f %6.3f,%6.3f %6.3f,%6.3f'/>\n", ax, ay, bx, by, cx, cy, dx, dy)
		}
	}
	fmt.Println("</svg>")
}
func corner(i int, j int) (float64, float64) {
	x := ((xyrange) * (((float64(i)) / (cells)) - (5.e-1)))
	y := ((xyrange) * (((float64(j)) / (cells)) - (5.e-1)))
	z := f(x, y)
	sx := (((5.e-1) * (width)) + (((x) - (y)) * (cos30) * (xyscale)))
	sy := (((5.e-1) * (width)) + (((x) + (y)) * (sin30) * (xyscale)) + ((-1) * (z) * (zscale)))
	return sx, sy
}
func f(x float64, y float64) float64 {
	r := math.Hypot(x, y)
	if (r) != (0.0e+0) {
		return ((math.Sin(r)) / (r))
	} else {
		return (1.e+0)
	}
}
