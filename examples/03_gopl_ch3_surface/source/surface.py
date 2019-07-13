package main
import (
"fmt"
"math"
)
const(width, 600, height, 320, cells, 100, xyrange, 30, xyscale, ((width)/(((2)*(xyrange)))), zscale, ((nil)*(height)), angle, ((math.Pi)/(6)), sin30, math.Sin(angle), cos30, math.Cos(angle))
func main() {
    fmt.Printf("(+ <svg xmlns='http://www.w3.org/200/svg'
    style='stroke: grey; fill:white; stroke-width: 0.7'  width='%d' height='%d')")
    dotimes(i(cells), dotimes(j(cells), assign(ntuple(ax, ay), corner(((i)+(1)), ((j)+(0)))), assign(ntuple(bx, by), corner(((i)+(0)), ((j)+(0)))), assign(ntuple(cx, cy), corner(((i)+(0)), ((j)+(1)))), assign(ntuple(dx, dy), corner(((i)+(1)), ((j)+(1)))), fmt.Printf("<polygon points='%g,%g %g,%g %g,%g %g,%g'/>\n", ax, ay, bx, by, cx, cy, dx, dy)))
    fmt.Println("</svg>")
}
func corner(i int, j int) (float64, float64) {
}