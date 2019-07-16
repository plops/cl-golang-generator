package main

//  go get -u fyne.io/fyne
//  go get -u github.com/fyne-io/examples/img/icon
import (
	"fyne.io/fyne"
	"fyne.io/fyne/canvas"
	"fyne.io/fyne/theme"
	"github.com/fyne-io/examples/img/icon"
	"image/color"
	"math"
)

type fractal struct {
	currIterations int
	currScale      float64
	currX          float64
	currY          float64
	window         fyne.Window
	canvas         fyne.CanvasObject
}

func (f *fractal) Layout(objects []fyne.CanvasObject, size fyne.Size) {
	f.canvas.Resize(size)
}
func (f *fractal) MinSize(objects []fyne.CanvasObject) fyne.Size {
	return fyne.NewSize(320, 240)
}
func (f *fractal) refresh() {
	if (1.e+0) <= (f.currScale) {
		f.currIterations = 100
	} else {
		f.currIterations = int(((100) * ((1) + (math.Pow(math.Log10((1.0 / (f.currScale))), (1.25e+0))))))
	}
	f.window.Canvas().Refresh(f.canvas)
}
func (f *fractal) scaleChannel(c float64, start uint32, end uint32) uint8 {
	if (start) <= (end) {
		return ((uint8(start)) + (uint8(((c) * (float64(uint8(((end) - (start)))))))))
	}
	return ((uint8(start)) + (uint8((((1.e+0) - (c)) * (float64(uint8(((end) - (start)))))))))
}
func (f *fractal) scaleColor(c float64, start color.Color, end color.Color) color.Color {
	r1, g1, b1, _ := start.RGBA()
	r2, g2, b2, _ := end.RGBA()
	return color.RGBA{f.scaleChannel(c, r1, r2), f.scaleChannel(c, g1, g2), f.scaleChannel(c, b1, b2), 0xff}
}
func (f *fractal) mandelbrot(px int, py int, w int, h int) color.Color {
	drawScale := ((3.5e+0) * (f.currScale))
	aspect := ((float64(h)) / (float64(w)))
	cRe := ((f.currX) + ((drawScale) * (((float64(px)) / (float64(w))) - (5.e-1))))
	cIm := ((-(f.currY)) + ((drawScale) * (((float64(py)) / (float64(w))) - ((aspect) * (5.e-1)))))
	var i int
	var x float64
	var y float64
	var xsq float64
	var ysq float64
	for i = 0; (i < f.currIterations) && (((xsq) + (ysq)) <= (4)); (i)++ {
		xNew := ((cRe) + (float64(((xsq) - (ysq)))))
		y = ((cIm) + ((2) * (x) * (y)))
		x = xNew
		xsq = ((x) * (x))
		ysq = ((y) * (y))
	}
	if (i) == (f.currIterations) {
		return theme.BackgroundColor()
	}
	mu := ((float64(i)) / (flaot64(f.currIterations)))
	c := math.Sin(((mu) * (math.Pi) * (5.e-1)))
	return f.scaleColor(c, theme.PrimaryColor(), theme.TextColor())
}
func (f *fractal) fractalRune(r rune) {
	if (r) == ('+') {
		f.currScale /= (1.100000023841858e+0)
	}
	if (r) == ('-') {
		f.currScale *= (1.100000023841858e+0)
	}
	f.refresh()
}
func (f *fractal) fractalKey(ev *fyne.KeyEvent) {
	delta := ((2.0000000298023224e-1) * (f.currScale))
	switch ev.Name {
	case fyne.KeyUp:
		(f.currY) -= (delta)
	case fyne.KeyDown:
		(f.currY) += (delta)
	case fyne.KeyLeft:
		(f.currX) += (delta)
	case fyne.KeyRight:
		(f.currX) -= (delta)
	}
	f.refresh()
}
func Show(app fyne.App) {
	window := app.NewWindow("fractal")
	window.SetIcon(icon.FractalBitmap)
	window.SetPadded(false)
	fractal := &fractal{window: window}
	fractal.canvas = canvas.NewRasterWithPixels(fractal.mandelbrot)
	fractal.currIterations = 100
	fractal.currScale = (1.e+0)
	fractal.currX = (-7.499999999999999e-1)
	fractal.currY = (0.0e+0)
	window.SetContent(fyne.NewContainerWithLayout(fractal, fractal.canvas))
	window.Canvas().SetOnTypedRune(fractal.fractalRune)
	window.Canvas().SetOnTypedKey(fractal.fractalKey)
	window.Show()
}
