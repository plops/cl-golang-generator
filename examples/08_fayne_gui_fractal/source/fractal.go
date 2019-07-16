package main

//  go get -u fyne.io/fyne
import (
	"fyne.io/fyne"
	"fyne.io/fyne/canvas"
	"fyne.io/fyne/theme"
	"github.com/fyne.io/examples/img/icon"
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
	return f.NewSize(320, 240)
}
func (f *fractal) refresh() {
	if (1.e+0) <= (f.currScale) {
		f.currIterations = 100
	} else {
		f.currIterations = uint(((100) * ((1) + (math.Pow(math.Log10((1.0 / (currScale))), (1.25e+0))))))
	}
	f.window.Canvas().Refresh(f.canvas)
}
func (f *fractal) scaleChannel(c float64, start uint32, end uint32) uint8 {
	if (start) <= (end) {
		return ((uint8(start)) + (uint8(((c) * (float64(uint8(((end) - (start)))))))))
	}
	return ((uint8(start)) + (uint8((((1.e+0) - (c)) * (float64(uint8(((end) - (start)))))))))
}
func main() {
	a := app.New()
	win := a.NewWindow("Hello World!")
	win.SetContent(widget.NewVBox(widget.NewLabel("Hello World!"), widget.NewButton("Quit", func() {
		a.Quit()
	})))
	win.ShowAndRun()
}
