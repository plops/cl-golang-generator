package main

import (
	"gioui.org/app"
	"gioui.org/font/gofont"
	"gioui.org/io/system"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/text"
	"gioui.org/widget/material"
	"image/color"
	"log"
	"os"
)

func main() {
	go (func() {
		w := app.NewWindow()
		err := run(w)
		if !((err) == (nil)) {
			log.Fatal(err)
		}
		os.Exit(0)
	})()
	app.Main()
}
func run(w *app.Window) error {
	// applications define fonts and color settings in themes
	th := material.NewTheme(gofont.Collection())
	var ops op.Ops
	for {
		e := <-w.Events()
		switch e := e.(type) {
		case system.DestroyEvent:
			// user pressed the close button
			return e.Err
		case system.FrameEvent:
			// program should handle input and render a new frame
			gtx := layout.NewContext(&ops, e)
			title := material.H1(th, "hello gio")
			maroon := color.NRGBA{R: 127, G: 0, B: 0, A: 255}
			title.Color = maroon
			title.Alignment = text.Middle
			title.Layout(gtx)
			e.Frame(gtx.Ops)
		}
	}
}
