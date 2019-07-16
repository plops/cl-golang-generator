package main

//  go get -u fyne.io/fyne
// the initial `go build hello.go` takes a while
import (
	"fyne.io/fyne/app"
	"fyne.io/fyne/widget"
)

func main() {
	a := app.New()
	win := a.NewWindow("Hello World!")
	win.SetContent(widget.NewVBox(widget.NewLabel("Hello World!"), widget.NewButton("Quit", func() {
		a.Quit()
	})))
	win.ShowAndRun()
}
