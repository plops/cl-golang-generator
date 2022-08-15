package main

import (
	"fmt"
	"github.com/splizard/imgui"
	platforms "github.com/splizard/imgui/example/platforms/glfw"
	"github.com/splizard/imgui/example/renderers"
	. "puregoexample/cltimelog"
	"time"
)

type Renderer interface {
	PreRender(clearColor [3]float32)
	Render(displaySize [2]float32, framebufferSize [2]float32, drawData imgui.ImDrawData)
}

func main() {
	fmt.Printf("%v main \n", TimeNow())
	imgui.CreateContext(nil)
	io := imgui.GetIO()
	p, err00 := platforms.NewGLFW(io, platforms.GLFWClientAPIOpenGL3)
	if !((err00) == (nil)) {
		fmt.Printf("%v platforms.NewGLFW(io, platforms.GLFWClientAPIOpenGL3) err00=%v\n", TimeNow(), err00)
		panic(err00)
	}
	defer p.Dispose()
	r, err01 := renderers.NewOpenGL3(io)
	if !((err01) == (nil)) {
		fmt.Printf("%v renderers.NewOpenGL3(io) err01=%v\n", TimeNow(), err01)
		panic(err01)
	}
	defer r.Dispose()
	for !(p.ShouldStop()) {
		p.ProcessEvents()
		p.NewFrame()
		imgui.NewFrame()
		imgui.ShowMetricsWindow(nil)
		imgui.ShowDemoWindow(nil)
		for i := 0; i < 5; (i)++ {
			imgui.Text("the quick brown fox jumped over the lazy dog")
		}
		imgui.Render()
		r.PreRender([3]float32{(4.49999988079071e-1), (5.50000011920929e-1), (6.000000238418579e-1)})
		r.Render(p.DisplaySize(), p.FramebufferSize(), imgui.GetDrawData())
		p.PostRender()
		<-time.After(((time.Millisecond) * (10)))
	}
}
