package main

import (
	"fmt"
	"github.com/hajimehoshi/ebiten/v2"
	"github.com/plops/cl-golang-generator/examples/27_wasm_game/source00/snake"
	"time"
)

func timeNow() string {
	return time.Now().Format("2006-01-02 15:04:05.000")
}
func main() {
	fmt.Printf("%v main \n", timeNow())
	game := snake.NewGame()
}
