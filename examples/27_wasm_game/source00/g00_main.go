package main

import (
	"fmt"
	"github.com/hajimehoshi/ebiten/v2"
	. "wasmgame/cltimelog"
	snake "wasmgame/snake"
)

func main() {
	fmt.Printf("%v main \n", TimeNow())
	game := snake.NewGame()
	ebiten.SetWindowSize(snake.ScreenWidth, snake.ScreenHeight)
	ebiten.SetWindowTitle("snake")
	err00 := ebiten.RunGame(game)
	if !((err00) == (nil)) {
		fmt.Printf("%v ebiten.RunGame(game) err00=%v\n", TimeNow(), err00)
		panic(err00)
	}
}
