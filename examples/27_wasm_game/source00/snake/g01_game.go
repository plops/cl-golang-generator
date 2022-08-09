package snake

import (
	"fmt"
	"github.com/hajimehoshi/ebiten/v2"
	"github.com/hajimehoshi/ebiten/v2/ebitenutil"
	"image/color"
	"time"
)

const (
	ScreenWidth  = 600
	ScreenHeight = 600
	boardRows    = 20
	boardCols    = 20
)

var backgroundColor = color.RGBA{50, 100, 50, 50}

type Game struct {
}

func NewGame() *Game {
	return &Game{}
}
func (g *Game) Layout(outsideWidth int, outsideHeight int) {
	return ScreenWidth, ScreenHeight
}
func (g *Game) Update() error {
	return nil
}
func (g *Game) Draw(screen *ebiten.Image) {
	return screen.Fill(backgroundColor)
}
