package snake

import (
	"fmt"
	"github.com/hajimehoshi/ebiten/v2"
	"github.com/hajimehoshi/ebiten/v2/ebitenutil"
	"image/color"
	. "wasmgame/cltimelog"
)

const (
	ScreenWidth  = 600
	ScreenHeight = 600
	boardRows    = 20
	boardCols    = 20
)

var backgroundColor = color.RGBA{50, 100, 50, 50}
var snakeColor = color.RGBA{200, 50, 150, 150}
var foodColor = color.RGBA{200, 200, 50, 150}

type Game struct {
	input *Input
	board *Board
}

func NewGame() *Game {
	fmt.Printf("%v NewGame \n", TimeNow())
	return &Game{input: NewInput(), board: NewBoard(boardRows, boardCols)}
}
func (g *Game) Layout(outsideWidth int, outsideHeight int) (int, int) {
	return ScreenWidth, ScreenHeight
}
func (g *Game) Update() error {
	return g.board.Update(g.input)
}
func (g *Game) Draw(screen *ebiten.Image) {
	screen.Fill(backgroundColor)
	if g.board.gameOver {
		ebitenutil.DebugPrint(screen, fmt.Sprintf("Game Over. Score: %d", g.board.points))
	} else {
		width := ((ScreenHeight) / (boardRows))
		for _, p := range g.board.snake.body {
			ebitenutil.DrawRect(screen, float64(((p.y) * (width))), float64(((p.x) * (width))), float64(width), float64(width), snakeColor)
		}
		if (g.board.food) != (nil) {
			ebitenutil.DrawRect(screen, float64(((g.board.food.y) * (width))), float64(((g.board.food.x) * (width))), float64(width), float64(width), foodColor)
		}
		ebitenutil.DebugPrint(screen, fmt.Sprintf("Score: %d", g.board.points))
	}
}
