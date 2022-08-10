package snake

import (
	"github.com/hajimehoshi/ebiten/v2"
	"math/rand"
	"time"
)

type Board struct {
	rows     int
	cols     int
	food     *Food
	snake    *Snake
	points   int
	gameOver int
	timer    time.Time
}

func NewBoard(rows int, cols int) *Board {
	board := &Board{rows: rows, cols: cols, timer: time.Now()}
	board.snake = NewSnake([]Coord{{0, 0}, {0, 1}, {0, 2}, {0, 3}}, ebiten.KeyArrowRight)
	board.placeFood()
	return board
}
func (b *Board) Update(input *Input) error {
	if b.gameOver {
		return nil
	}
	// faster snake, more points
	interval := ((200) * (time.Millisecond))
	if 10 < b.points {
		interval = ((150) * (time.Millisecond))
	} else {
		if 20 < b.points {
			interval = ((100) * (time.Millisecond))
		}
	}
	newDir, ok := input.Dir()
	if ok {
		b.snake.ChangeDirection(newDir)
	}
	if (interval) <= (time.Since(b.timer)) {
		err01 := b.moveSnake()
		if !((err01) == (nil)) {
			return err01
		}
		b.timer = time.Now()
	}
	return nil
}
func (b *Board) placeFood() {
	var x, y int
	for {
		x = rand.Intn(b.cols)
		y = rand.Intn(b.rows)
		// no food on snake head
		if !(b.snake.HeadHits(x, y)) {
			break
		}
	}
	b.food = NewFood(x, y)
}
func (b *Board) moveSnake() error {
	b.snake.Move()
	if (b.snakeLeftBoard()) || (b.snake.HeadHitsBody()) {
		b.gameOver = true
		return nil
	}
	if b.snake.HeadHits(b.food.x, b.food.y) {
		b.snake.justAte = true
		b.placeFood()
		(b.points)++
	}
	return nil
}
func (b *Board) snakeLeftBoard() bool {
	h := b.snake.Head()
	return ((((b.cols) - (1)) < head.x) || (((b.rows) - (1)) < head.y) || (head.x < 0) || (head.y < 0))
}
