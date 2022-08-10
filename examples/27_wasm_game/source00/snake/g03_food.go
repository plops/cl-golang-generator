package snake

type Food struct {
	x int
	y int
}

func NewFood(x int, y int) *Food {
	return &Food{x: x, y: y}
}
