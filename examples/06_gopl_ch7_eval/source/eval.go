package main

type Var string
type literal float64
type unary struct {
	op rune
	x  Expr
}
type binary struct {
	op rune
	x  Expr
	y  Expr
}
type call struct {
	fn   string
	args []Expr
}
type Env map[Var]float64
