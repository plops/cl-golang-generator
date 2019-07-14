package main

type Var string
type literal float64
type unary struct {
	op rune
	x  Expr
}
