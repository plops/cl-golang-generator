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
type Expr interface {
	Eval(env Env) float64
}

func (v Var) Eval(env Env) float64 {
	return env[v]
}
func (l literal) Eval(_ Env) float64 {
	return float64(l)
}
func (u unary) Eval(env Env) float64 {
	switch u.op {
	case '+':
		return u.x.Eval(env)
	case '-':
		return (-(u.x.Eval(env)))
	}
}
