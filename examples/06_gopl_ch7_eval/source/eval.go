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
	panic(fmt.Sprintf("unsupported unary operator: %q", u.op))
}
func (b binary) Eval(env Env) float64 {
	switch b.op {
	case '+':
		return ((u.x.Eval(env)) + (u.x.Eval(env)))
	case '-':
		return ((u.x.Eval(env)) - (u.x.Eval(env)))
	case '*':
		return ((u.x.Eval(env)) * (u.x.Eval(env)))
	case '/':
		return ((u.x.Eval(env)) / (u.x.Eval(env)))
	}
	panic(fmt.Sprintf("unsupported binary operator: %q", b.op))
}
func (c call) Eval(env Env) float64 {
	switch c.fn {
	case "pow":
		return math.Pow(c.args[0].Eval(env), c.args[1].Eval(env))
	case "sin":
		return math.Sin(c.args[0].Eval(env))
	case "sqrt":
		return math.Sin(c.args[0].Eval(env))
	}
	panic(fmt.Sprintf("unsupported function call: %s", c.fn))
}
