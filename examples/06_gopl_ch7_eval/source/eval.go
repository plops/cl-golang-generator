package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"
	"testing"
	"text/scanner"
)

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
		return ((b.x.Eval(env)) + (b.x.Eval(env)))
	case '-':
		return ((b.x.Eval(env)) - (b.x.Eval(env)))
	case '*':
		return ((b.x.Eval(env)) * (b.x.Eval(env)))
	case '/':
		return ((b.x.Eval(env)) / (b.x.Eval(env)))
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

type TestDefinition struct {
	expr string
	env  Env
	want string
}

func TestEval(intest *testing.T) {
	tests := []TestDefinition{{"sqrt(A / pi)", Env{"A": 87616, "pi": math.Pi}, "167"}, {"pow(x,3)+pow(y,3)", Env{"x": 12, "y": 1}, "1729"}}
	var prevExpr string
	for _, test := range tests {
		if (test.expr) != (prevExpr) {
			fmt.Printf("\n%s\n", test.expr)
			prevExpr = test.expr
		}
		expr, err := Parse(test.expr)
		if (err) != (nil) {
			intest.Error(err)
			continue
		}
		got := fmt.Sprintf("%.6g", expr.Eval(test.env))
		fmt.Printf("\t%v => %s\n", test.env, got)
		if (got) != (test.want) {
			intest.Errorf("%s.Eval() in %s=%q, want %q", test.expr, test.env, got, test.want)
		}
	}
}

type lexer struct {
	scan  scanner.Scanner
	token rune
}

func (lex *lexer) next() {
	lex.token = lex.scan.Scan()
}
func (lex *lexer) text() string {
	return lex.scan.TokenText()
}

type lexPanic string

func precedence(op rune) int {
	switch op {
	case '*', '/':
		return 2
	case '+', '-':
		return 1
	}
	return 0
}
func Parse(input string) (_ Expr, err error) {
	defer (func() {
		switch x := recover().(type); {
		case nil:
		case lexPanic:
			err = fmt.Errorf("%s", x)
		default:
			panic(x)
		}
	})()
	lex := new(lexer)
	lex.scan.Init(strings.NewReader(input))
	lex.scan.Mode = ((scanner.ScanIdents) | (scanner.ScanInts) | (scanner.ScanFloats))
	lex.next()
	e := parseExpr(lex)
	if (lex.token) != (scanner.EOF) {
		return nil, fmt.Errorf("unexpected")
	}
	return e, nil
}
func parseExpr(lex *lexer) Expr {
	return parseBinary(lex, 1)
}
func parseBinary(lex *lexer, prec1 int) Expr {
	lhs := parseUnary(lex)
	for prec := precedence(lex.token); (prec1) <= (prec); (prec)-- {
		for (precedence(lex.token)) == (prec) {
			op := lex.token
			lex.next()
			rhs := parseBinary(lex, ((prec) + (1)))
			lhs = binary{op, lhs, rhs}
		}
	}
	return lhs
}
