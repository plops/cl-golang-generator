package main

import (
	"fmt"
	"sort"
)

var prereqs = map[string][]string{"algorithm": {"data-structures"}, "calculus": {"linear-algebra"}, "compilers": {"data-structures", "formal-languages", "computer-organization"}, "data-structures": {"discrete-math"}, "databases": {"data-structures"}, "discrete-math": {"intro-to-programming"}, "formal-languages": {"discrete-math"}, "networks": {"operating-systems"}, "operating-systems": {"data-structures", "computer-organization"}, "programming-languages": {"data-structures", "computer-organization"}}

func main() {
	for i, course := range topoSort(prereqs) {
		fmt.Printf("%d:\t%s\n", ((i) + (1)), course)
	}
}
