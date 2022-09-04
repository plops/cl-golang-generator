package main

import (
	"fmt"
	"github.com/batchatco/go-native-netcdf/netcdf/api"
	"github.com/batchatco/go-native-netcdf/netcdf/cdf"
	"github.com/samber/lo"
	"gorgonia.org/tensor"
	"runtime/debug"
	"time"
)

func timeNow() string {
	return time.Now().Format("2006-01-02 15:04:05.000")
}
func reportDependencies() {
	bi, ok := debug.ReadBuildInfo()
	if ok {
		for _, dep := range bi.Deps {
			fmt.Printf("%v  dep=%v\n", timeNow(), dep)
		}
	} else {
		fmt.Printf("%v failed to read build info \n", timeNow())
	}
}
func reportGenerator() {
	code_git_version := "f4a1c1babae0be5c2244254207ab54b1e4cb79c6"
	code_repository := "https://github.com/plops/cl-golang-generator/tree/master/examples/33_netcdf_tensor"
	code_generation_time := "11:57:43 of Sunday, 2022-09-04 (GMT+1)"
	fmt.Printf("%v  code_git_version=%v\n", timeNow(), code_git_version)
	fmt.Printf("%v  code_repository=%v\n", timeNow(), code_repository)
	fmt.Printf("%v  code_generation_time=%v\n", timeNow(), code_generation_time)
}
func main() {
	fmt.Printf("%v netcdf \n", timeNow())
	reportGenerator()
	reportDependencies()
	// create 2,3,4 3-Tensor of float32, column-major backing
	b := tensor.New(tensor.WithBacking(tensor.Range(tensor.Float32, 0, 24)), tensor.WithShape(2, 3, 4))
	fmt.Printf("%v  b=%v\n", timeNow(), b)
	fn := "newdata.nc"
	cw, err00 := cdf.OpenWriter(fn)
	if !((err00) == (nil)) {
		fmt.Printf("%v cdf.OpenWriter(fn) err00=%v\n", timeNow(), err00)
		panic(err00)
	}
	fmt.Printf("%v define coordinate x \n", timeNow())
	nx := 2
	x := lo.Map[int, uint8](lo.Range(2), func(x int, _ int) uint8 {
		return uint8(x)
	})
	err01 := cw.AddVar("x", api.Variable{x, []string{"x"}, nil})
	if !((err01) == (nil)) {
		fmt.Printf("%v cw.AddVar('x', api.Variable {x, []string {'x'}, nil}) err01=%v\n", timeNow(), err01)
		panic(err01)
	}
	fmt.Printf("%v define coordinate y \n", timeNow())
	ny := 3
	y := lo.Map[int, uint8](lo.Range(3), func(x int, _ int) uint8 {
		return uint8(x)
	})
	err02 := cw.AddVar("y", api.Variable{y, []string{"y"}, nil})
	if !((err02) == (nil)) {
		fmt.Printf("%v cw.AddVar('y', api.Variable {y, []string {'y'}, nil}) err02=%v\n", timeNow(), err02)
		panic(err02)
	}
	fmt.Printf("%v define coordinate z \n", timeNow())
	nz := 4
	z := lo.Map[int, uint8](lo.Range(4), func(x int, _ int) uint8 {
		return uint8(x)
	})
	err03 := cw.AddVar("z", api.Variable{z, []string{"z"}, nil})
	if !((err03) == (nil)) {
		fmt.Printf("%v cw.AddVar('z', api.Variable {z, []string {'z'}, nil}) err03=%v\n", timeNow(), err03)
		panic(err03)
	}
	fmt.Printf("%v compute val \n", timeNow())
	val := make([][][]float32, nx, nx)
	for i := 0; i < nx; (i)++ {
		val[i] = make([][]float32, ny, ny)
		for j := 0; j < ny; (j)++ {
			val[i][j] = make([]float32, nz, nz)
			for k := 0; k < nz; (k)++ {
				x, err04 := b.At(i, j, k)
				if !((err04) == (nil)) {
					fmt.Printf("%v b.At(i, j, k) err04=%v\n", timeNow(), err04)
					panic(err04)
				}
				val[i][j][k] = x.(float32)
			}
		}
	}
	err05 := cw.AddVar("val", api.Variable{val, []string{"x", "y", "z"}, nil})
	if !((err05) == (nil)) {
		fmt.Printf("%v cw.AddVar('val', api.Variable {val, []string {'x', 'y', 'z'}, nil}) err05=%v\n", timeNow(), err05)
		panic(err05)
	}
	defer (func() {
		fmt.Printf("%v close fn=%v\n", timeNow(), fn)
		err06 := cw.Close()
		if !((err06) == (nil)) {
			fmt.Printf("%v cw.Close() err06=%v\n", timeNow(), err06)
			panic(err06)
		}
	})()
}
