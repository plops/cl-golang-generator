//go:build linux
// +build linux

package main

import (
	. "bpfexample/cltimelog"
	"fmt"
	"github.com/cilium/ebpf/link"
	"github.com/cilium/ebpf/rlimit"
	"time"
)

//go:generate go run github.com/cilium/ebpf/cmd/bpf2go -cc clang-14 -cflags "-O2 -g -Wall -Werror" bpf bpf/kprobe_percpu.c
const (
	mapKey uint32 = 0
)

func main() {
	fmt.Printf("%v main \n", TimeNow())
	fmt.Printf("%v based on https://github.com/cilium/ebpf/tree/master/examples/kprobe_percpu \n", TimeNow())
	fn := "sys_execve"
	fmt.Printf("%v we will trace the following kernel function fn=%v\n", TimeNow(), fn)
	fmt.Printf("%v allow current process to lock memory for eBPF resources \n", TimeNow())
	err00 := rlimit.RemoveMemlock()
	if !((err00) == (nil)) {
		fmt.Printf("%v rlimit.RemoveMemlock() err00=%v\n", TimeNow(), err00)
		panic(err00)
	}
	fmt.Printf("%v load pre-compiled programs and maps into the kernel \n", TimeNow())
	objs := bpfObjects{}
	err01 := loadBpfObjects(&objs, nil)
	if !((err01) == (nil)) {
		fmt.Printf("%v loadBpfObjects(&objs, nil) err01=%v\n", TimeNow(), err01)
		panic(err01)
	}
	defer objs.Close()
	kp, err02 := link.Kprobe(fn, objs.KprobeExecve, nil)
	if !((err02) == (nil)) {
		fmt.Printf("%v link.Kprobe(fn, objs.KprobeExecve, nil) err02=%v\n", TimeNow(), err02)
		panic(err02)
	}
	defer kp.Close()
	// read loop reports every second number of times the kernel function was entered
	ticker := time.NewTicker(((1) * (time.Second)))
	defer ticker.Stop()
	for range ticker.C {
		var all_cpu_value []uint64
		err03 := objs.KprobeMap.Lookup(mapKey, &all_cpu_value)
		if !((err03) == (nil)) {
			fmt.Printf("%v objs.KprobeMap.Lookup(mapKey, &all_cpu_value) err03=%v\n", TimeNow(), err03)
			panic(err03)
		}
		for cpuid, cpuvalue := range all_cpu_value {
			fmt.Printf("%v calls fn=%v cpuvalue=%v cpuid=%v\n", TimeNow(), fn, cpuvalue, cpuid)
		}
	}
}
