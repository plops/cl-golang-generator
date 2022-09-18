#!/bin/bash
set -x
~/go/bin/swag init --parseDependency -g 00_mymain.go
~/go/bin/swag fmt
go build
