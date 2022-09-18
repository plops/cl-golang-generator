#!/bin/bash
set -x
~/go/bin/swag init -g 00_mymain.go
~/go/bin/swag fmt
go build
