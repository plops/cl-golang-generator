#!/bin/bash
set -x
#~/go/bin/swag init --parseDependency -g 00_mymain.go
#~/go/bin/swag init --parseDependency --parseInternal -g 00_mymain.go --dir "./,/home/martin/go/pkg/mod/github.com/gin-gonic/gin@v1.8.1/"
~/go/bin/swag init --parseDependency --parseInternal --parseDepth 1 -g 00_mymain.go --dir "./,/home/martin/go/pkg/mod/github.com/gin-gonic/gin@v1.8.1/" 
#~/go/bin/swag init  -g 00_mymain.go
~/go/bin/swag fmt
go build
 
