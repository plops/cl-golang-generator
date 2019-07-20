package main

import (
	"github.com/gin-gonic/gin"
	"google.golang.org/grpc"
	"net/http"
	"strconv"
)

func main() {
	conn, err := grpc.Dial("localhost:4040", grpc.WithInsecure())
	if !((err) == (nil)) {
		panic(err)
	}
	client := proto.NewAddServiceClient(conn)
	g := gin.Default()
	g.GET("/add/:a/:b", func(ctx *gin.Context))
	g.GET("/mult/:a/:b", func(ctx *gin.Context))
}
