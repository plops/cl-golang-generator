package main

import (
	"google.golang.org/grpc"
)

func main() {
	conn, err := grpc.Dial("localhost:4040", grpc.WithInsecure())
	if !((err) == (nil)) {
		panic(err)
	}
	client := proto.NewAddServiceClient(conn)
	g := gin.Default()
}
