package main

import (
	"context"
	"source/proto"
)

type server struct {
}

func main() {
}
func (s *server) Add(ctx context.Context, request *proto.Request) (*proto.Response, error) {
	a := request.GetA()
	b := request.GetB()
	result := ((a) + (b))
	return &proto.Response{Result: result}, nil
}
