package main

import (
	"11_grpc/source/proto"
	"context"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"
	"net"
)

type server struct {
}

func main() {
	listener, err := net.Listen("tcp", ":4040")
	if !((err) == (nil)) {
		panic(err)
	}
	srv := grpc.NewServer()
	proto.RegisterAddServiceServer(srv, &server{})
	reflection.Register(srv)
	{
		e := srv.Serve(listener)
		if !((e) == (nil)) {
			panic(e)
		}
	}
}
func (s *server) Add(ctx context.Context, request *proto.Request) (*proto.Response, error) {
	a := request.GetA()
	b := request.GetB()
	result := ((a) + (b))
	return &proto.Response{Result: result}, nil
}
func (s *server) Multiply(ctx context.Context, request *proto.Request) (*proto.Response, error) {
	a := request.GetA()
	b := request.GetB()
	result := ((a) * (b))
	return &proto.Response{Result: result}, nil
}
