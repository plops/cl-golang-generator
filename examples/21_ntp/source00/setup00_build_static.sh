# https://github.com/golang/go/issues/26492
go build -tags 'osusergo netgo' 
readelf -d main
