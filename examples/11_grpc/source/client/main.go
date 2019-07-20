package main
import (
"google.golang.org/grpc"
"github.com/gin-gonic/gin"
"strconv"
"net/http"
)
func main() {
    conn, err:=grpc.Dial("localhost:4040", grpc.WithInsecure())
    if ( !((err)==(nil)) ) {
        panic(err)
}
    client:=proto.NewAddServiceClient(conn)
    g:=gin.Default()
    g.GET("/add/:a/:b", func func(ctx *gin.Context) {
        a, err:=strconv.ParseUint(ctx.Param("a"), 10, 64)
        if ( !((err)==(nil)) ) {
            ctx.JSON(http.StatusBadRequest, gin.H {"error": "invalid parameter a"})
            return
}
        b, err:=strconv.ParseUint(ctx.Param("b"), 10, 64)
        if ( !((err)==(nil)) ) {
            ctx.JSON(http.StatusBadRequest, gin.H {"error": "invalid parameter b"})
            return
}
})
    g.GET("/mult/:a/:b", func(ctx *gin.Context) )
}