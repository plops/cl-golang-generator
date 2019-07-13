package main
import (
"fmt"
"io"
"io/ioutil"
"net/http"
"os"
"time"
)
func main() {
    start:=time.Now()
    ch:=make(chan(string))
    for _, url:=range(os.Args[1:]) {
        go fetch(url, ch)
}
    for range(os.Args[1:]) {
        fmt.Println(<-ch)
}
    fmt.Printf("%.2fs elapsed\n", time.Since(start).Seconds())
}
func fetch(url string, ch chan<- string) {
    start:=time.Now()
    resp, err:=http.Get(url)
    if ( !=(err, ) ) {
        ch<-fmt.Sprint(err)
        return
}
}