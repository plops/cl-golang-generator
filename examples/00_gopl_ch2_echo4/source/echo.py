package main
import (
"flag"
"fmt"
"string"
)
func main() {
    flag.Parse()
    fmt.Print(strings.Join(flag.Args(), *sep))
    if ( !(*n) ) {
        fmt.Println()
}
}