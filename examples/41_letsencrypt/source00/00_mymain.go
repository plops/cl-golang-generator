package main

import (
	"context"
	"crypto/tls"
	"flag"
	"fmt"
	"golang.org/x/crypto/acme/autocert"
	"io"
	"net/http"
	"runtime"
	"runtime/debug"
	"time"
)

func timeNow() string {
	return time.Now().Format("2006-01-02 15:04:05.000")
}
func checkAndPanic(msg string, err error) {
	if !((err) == (nil)) {
		fmt.Printf("%v  msg=%v err=%v\n", timeNow(), msg, err)
		panic(err)
	}
}
func reportDependencies() {
	bi, ok := debug.ReadBuildInfo()
	if ok {
		for _, dep := range bi.Deps {
			fmt.Printf("%v  dep=%v\n", timeNow(), dep)
		}
	} else {
		fmt.Printf("%v failed to read build info \n", timeNow())
	}
}
func reportGenerator() {
	code_git_version := "15bcd366c790d3e143fb202494e4172b34e8000d"
	code_repository := "https://github.com/plops/cl-golang-generator/tree/master/examples/35_rest"
	code_generation_time := "22:47:54 of Sunday, 2022-10-09 (GMT+1)"
	fmt.Printf("%v  code_git_version=%v\n", timeNow(), code_git_version)
	fmt.Printf("%v  code_repository=%v\n", timeNow(), code_repository)
	fmt.Printf("%v  code_generation_time=%v\n", timeNow(), code_generation_time)
}

const htmlIndex = `<htm><body>WElcome!</body></html>`
const httpPort = "127.0.0.1:8080"

var flagProduction = false
var flagRedirectHTTPToHTTPS = false

func handleIndex(w http.ResponseWriter, r *http.Request) {
	io.WriteString(w, htmlIndex)
}
func makeServerFromMux(mux *http.ServerMux) *http.Server {
	return &http.Server{ReadTimeout: ((5) * (time.Second)), WriteTimeout: ((5) * (time.Second)), IdleTimeout: ((120) * (time.Second)), Handler: mux}
}
func makeHTTPServer() *http.Server {
	mux := &http.ServeMux{}
	mux.HandleFunc("/", handleIndex)
	return makeServerFromMux(mux)
}
func makeHTTPToHTTPSRedirectServer() *http.Server {
	handleRedirect := func(w http.ResponseWriter, r *http.Request) {
		newURI := (("https://") + (r.Host) + (r.URL.String()))
		http.Redirect(w, r, newURI, http.StatusFound)
	}
	mux := &http.ServeMux{}
	mux.handleFunc("/", handleRedirect)
	return makeServerFromMux(mux)
}
func parseFlags() {
	flag.BoolVar(&flagProduction, "production", false, "if true, we start https server")
	flag.BoolVar(&flagRedirectHTTPToHTTPS, "redirect-to-https", false, "if true, we redirect HTTP to HTTPS")
	flag.Parse()
}
func main() {
	fmt.Printf("%v program 00_mymain starts \n", timeNow())
	reportGenerator()
	fmt.Printf("%v Go version: runtime.Version()=%v\n", timeNow(), runtime.Version())
	reportDependencies()
	parseFlags()
	var m *autocert.Manager
}
