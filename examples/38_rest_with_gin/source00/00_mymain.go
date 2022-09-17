package main

import (
	"fmt"
	"github.com/gin-gonic/gin"
	"net/http"
	"runtime"
	"runtime/debug"
	"time"
)

type Album struct {
	ID     string  `json:"id"`
	Title  string  `json:"title"`
	Artist string  `json:"artist"`
	Price  float64 `json:"price"`
}

var Albums = []Album{{ID: "1", Title: "blue train", Artist: "john coltrane", Price: 54.99}, {ID: "2", Title: "jeru", Artist: "eryy muliiang", Price: 17.99}, {ID: "3", Title: "vaun and brown", Artist: "vaaughn", Price: 39.99}}

func timeNow() string {
	return time.Now().Format("2006-01-02 15:04:05.000")
}
func checkAndPanic(msg string, err error) {
	if !((err) == (nil)) {
		fmt.Printf("%v  msg=%v err=%v\n", timeNow(), msg, err)
		panic(err)
	}
}
func getAlbums(c *gin.Context) {
	// gin.Context carries request details, validates and serializes JSON
	// note: Context.JSON would be more compact
	c.IndentedJSON(http.StatusOK, Albums)
}
func postAlbums(c *gin.Context) {
	var newAlbum Album
	err00 := c.BindJSON(&newAlbum)
	if !((err00) == (nil)) {
		return
	}
	// add new Album to slice
	Albums = append(Albums, newAlbum)
	c.IndentedJSON(http.StatusCreated, newAlbum)
}
func getAlbumByID(c *gin.Context) {
	id := c.Param("id")
	// locate Album whose ID matches parameter
	for _, a := range Albums {
		if (a.ID) == (id) {
			c.IndentedJSON(http.StatusOK, a)
			return
		}
	}
	c.IndentedJSON(http.StatusNotFound, gin.H{"message": "Album not found"})
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
	code_git_version := "733a0e411fd34e095f57ee93a1710efd4b3b5c78"
	code_repository := "https://github.com/plops/cl-golang-generator/tree/master/examples/35_rest"
	code_generation_time := "13:35:31 of Saturday, 2022-09-17 (GMT+1)"
	fmt.Printf("%v  code_git_version=%v\n", timeNow(), code_git_version)
	fmt.Printf("%v  code_repository=%v\n", timeNow(), code_repository)
	fmt.Printf("%v  code_generation_time=%v\n", timeNow(), code_generation_time)
}
func main() {
	fmt.Printf("%v program 00_mymain starts \n", timeNow())
	reportGenerator()
	fmt.Printf("%v Go version: runtime.Version()=%v\n", timeNow(), runtime.Version())
	reportDependencies()
	router := gin.Default()
	router.GET("/albums", getAlbums)
	router.POST("/albums", postAlbums)
	router.GET("/Albums/:id", getAlbumByID)
	router.Run("localhost:8080")
}
