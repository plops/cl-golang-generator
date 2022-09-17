package main

import (
	"fmt"
	"github.com/gin-gonic/gin"
	swaggerfiles "github.com/swaggo/files"
	ginSwagger "github.com/swaggo/gin-swagger"
	docs "mymain/docs"
	"net/http"
	"runtime"
	"runtime/debug"
	"time"
)

// @title  Music Album service
// @version   1.0
// @description Information about music album
// @license.name Apache 2.0
// @host localhost:8080
// @BasePath /
type Album struct {
	ID     string  `json:"id" binding:"required" example:"1"`
	Title  string  `json:"title" binding:"required" example:"This is a song"`
	Artist string  `json:"artist" binding:"required" example:"Art Ista"`
	Price  float64 `json:"price" binding:"required" example:"12.23"`
}

var albums = []Album{{ID: "1", Title: "blue train", Artist: "john coltrane", Price: 54.99}, {ID: "2", Title: "jeru", Artist: "eryy muliiang", Price: 17.99}, {ID: "3", Title: "vaun and brown", Artist: "vaaughn", Price: 39.99}}

func timeNow() string {
	return time.Now().Format("2006-01-02 15:04:05.000")
}
func checkAndPanic(msg string, err error) {
	if !((err) == (nil)) {
		fmt.Printf("%v  msg=%v err=%v\n", timeNow(), msg, err)
		panic(err)
	}
}

// getAlbums godoc
// @Summary List existing albums
// @Schemes
// @Description Get all the albums
// @Tags albums
// @Accept json
// @Produce json
// @Success 200 {array} Album
// @Router /albums [get]
func getAlbums(c *gin.Context) {
	// gin.Context carries request details, validates and serializes JSON
	// note: Context.JSON would be more compact
	c.IndentedJSON(http.StatusOK, albums)
}
func postAlbums(c *gin.Context) {
	var newAlbum Album
	err00 := c.BindJSON(&newAlbum)
	if !((err00) == (nil)) {
		return
	}
	// add new Album to slice
	albums = append(albums, newAlbum)
	c.IndentedJSON(http.StatusCreated, newAlbum)
}
func getAlbumByID(c *gin.Context) {
	id := c.Param("id")
	// locate Album whose ID matches parameter
	for _, a := range albums {
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
	code_git_version := "a0d9aceea837a6f78af6d3182743bdab1ef132f6"
	code_repository := "https://github.com/plops/cl-golang-generator/tree/master/examples/35_rest"
	code_generation_time := "21:45:37 of Saturday, 2022-09-17 (GMT+1)"
	fmt.Printf("%v  code_git_version=%v\n", timeNow(), code_git_version)
	fmt.Printf("%v  code_repository=%v\n", timeNow(), code_repository)
	fmt.Printf("%v  code_generation_time=%v\n", timeNow(), code_generation_time)
}
func main() {
	fmt.Printf("%v program 00_mymain starts \n", timeNow())
	reportGenerator()
	fmt.Printf("%v Go version: runtime.Version()=%v\n", timeNow(), runtime.Version())
	reportDependencies()
	docs.SwaggerInfo.Title = "Music Albums API"
	docs.SwaggerInfo.Description = "Music Albums API "
	docs.SwaggerInfo.Version = "1.0"
	docs.SwaggerInfo.Host = "localhost:8080"
	docs.SwaggerInfo.BasePath = "/"
	docs.SwaggerInfo.Schemes = []string{"http"}
	router := gin.Default()
	router.GET("/albums", getAlbums)
	router.POST("/albums", postAlbums)
	router.GET("/albums/:id", getAlbumByID)
	router.GET("/swagger/*any", ginSwagger.WrapHandler(swaggerfiles.Handler))
	router.Run("localhost:8080")
}
