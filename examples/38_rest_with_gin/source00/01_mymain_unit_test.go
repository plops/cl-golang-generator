package main

import (
	"bytes"
	"encoding/json"
	"github.com/gin-gonic/gin"
	"github.com/rs/xid"
	"github.com/stretchr/testify/assert"
	"net/http"
	"net/http/httptest"
	"testing"
)

// run with `go test` or `GIN_MODE=release go test -v`
// a test file must end with _test.go. Each test method must start with prefix Test
func SetUpRouter() *gin.Engine {
	router := gin.Default()
	return router
}
func Test_getAlbums(tt *testing.T) {
	r := SetUpRouter()
	r.GET("/Albums", getAlbums)
	req, _ := http.NewRequest("GET", "/Albums", nil)
	w := httptest.NewRecorder()
	r.ServeHTTP(w, req)
	var albumsOrig = []Album{{ID: "1", Title: "blue train", Artist: "john coltrane", Price: 54.99}, {ID: "2", Title: "jeru", Artist: "eryy muliiang", Price: 17.99}, {ID: "3", Title: "vaun and brown", Artist: "vaaughn", Price: 39.99}}
	var albums []Album
	json.Unmarshal(w.Body.Bytes(), &albums)
	assert.Equal(tt, http.StatusOK, w.Code)
	assert.NotEmpty(tt, albums)
	assert.Equal(tt, albums, albumsOrig)
}
func Test_postAlbums(tt *testing.T) {
	r := SetUpRouter()
	r.POST("/albums", postAlbums)
	albumId := xid.New().String()
	album := Album{ID: albumId, Title: "bla", Artist: "blub", Price: 32.12}
	jsonValue, _ := json.Marshal(album)
	req, _ := http.NewRequest("POST", "/albums", bytes.NewBuffer(jsonValue))
	w := httptest.NewRecorder()
	r.ServeHTTP(w, req)
	assert.Equal(tt, http.StatusCreated, w.Code)
}
