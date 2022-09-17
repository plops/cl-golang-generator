package mymain

import (
	"encoding/json"
	"github.com/gin-gonic/gin"
	"github.com/stretchr/testify/assert"
	"net/http"
	"net/http/httptest"
	"testing"
)

// a test file must end with _test.go. Each test method must start with prefix Test
func SetUpRouter() *gin.Engine {
	router := gin.Default()
	return router
}
func Test_getAlbums(tt *testing.T) {
	r := SetUpRouter()
	r.GET("/albums", getAlbums)
	req, _ := http.NewRequest("GET", "/albums", nil)
	w := httptest.NewRecorder()
	r.ServeHTTP(w, req)
	var albumsOrig = []album{{ID: "1", Title: "blue train", Artist: "john coltrane", Price: 54.99}, {ID: "2", Title: "jeru", Artist: "eryy muliiang", Price: 17.99}, {ID: "3", Title: "vaun and brown", Artist: "vaaughn", Price: 39.99}}
	var albums []album
	json.Unmarshal(w.Body.Bytes(), &albums)
	assert.Equal(tt, http.StatusOK, w.Code)
	assert.NotEmpty(tt, albums)
	assert.Equal(tt, albums, albumsOrig)
}
