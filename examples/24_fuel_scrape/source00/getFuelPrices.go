package main

import (
	"database/sql"
	"fmt"
	"github.com/gocolly/colly"
	_ "github.com/mattn/go-sqlite3"
	"strings"
	"time"
)

func timeNow() string {
	return time.Now().Format("2006-01-02 15:04:05.000")
}
func main() {
	fmt.Printf("%v getFuelPrices \n", timeNow())
	c := colly.NewCollector(colly.UserAgent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
	c.Limit(&colly.LimitRule{DomainGlob: "www.makro.nl/*", Delay: ((3) * (time.Second)), RandomDelay: ((1) * (time.Second))})
	cityName := "None"
	c.OnHTML("div.price.slide.element-position", func(p0 *colly.HTMLElement) {
		fmt.Printf("%v OnHTML 'div.price.slide.element-position' \n", timeNow())
		spl := strings.Split(p0.Text, "€")
		if (3) <= (len(spl)) {
			name := spl[0]
			price := spl[2]
			fmt.Printf("%v  cityName=%v name=%v price=%v\n", timeNow(), cityName, name, price)
		} else {
			spl := strings.Split(p0.Text, "€ / liter")
			if (2) <= (len(spl)) {
				name := spl[0]
				price := spl[1]
				fmt.Printf("%v  cityName=%v name=%v price=%v\n", timeNow(), cityName, name, price)
			} else {
				fmt.Printf("%v cant parse cityName=%v p0.Text=%v\n", timeNow(), cityName, p0.Text)
			}
		}
	})
	db, err00 := sql.Open("sqlite3", "fuel.db")
	if !((err00) == (nil)) {
		fmt.Printf("%v sql.Open('sqlite3', 'fuel.db') err00=%v\n", timeNow(), err00)
		panic(err00)
	}
	defer db.Close()
	_, err01 := db.Exec("CREATE TABLE IF NOT EXISTS fuel ( id INTEGER NOT NULL PRIMARY KEY, time DATETIME NOT NULL, description TEXT );")
	if !((err01) == (nil)) {
		fmt.Printf("%v db.Exec('CREATE TABLE IF NOT EXISTS fuel ( id INTEGER NOT NULL PRIMARY KEY, time DATETIME NOT NULL, description TEXT );') err01=%v\n", timeNow(), err01)
		panic(err01)
	}
	makros_with_gas_station := []string{"amsterdam", "best", "breda", "delft", "duiven", "groningen", "nuth"}
	for _, name := range makros_with_gas_station {
		cityName = name
		c.Visit((("https://www.makro.nl/vestigingen/") + (name)))
	}
}
