package main

import (
	"database/sql"
	"fmt"
	"github.com/gocolly/colly"
	_ "github.com/mattn/go-sqlite3"
	"os"
	"os/signal"
	"strings"
	"time"
)

func timeNow() string {
	return time.Now().Format("2006-01-02 15:04:05.000")
}
func main() {
	fmt.Printf("%v getFuelPrices \n", timeNow())
	fmt.Printf("%v catch signals \n", timeNow())
	sig := make(chan os.Signal, 1)
	signal.Notify(sig, os.Interrupt)
	c := colly.NewCollector(colly.UserAgent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
	c.Limit(&colly.LimitRule{DomainGlob: "www.makro.nl/*", Delay: ((3) * (time.Second)), RandomDelay: ((1) * (time.Second))})
	c.AllowURLRevisit = true
	cityName := "None"
	fn := "fuel.db"
	fmt.Printf("%v open database fn=%v\n", timeNow(), fn)
	db, err00 := sql.Open("sqlite3", fn)
	if !((err00) == (nil)) {
		fmt.Printf("%v sql.Open('sqlite3', fn) err00=%v\n", timeNow(), err00)
		panic(err00)
	}
	defer (func() {
		fmt.Printf("%v close database fn=%v\n", timeNow(), fn)
		err01 := db.Close()
		if !((err01) == (nil)) {
			fmt.Printf("%v db.Close() err01=%v\n", timeNow(), err01)
			panic(err01)
		}
	})()
	_, err02 := db.Exec("CREATE TABLE IF NOT EXISTS fuel ( id INTEGER NOT NULL PRIMARY KEY,time DATETIME NOT NULL,city TEXT,response TEXT,fuel TEXT,price TEXT);")
	if !((err02) == (nil)) {
		fmt.Printf("%v db.Exec('CREATE TABLE IF NOT EXISTS fuel ( id INTEGER NOT NULL PRIMARY KEY,time DATETIME NOT NULL,city TEXT,response TEXT,fuel TEXT,price TEXT);') err02=%v\n", timeNow(), err02)
		panic(err02)
	}
	c.OnHTML("div.price.slide.element-position", func(p0 *colly.HTMLElement) {
		fmt.Printf("%v OnHTML 'div.price.slide.element-position' \n", timeNow())
		name := "None"
		price := "None"
		spl := strings.Split(p0.Text, "€")
		if (3) <= (len(spl)) {
			name = spl[0]
			price = spl[2]
			fmt.Printf("%v  cityName=%v name=%v price=%v\n", timeNow(), cityName, name, price)
		} else {
			spl := strings.Split(p0.Text, "€ / liter")
			if (2) <= (len(spl)) {
				name = spl[0]
				price = spl[1]
				fmt.Printf("%v  cityName=%v name=%v price=%v\n", timeNow(), cityName, name, price)
			} else {
				fmt.Printf("%v cant parse cityName=%v p0.Text=%v\n", timeNow(), cityName, p0.Text)
			}
		}
		res, err03 := db.Exec("INSERT INTO   fuel VALUES (NULL,?,?,?,?,?);", time.Now(), cityName, p0.Text, name, price)
		if !((err03) == (nil)) {
			fmt.Printf("%v db.Exec('INSERT INTO   fuel VALUES (NULL,?,?,?,?,?);', time.Now(), cityName, p0.Text, name, price) err03=%v\n", timeNow(), err03)
			panic(err03)
		}
		id, err04 := res.LastInsertId()
		if !((err04) == (nil)) {
			fmt.Printf("%v res.LastInsertId() err04=%v\n", timeNow(), err04)
			panic(err04)
		}
		fmt.Printf("%v  id=%v\n", timeNow(), id)
	})
	makros_with_gas_station := []string{"amsterdam", "best", "breda", "delft", "duiven", "groningen", "nuth"}
	ticker := time.NewTicker(((1800) * (time.Second)))
	defer (func() {
		fmt.Printf("%v stop ticker \n", timeNow())
		ticker.Stop()
	})()
	done := make(chan bool)
	go (func() {
		<-sig
		fmt.Printf("%v received signal, exit program ... \n", timeNow())
		done <- true
	})()
	for _, name := range makros_with_gas_station {
		cityName = name
		c.Visit((("https://www.makro.nl/vestigingen/") + (name)))
	}
	fmt.Printf("%v wait for ticks every 1800 seconds, you can abort program with C-c \n", timeNow())
	for {
		select {
		case <-done:
			{
				fmt.Printf("%v leave for loop \n", timeNow())
				return
			}
		case tick := <-ticker.C:
			{
				fmt.Printf("%v tick at tick=%v\n", timeNow(), tick)
				for _, name := range makros_with_gas_station {
					cityName = name
					c.Visit((("https://www.makro.nl/vestigingen/") + (name)))
				}
				fmt.Printf("%v wait for next tick \n", timeNow())
			}
		}
	}
	fmt.Printf("%v program will exit now. \n", timeNow())
}
