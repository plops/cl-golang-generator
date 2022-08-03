package main

import (
	"fmt"
	"github.com/gocolly/colly"
	"time"
)

func timeNow() string {
	return time.Now().Format("2006-01-02 15:04:05.000")
}
func main() {
	fmt.Printf("%v getFuelPrices \n", timeNow())
	c := colly.NewCollector(colly.UserAgent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
	c.Limit(&colly.LimitRule{DomainGlob: "www.makro.nl/*", Delay: ((3) * (time.Second)), RandomDelay: ((1) * (time.Second))})
	c.OnRequest(func(p0 *colly.Request) {
		fmt.Printf("%v OnRequest  p0.URL=%v\n", timeNow(), p0.URL)

	})
	c.OnHTML("div.price.slide.element-position", func(p0 *colly.HTMLElement) {
		fmt.Printf("%v OnHTML 'div.price.slide.element-position' p0=%v\n", timeNow(), p0)

	})
	c.OnError(func(p0 *colly.Response, p1 error) {
		fmt.Printf("%v OnError  p0.Request.URL=%v p1=%v\n", timeNow(), p0.Request.URL, p1)

	})
	c.OnResponse(func(p0 *colly.Response) {
		fmt.Printf("%v OnResponse  p0.Request.URL=%v\n", timeNow(), p0.Request.URL)

	})
	c.OnScraped(func(p0 *colly.Response) {
		fmt.Printf("%v OnScraped  p0.Request.URL=%v\n", timeNow(), p0.Request.URL)

	})
	c.Visit("https://www.makro.nl/vestigingen/best")
}
