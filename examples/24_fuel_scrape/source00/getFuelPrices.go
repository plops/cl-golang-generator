package main

import (
	"fmt"
	"github.com/gocolly/colly"
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
	c.OnHTML("div.price.slide.element-position", func(p0 *colly.HTMLElement) {
		fmt.Printf("%v OnHTML 'div.price.slide.element-position' \n", timeNow())
		spl := strings.Split(p0.Text, "€")
		name := spl[0]
		price := spl[2]
		fmt.Printf("%v  name=%v price=%v\n", timeNow(), name, price)
	})
	c.Visit("https://www.makro.nl/vestigingen/amsterdam")
	c.Visit("https://www.makro.nl/vestigingen/best")
	c.Visit("https://www.makro.nl/vestigingen/breda")
	c.Visit("https://www.makro.nl/vestigingen/delft")
	c.Visit("https://www.makro.nl/vestigingen/duiven")
	c.Visit("https://www.makro.nl/vestigingen/groningen")
	c.Visit("https://www.makro.nl/vestigingen/nuth")
}
