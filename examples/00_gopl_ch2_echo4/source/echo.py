func main() {
    flag.Parse()
    fmt.Print(strings.Join(flag.Args(), *sep))
}