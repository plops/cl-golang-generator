func main() nil {
    flag.Parse()
    fmt.Print(strings.Join(flag.Args(), *sep))
}