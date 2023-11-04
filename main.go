package main

import "fmt"

type Parsed[T any] struct {
	result T
	rest   string
}

type ParseError struct{}

func (n ParseError) Error() string {
	return "Cannot parse"
}

type Parser[T any] func(string) (Parsed[T], error)

func Char(chr byte) Parser[byte] {
	return func(input string) (Parsed[byte], error) {
		if input == "" {
			return Parsed[byte]{}, ParseError{}
		}

		switch input[0] {
		case chr:
			return Parsed[byte]{chr, input[1:]}, nil
		default:
			return Parsed[byte]{}, ParseError{}
		}
	}
}

func String(str string) Parser[string] {
	return func(input string) (Parsed[string], error) {
		return Parsed[string]{}, ParseError{}
	}
}

func main() {
	parser := Char('h')
	fmt.Println(parser("hello"))
	fmt.Println(parser("bello"))
}
