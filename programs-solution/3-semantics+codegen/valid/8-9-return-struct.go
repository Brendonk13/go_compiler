//~2
//~1

package main

type my struct {
	a int
}

var a my

func foo() my {
	a.a = 1
	return a
}

func main() {
	var b my = foo()
	a.a = 2
	println(a.a)
	println(b.a)
}
