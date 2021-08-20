//~2
//~1

package main

var a [5]int

func foo() [5]int {
	a[0] = 1
	return a
}

func main() {
	var b [5]int = foo()
	a[0] = 2

	println(a[0])
	println(b[0])
}
