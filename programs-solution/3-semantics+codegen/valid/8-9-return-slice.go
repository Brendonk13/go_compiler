//~2
//~2

package main

var a []int

func foo() []int {
	return a
}

func main() {
	a = append(a, 1)

	var b []int = foo()
	a[0] = 2

	println(a[0])
	println(b[0])
}

