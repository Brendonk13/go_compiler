//~1
//~11
//~20

package main

var a int = 0

func foo() int {
	a++
	return a
}

func main() {
	var a [3]int
	a[0] = 1
	a[1] = 10
	a[2] = 20

	a[foo()]++

	println(a[0])
	println(a[1])
	println(a[2])
}
