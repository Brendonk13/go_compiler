//~2
//~3

package main

func id(a int) int {
	println(2)
	return a
}

func main() {
	var a int
	_, a = id(2), 3

	println(a)
}
