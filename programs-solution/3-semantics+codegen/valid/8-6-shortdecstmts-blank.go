//~2
//~3

package main

func id(a int) int {
	println(a)
	return a
}

func main() {
	_, a := id(2), 3

	println(a)
}
