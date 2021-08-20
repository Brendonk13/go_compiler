//~1
//~0

package main

func main() {
	var a [3]int
	var b [][3]int

	b = append(b, a)

	a[0] = 1

	println(a[0])
	println(b[0][0])
}
