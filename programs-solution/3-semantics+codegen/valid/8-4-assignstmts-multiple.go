//~1
//~2
//~4
//~5
//~5
//~5
//~6
//~7
//~8
//~7
//~6
//~7

package main

func main() {
	var a,b int = 1, 2
	println(a)
	println(b)

	a, a, b = 3, 4, 5
	println(a)
	println(b)

	a, b = b, a + 1
	println(a)
	println(b)

	a, b, c := 6, 7, 8
	println(a)
	println(b)
	println(c)

	a, b, d := b, a, a + 1
	println(a)
	println(b)
	println(d)
}
