//~5
//~+6.000000e+000
//~66
//~-5
//~-6.000000e+000
//~-66
//~false
//~-6
//~-67

package main

func main() {
	var a int = 5
	var b float64 = 6.0
	var c bool = true
	var d rune = 'B'

	println(+a)
	println(+b)
	println(+d)

	println(-a)
	println(-b)
	println(-d)

	println(!c)

	println(^a)
	println(^d)
}
