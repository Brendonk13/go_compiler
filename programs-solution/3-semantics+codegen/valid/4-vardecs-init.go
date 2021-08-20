//~0
//~+0.000000e+000
//~
//~0
//~false
//~0
//~0
//~0

package main

func main() {
	var a int
	var b float64
	var c string
	var d rune
	var e bool
	var x [3]int
	var y  struct {
		a int
		b struct {
			c [5]int
		}
	}
	println(a)
	println(b)
	println(c)
	println(d)
	println(e)
	println(x[0])
	println(y.a)
	println(y.b.c[0])
}
