//~3
//~-1
//~2
//~0
//~+3.000000e+000
//~-1.000000e+000
//~+2.000000e+000
//~+5.000000e-001
//~
//~6
//~1
//~1
//~3
//~1
//~5

package main

func main() {
	var a1 int = 1
	var a2 int = 2
	var b1 float64 = 1.0
	var b2 float64 = 2.0

	println(a1 + a2);
	println(a1 - a2);
	println(a1 * a2);
	println(a1 / a2);

	println(b1 + b2);
	println(b1 - b2);
	println(b1 * b2);
	println(b1 / b2);

	// More complex operations
	var c1 int = 3
	var c2 int = 6
	println()

	println(c1 << 1)
	println(c1 >> 1)

	println(c1 % a2)
	println(a1 | a2)
	println(a1 &^ a2)

	println(c2 ^ c1)
}
