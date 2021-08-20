//~or1
//~or2
//~or2
//~and1
//~and1
//~and2
//~true
//~false

package main

func foo(a string) bool {
	println(a)
	return true
}

func main() {
	var a, b bool

	a = foo("or1") || foo("or1")
	a = !foo("or2") || foo("or2")

	b = foo("and1") && foo("and1")
	b = !foo("and2") && foo("and2")

	println(a)
	println(b)
}
