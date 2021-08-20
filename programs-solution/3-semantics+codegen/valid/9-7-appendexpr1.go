//!
//~Error: out-of-bounds

package main

func f(a []int) {
	a = append(a, 2)
}

func main() {
	var a []int
	a = append(a, 1)

	f(a)
	println(a[1])
}
