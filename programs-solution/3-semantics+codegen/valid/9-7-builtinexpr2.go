// Capacity builtin function
//~0
//~2
//~2
//~4
//~5

package main

func main() {
	var b []int
	println(cap(b))

	b = append(b, 0)
	println(cap(b))

	b = append(b, 1)
	println(cap(b))

	b = append(b, 2)
	b = append(b, 3)
	println(cap(b))

	var c [5]int
	println(cap(c))
}

