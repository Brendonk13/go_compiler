//~1

package main

func main() {
	var a [][]int
	var b []int

	a = append(a, b)
	a[0] = append(a[0], 1)

	println(a[0][0])
}
