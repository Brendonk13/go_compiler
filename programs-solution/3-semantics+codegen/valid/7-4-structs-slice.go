//~1
//~1
//~2
//~2

package main

func main() {
	var a struct {
		a []int
	}

	a.a = append(a.a, 1)

	var b = a

	println(a.a[0])
	println(b.a[0])

	a.a[0] = 2

	println(a.a[0])
	println(b.a[0])
}
