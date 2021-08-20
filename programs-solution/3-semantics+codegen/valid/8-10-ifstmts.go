//~true
//~false
//~true
//~false
//~false

package main

func main() {
	var a bool = false

	if a := true; a {
		println(a)
	}
	println(a)

	if a {
		var a int
	} else if a := true; a {
		println(a)
	}
	println(a)

	if a {

	} else {
		println(a)
	}
}
