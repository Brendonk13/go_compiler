//~true
//~b
//~default
//~1

package main

var glob int = 0

func inc() int {
	glob++
	return glob
}

func main() {
	var a, b bool = true, true
	println(a);

	switch a := false; {
	case a:
		println("a");
	case b:
		println("b");
	default:
		println("default");
	}

	switch a {
	default:
		println("default")
	}

	switch inc() {
	case 0:
		println("0")
	case 1:
		println("1")
	}
}
