//~3
//~3
//~3
//~1
//~3

package main

var x int = 3

func main() {
	println(x)
	var x int = x
	println(x)

	{
		println(x)
		var x int = 1
		println(x)
	}

	println(x)
}
