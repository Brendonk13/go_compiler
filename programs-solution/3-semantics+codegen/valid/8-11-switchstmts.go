//~1

package main

func main() {
	var x int
	switch x := 1; x {
		case 0:
			var x int = 0
			println(x)
		case 1:
			var x int = 1
			println(x)
		default:
			var x int = 2
			println(x)
	}
}
