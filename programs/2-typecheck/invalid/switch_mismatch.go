// case expression types must match type of switch expression
package main

func main() {
	var a = 1
	switch a {
	case 1: 
		print("1")
	case "2":
		print("2")
	default:
		print("error")
	}
}

