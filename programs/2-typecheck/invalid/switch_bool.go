// case expressions in swtich without expression should resolve to bool.
package main

func main() {
	switch {
	case 1:
		print("not bool")
	default:
		print("asdasdas")
	}
}

