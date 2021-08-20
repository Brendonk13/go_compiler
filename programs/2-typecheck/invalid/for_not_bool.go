// the expression in the for statement has to resolve to type bool
package main

func main() {
		
	for i := 0; "i < 0"; i++ { // the middle expression needs to resolve to bool
		print("bleh")
	}
}

