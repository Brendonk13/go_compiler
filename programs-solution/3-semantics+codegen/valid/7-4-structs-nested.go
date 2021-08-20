//~Success
package main

func main() {
	type A struct {
		a int
	}
	type B struct {
		a A
	}
	println("Success")
}
