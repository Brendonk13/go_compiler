// variables defined in the block are not in the same scope out of block
package main

func main() {
	
	{
		var a string = "Not in scope outside this block"
	}

	print(a)
}
