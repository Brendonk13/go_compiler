//!
//~Error: out-of-bounds

package main

func main(){
	var a, b []int
	b = append(a, 1)

	println(b[1])
}
