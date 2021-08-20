package main

func main() {
	var a string = "string"

    // need to make slice dcls gen correctly
	// var sad [][]int

    // array dcl's are correct, at least in this case
	// var sad [1][5]int
	var b []int
	b = append(b, 0)
	b = append(b, 0)

	var xx [][][]int

    type new_typ []int
    var x new_typ
    x = append(x, 0)

	var c [5]int

	println(len(a))

	println(len(b))
	println(cap(b))

	println(len(c))
	println(cap(c))
}
