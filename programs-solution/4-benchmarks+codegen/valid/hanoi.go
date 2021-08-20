//~Number of moves: 268435455
//&Python: 57.5 seconds
//&C++: 3.4 seconds
//&Opt C++: 1.6 seconds

package main

var count int = 0

func towers(num int, from rune, to rune, aux rune) {
	if num == 1 {
		count++
		return
	}
	towers(num-1, from, aux, to)
	count++
	towers(num-1, aux, to, from)
}

func main() {
	towers(28, 'A', 'C', 'B')
	println("Number of moves:", count)
}
