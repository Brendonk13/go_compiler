//~Number of moves: 31

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
	towers(5, 'A', 'C', 'B')
	println("Number of moves:", count)
}
