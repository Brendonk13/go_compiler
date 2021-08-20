//~[21, 26]
//~[19, 25]
//~[17, 24]
//~[20, 23]
//~[13, 22]
//~[11, 21]
//~[7, 20]
//~[15, 19]
//~[6, 18]
//~[9, 17]
//~[5, 16]
//~[25, 15]
//~[23, 14]
//~[26, 13]
//~[24, 12]
//~[22, 11]
//~[18, 10]
//~[16, 9]
//~[14, 8]
//~[12, 7]
//~[10, 6]
//~[8, 5]
//~[3, 4]
//~[1, 3]
//~[4, 2]
//~[2, 1]
//~[0, 0]
//&Python: 34.1 seconds
//&C++: 1.7 seconds
//&Opt C++: 0.9 seconds

package main

// This is the classic queens problem. Set the chess grid size (in main class) to a number and the program will
// place that many queens on the chessboard in a way that they wont be attacking eachother.

type point struct {
	x, y int
}

var chessGridSize int = 27
var queens []point

//returns 1 if everything is good
func queenRecursion(index int) int {
	if index >= chessGridSize {
		return 1
	}

	var i int
	var j int
	var p point
	var allgood int
	var p2 point
	var solutionFound int = 0

	p = queens[index]

	for i = 0; i < chessGridSize; i++ {
		p.x = i

		allgood = 1
		for j = 0; j < index; j++ {
			p2 = queens[j]

			if p.x == p2.x || p2.y-p.y == p2.x-p.x || p2.y-p.y == p.x-p2.x {
				allgood = 0
				break
			}
		}

		if allgood == 1 {
			queens[index] = p
			if queenRecursion(index+1) == 1 {
				print("[")
				print(p.x)
				print(", ")
				print(p.y)
				println("]")
				solutionFound = 1
				break
			}
		}
	}

	return solutionFound
}

func main() {

	var i int
	for i = 0; i < chessGridSize; i++ {
		var p point
		p.x = 0
		p.y = i
		queens = append(queens, p)
	}

	queenRecursion(0)
}
