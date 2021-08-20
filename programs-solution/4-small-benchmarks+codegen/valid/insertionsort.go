//~0
//~1
//~2
//~3
//~4
//~5
//~6
//~7
//~8
//~9

package main

var size = 100
var arr [100]int

func insertionSort() {
	for j := 0; j < size; j++ {
		key := arr[j]
		i := j - 1
		for i >= 0 && arr[i] > key {
			arr[i+1] = arr[i]
			i = i - 1
		}

		arr[i+1] = key
	}
}

func main() {
	for j := size - 1; j >= 0; j-- {
		arr[size - j - 1] = j
	}
	insertionSort()
	for i := 0; i < 10; i++ {
		println(arr[i])
	}

}
