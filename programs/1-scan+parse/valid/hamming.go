//Function to compute the hamming distance between two int arrays of the same length.

package main;


func hamming(x []int, y []int){
    var dist int = 0;
    if(len(x) != len(y)){
        return -1
    }
    for i := 0; i < size; i++ {
        if(x[i] != y[i]) {
            dist++
        }
    }
    return dist
}
