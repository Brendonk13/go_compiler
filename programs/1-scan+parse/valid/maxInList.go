
package main;

func main(){
    type list struct {
        prev_ind int
        val int
    }

    var lis [6] list
    max := 1000000
    max_ind := -1
    for i := 0; i < 6; i++ {
        if (lis[i] > max){
            max = lis[i]
            max_ind = i;
        }
    }
    print("max value is :" + max + "found at position: " + max_ind);
}

