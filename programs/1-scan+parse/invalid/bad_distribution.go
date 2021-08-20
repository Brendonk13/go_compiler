// This program contains a var keyword in a var distribution.
// This should produce a parser error.

package main

func main(){
    var(
        x,y = 0,1
        var z = "hi"
    )
}
