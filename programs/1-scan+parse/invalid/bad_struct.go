// This program contains a a struct with invalid parameters.
// This should produce a parser error.

package main;

type test struct{
    x int
    y int
}
var p test;
p.x  = 0
p.y  = p.x + "asd \r ";

type test struct{
    var x,y,z int = 1,2,3
}

func main (){
    return;
}
