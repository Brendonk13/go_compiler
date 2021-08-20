// This program contains an invaild hexadecimal in literal.
// This should produce a lexer error.

package main

func main(){
    var x = 0xff123gh
    return
}
