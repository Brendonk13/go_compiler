// This program contains an invalid type declaration.
// This should produce a parser error.

package main

type test "abc123"

func main(){
    return;
}
