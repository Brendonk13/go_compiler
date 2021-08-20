// This function contwains a function declaration with an extra comma in its arguments.
// This should produce a parser error.

package main

func test(x,y int, t,u string,){
    return;
}
