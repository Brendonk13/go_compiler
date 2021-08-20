// This program contains non-declarations outside a function body.
// This should produce a parser error.

package main

if(true){
    println "HI"
}

func main() {
    return;
}
