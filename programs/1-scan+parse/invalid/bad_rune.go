// This program contains a rune with an invalid escape sequence.
// This shoul produce a lexer error.

package main

func main() {
    var x rune = '\c'
    return x
}
