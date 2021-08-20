//cases in a switch statement need expressions
package main;

func x() {
	switch {
		case: 1 // error
		default: 2
	}
}
