// can't call built in functions in statement context
package main;

func x() {
	len(x)
}
