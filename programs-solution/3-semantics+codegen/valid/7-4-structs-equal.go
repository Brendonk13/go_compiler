//~true
//~false
//~false
//~true
//~true
//~false
//~true
//~false

package main

type my struct {
	a int
}

type empty struct {

}

type uncomparable struct {
	a []int
}

type blank struct {
	_ int
	a int
	_ int
}

func main() {
	var m1, m2 my
	m1.a = 1
	m2.a = 1
	println(m1 == m2)
	println(m1 != m2)
	m2.a = 2
	println(m1 == m2)
	println(m1 != m2)

	var m3, m4 empty
	println(m3 == m4)
	println(m3 != m4)

	var b1, b2 blank
	println(b1 == b2)
	println(b1 != b2)
}
