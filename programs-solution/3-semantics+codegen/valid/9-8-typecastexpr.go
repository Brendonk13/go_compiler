//~64
//~+6.500000e+001
//~@
//~a
//~a
//~str
//~A

package main

func main(){
	var a int = 64
	var b float64 = float64(a + 1)
	var c, d, e string = string(a), string('a'), string("a")

	type mystring string
	type myint int

	var f, g string = string(mystring("str")), string(myint(65))

	println(a)
	println(b)
	println(c)
	println(d)
	println(e)
	println(f)
	println(g)
}
