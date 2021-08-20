//~applepear
//~true false
//~false true
//~false false
//~false true
//~false
//~true
//~true
//~false

package main

func main(){
	var s1 string = "apple"
	var s2 string = "pear"
	var s3 string = s1 + s2
	println(s3)

	var s4 string = "apple"

	println(s1 == s4, s1 == s2)
	println(s1 != s4, s1 != s2)
	println(s1 > s4, s1 > s2)
	println(s1 < s4, s1 < s2)

	println("apple" == "Apple")
	println("apple" != "Apple")
	println("apple" > "Apple")
	println("apple" < "Apple")
}
