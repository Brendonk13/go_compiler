package main
//~true

func main(){
    var result bool
        value := 2147483647
    for i := 2; i <= int(float64(value) / 2.0); i++ {
        if value%i == 0 {
            result = false
            break
        }
    }
    result = value > 1
    println(result)
}
