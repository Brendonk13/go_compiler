package main

func main(){
    var a [] int
    for i := 0; i < 10; i ++ {
        println (" Cap :" , cap (a) , ", len :" , len(a ))
        a = append (a , 0)
    }
}
