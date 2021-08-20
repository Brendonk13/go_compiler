package main;

func IsPrime(value int) bool {
    for i := 2; i <= int(Floor(float64(value) / 2)); i++ {
        if value%i == 0 {
            return false
        }
    }
    return value > 1
}
