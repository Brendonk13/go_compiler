package main

func main() {
    type innerStruct struct {
        dankness string
    }

    type list struct {
        n []struct {
            prev_ind int
            val int
            x [10]innerStruct
        }
    }

}


