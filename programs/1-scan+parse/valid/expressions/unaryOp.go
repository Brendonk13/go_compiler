package main;
func x() {
    var a = int(+1)
    b := -a
    var c = !(a < b)
    d := true
    if true {
        var y = x
        x := 10
        print(x);
    }

    e := false

    /* can redeclare variables using := */
    c := !(!a || !b)

}
