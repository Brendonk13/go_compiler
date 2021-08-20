// Program to print all prime numbers till n
package main;

func prime(n int) {

    var num =0;
    
    for i := 1; i <= n; i++ { 		 		  
    	var counter = 0 		  
        for num = i; num >=1; num-- {
		    if i % num == 0 {
				counter = counter + 1;
		    }
	 	}
	 	if counter == 2 {
	 		println(i)
	 	}
	}
}
