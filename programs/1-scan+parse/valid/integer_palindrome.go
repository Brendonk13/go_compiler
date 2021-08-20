// Program to check whether a number is a palindrome or not
package main;
func palindrome(n int) bool {

    var reverse = 0
    var temp = n

    for {
        var remainder = n%10
        reverse = reverse*10 + remainder
        n /= 10
 
        if (n == 0){
            break 
        }
    }
     
        if temp == reverse {
            return true
        } else{
            return false
        }
}


