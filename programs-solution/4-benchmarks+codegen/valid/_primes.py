def main():
    target = 1000000
    primes = []
    primes.append(2)

    sqrt = 2
    square = 4
    counter = 1
    number = 2
    last_prime = 2
    i = 0
    temp = 0
    is_prime = True
    num_primes = 1

    while counter < target:
        number += 1
        if number > square:
            sqrt += 1
            square = sqrt * sqrt
        is_prime = True
        for i in xrange(num_primes):
            temp = primes[i]
            if temp > sqrt:
                break
            if number % temp == 0:
                is_prime = False
                break
        if is_prime:
            primes.append(number)
            num_primes += 1
            last_prime = number
            counter += 1
    print "Last prime: %d" % last_prime

if __name__ == "__main__":
    main()
