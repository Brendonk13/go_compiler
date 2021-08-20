s = []
w = []
cost_s = 0
r = 0

def rand():
    global r
    r = (8121*r + 28411) % 134456
    return r

def compute_cost(n, A):
    sum = 0
    for i in xrange(n-1):
        sum += w[A[i]][A[i+1]]
    return sum + w[n-1][0]

def permute(n, A):
    global cost_s
    global s
    global w
    index = []
    for i in xrange(n):
        index.append(0)

    temp = 0
    swap = 0
    cost_i = 0

    i = 1
    while i < n:
        if index[i] < i:
            swap = i % 2 * index[i]
            temp = A[swap]
            A[swap] = A[i]
            A[i] = temp
            cost_i = compute_cost(n, A)
            if cost_i < cost_s:
                cost_s = cost_i
                for j in xrange(n-1):
                    s[j] = A[j]
            index[i] += 1
            i = 1
        else:
            index[i] = 0
            i += 1

def main():
    global w
    global cost_s
    n = 11
    for i in xrange(n):
        s.append(i)
        w_i = []
        for j in xrange(n):
            w_i.append(rand())
        w.append(w_i)
    cost_s = compute_cost(n, s)
    print "Ordered path cost:", cost_s
    permute(n, s)
    print "Optimal path cost:", cost_s

if __name__ == "__main__":
    main()
