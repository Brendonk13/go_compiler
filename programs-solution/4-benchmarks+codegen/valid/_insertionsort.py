size = 25000
arr = [0] * size

def insertion_sort():
    global size
    global arr
    for j in xrange(1, size):
        key = arr[j]
        i = j - 1
        while i >= 0 and arr[i] > key:
            arr[i+1] = arr[i]
            i = i - 1
        arr[i+1] = key

def main():
    global size
    global arr
    j = size - 1
    while j >= 1:
        arr[size - j - 1] = j
        j = j - 1
    insertion_sort()
    for i in range(10):
        print arr[i]

if __name__ == "__main__":
    main()
