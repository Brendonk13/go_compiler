def selection_sort(array, length):
    for i in xrange(0, length):
        min = array[i]
        min_index = i
        for j in xrange(i+1, length):
            if array[j] < min:
                min = array[j]
                min_index = j
        
        if min_index != i:
            array[min_index] = array[i]
            array[i] = min

def main():
    array = []
    length = 50000
    for i in xrange(0, length):
        array.append(length-i-1)

    selection_sort(array, length)

    for i in range(10):
        print array[i]

if __name__ == "__main__":
    main()
