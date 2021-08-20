def merge(array, temp, lo, hi):
    mid = (lo+hi)/2
    temp_index = lo
    index_left = lo
    index_right = mid+1

    while index_left <= mid and index_right <= hi:
        if array[index_left] < array[index_right]:
            temp[temp_index] = array[index_left]
            index_left += 1
        else:
            temp[temp_index] = array[index_right]
            index_right += 1

        temp_index += 1

    while index_right <= hi:
        temp[temp_index] = array[index_right]
        index_right += 1
        temp_index += 1

    while index_left <= mid:
        temp[temp_index] = array[index_left]
        index_left += 1
        temp_index += 1

    for i in xrange(lo, hi+1):
        array[i] = temp[i]

def merge_sort_helper(array, temp, lo, hi):
    if lo >= hi:
        return

    mid = (lo+hi)/2
    merge_sort_helper(array, temp, lo, mid)
    merge_sort_helper(array, temp, mid+1, hi)

    merge(array, temp, lo, hi)

def merge_sort(array, length):
    temp = []
    for i in xrange(0, length):
        temp.append(array[i])
    merge_sort_helper(array, temp, 0, length-1)

def main():
    array = []
    length = 10000000
    for i in xrange(0, length):
        array.append(length-i-1)

    merge_sort(array, length)

    for i in range(10):
        print array[i]

if __name__  == "__main__":
    main()
