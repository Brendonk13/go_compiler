count = 0

def towers(num, src, dst, aux):
    global count
    if num == 1:
        count += 1
        return
    towers(num-1, src, aux, dst)
    count += 1
    towers(num-1, aux, dst, src)

def main():
    global count
    towers(28, 'A', 'B', 'C')
    print "Number of moves:", count

if __name__ == "__main__":
    main()
