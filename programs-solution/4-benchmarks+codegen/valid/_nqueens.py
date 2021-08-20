chess_grid_size = 27
queens = []

def place_queens(index):
    global queens
    if index >= chess_grid_size:
        return 1

    solution_found = 0
    p = queens[index]

    for i in xrange(chess_grid_size):
        p[0] = i

        all_good = 1
        for j in xrange(index):
            p2 = queens[j]

            if p[0] == p2[0] or p2[1]-p[1] == p2[0]-p[0] or p2[1]-p[1] == p[0]-p2[0]:
                all_good = 0
                break

        if all_good == 1:
            queens[index] = p
            if place_queens(index+1) == 1:
                print p
                solution_found = 1
                break
    return solution_found

def main():
    global queens
    for i in xrange(chess_grid_size):
        p = [0, i]
        queens.append(p)
    place_queens(0)

if __name__ == "__main__":
    main()
