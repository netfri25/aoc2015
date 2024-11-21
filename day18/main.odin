package main

import "core:fmt"
import "core:math"
import "core:os"
import "core:strings"

main :: proc() {
    input, ok := read_input("./input.txt")
    if !ok {
        fmt.eprintln("error reading file")
        return
    }

    sol1 := part1(input);
    fmt.println("part 1:", sol1)
    sol2 := part2(input);
    fmt.println("part 2:", sol2)
}

SIZE :: 100

Mat :: struct {
    data: [SIZE][SIZE]bool
}

part2 :: proc(input: Mat) -> int {
    input := input
    input.data[0][0] = true;
    input.data[0][SIZE-1] = true;
    input.data[SIZE-1][0] = true;
    input.data[SIZE-1][SIZE-1] = true;
    return solve_with(input, step2)
}

part1 :: proc(input: Mat) -> int {
    return solve_with(input, step1)
}

solve_with :: proc(input: Mat, step: proc(^Mat)) -> int {
    input := input

    for _ in 0..<100 {
        step(&input)
    }

    count: int = 0
    for row in 0..<SIZE {
        for col in 0..<SIZE {
            if input.data[row][col] {
                count += 1
            }
        }
    }

    return count
}

is_corner :: proc(row: int, col: int) -> bool {
    return (row == 0 || row == SIZE-1) && (col == 0 || col == SIZE-1)
}

step2 :: proc(self: ^Mat) {
    old_mat: Mat = self^;
    for row in 0..<SIZE {
        for col in 0..<SIZE {
            state := old_mat.data[row][col]
            nbors := count_nbors(&old_mat, row, col)
            self.data[row][col] = is_corner(row, col) || is_alive(state, nbors)
        }
    }
}

step1 :: proc(self: ^Mat) {
    old_mat: Mat = self^;
    for row in 0..<SIZE {
        for col in 0..<SIZE {
            state := old_mat.data[row][col]
            nbors := count_nbors(&old_mat, row, col)
            self.data[row][col] = is_alive(state, nbors)
        }
    }
}

count_nbors :: proc(self: ^Mat, row: int, col: int) -> int {
    count: int = 0
    for r_off in -1..=1 {
        r := row + r_off
        if r < 0 || r >= SIZE {
            continue
        }
        for c_off in -1..=1 {
            c := col + c_off
            if c < 0 || c >= SIZE || r == row && c == col {
                continue
            }

            if self.data[r][c] {
                count += 1
            }
        }
    }

    return count
}

is_alive :: proc(alive: bool, nbors: int) -> bool {
    return nbors == 3 || alive && nbors == 2
}

read_input :: proc(path: string) -> (Mat, bool) {
    output: Mat = ---

    data, ok := os.read_entire_file(path, context.allocator)
    if !ok {
        return output, false
    }
    defer delete(data, context.allocator)

    it := string(data)

    row := 0
    for line in strings.split_lines_iterator(&it) {
        for c, col in line {
            output.data[row][col] = c == '#'
        }
        row += 1
    }

    return output, true
}
