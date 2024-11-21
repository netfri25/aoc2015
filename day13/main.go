package main

import (
	"bufio"
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
)

type happiness = map[string]map[string]int64

func main() {
	file_path := "./input.txt"
	lines, err := readLines(file_path)
	if err != nil {
		fmt.Printf("Error opening %s: %s\n", file_path, err)
	}

	input, err := parseLines(lines)
	if err != nil {
		fmt.Printf("Error parsing input: %s\n", err)
	}

	part1_sol := part1(input)
	fmt.Printf("part 1: %d\n", part1_sol)

	part2_sol := part2(input)
	fmt.Printf("part 2: %d\n", part2_sol)
}

func part1(input happiness) int64 {
	return optimalSeating(input, []string{})
}

func part2(input happiness) int64 {
	var names []string
	for name := range input {
		names = append(names, name)
	}

	input["Me"] = make(map[string]int64)
	for _, name := range names {
		input["Me"][name] = 0
	}

	return optimalSeating(input, []string{})
}

func readLines(path string) ([]string, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return lines, nil
}

func parseLines(lines []string) (happiness, error) {
	output := make(happiness)
	for _, line := range lines {
		fields := strings.Fields(line)
		of := fields[0]
		next_to := fields[2]
		amount, err := strconv.ParseInt(fields[1], 10, 64)
		if err != nil {
			return nil, err
		}
		_, exist := output[of]
		if !exist {
			output[of] = make(map[string]int64)
		}
		output[of][next_to] = amount
	}

	return output, nil
}

func optimalSeating(happiness happiness, seating []string) int64 {
	if len(seating) == len(happiness) {
		return calculateSeating(happiness, seating)
	} else {
		var best int64 = -1
		for name := range happiness {
			if slices.Contains(seating, name) {
				continue
			}

			seating = append(seating, name)
			value := optimalSeating(happiness, seating)
			if value > best {
				best = value
			}
			seating = seating[:len(seating)-1]
		}
		return best
	}
}

func calculateSeating(happiness happiness, seating []string) int64 {
	var total int64 = 0
	for i, name := range seating {
		prev_index := (i + len(seating) - 1) % len(seating)
		next_index := (i + 1) % len(seating)
		total += happiness[name][seating[prev_index]]
		total += happiness[name][seating[next_index]]
	}
	return total
}
