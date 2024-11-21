#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <limits.h>
#include <stdint.h>

int costs[8][8];

int find_shortest_path(int *path, int total, uint8_t bits);
int find_longest_path(int *path, int total, uint8_t bits);

int main(void) {
    FILE* file = fopen("./input.txt", "r");

    for (;;) {
        int from, to, cost;
        int read = fscanf(file, "%d to %d = %d", &from, &to, &cost);
        if (read != 3) break;

        printf("%d to %d = %d\n", from, to, cost);
        costs[from][to] = cost;
        costs[to][from] = cost;
    }
    fclose(file);

    int path[8];

    int smallest = INT_MAX;
    for (size_t i = 0; i < 8; i++) {
        path[0] = i;
        int min_cost = find_shortest_path(path, 0, 1 << i);
        if (min_cost < smallest) {
            smallest = min_cost;
        }
    }
    printf("part 1: %d\n", smallest);

    int biggest = INT_MIN;
    for (size_t i = 0; i < 8; i++) {
        path[0] = i;
        int max_cost = find_longest_path(path, 0, 1 << i);
        if (max_cost > biggest) {
            biggest = max_cost;
        }
    }
    printf("part 2: %d\n", biggest);
}

int find_shortest_path(int *path, int total, uint8_t bits) {
    if (bits == 0xFF) {
        return total;
    }

    int smallest = INT_MAX;

    for (size_t i = 0; i < 8; i++) {
        bool used = (bits >> i) & 1;
        if (used) continue;

        path[1] = i;

        int new_total = total + costs[*path][i];
        int min_cost = find_shortest_path(path + 1, new_total, bits | (1 << i));
        if (min_cost < smallest) {
            smallest = min_cost;
        }
    }

    return smallest;
}

int find_longest_path(int *path, int total, uint8_t bits) {
    if (bits == 0xFF) {
        return total;
    }

    int biggest = INT_MIN;

    for (size_t i = 0; i < 8; i++) {
        bool used = (bits >> i) & 1;
        if (used) continue;

        path[1] = i;

        int new_total = total + costs[*path][i];
        int max_cost = find_longest_path(path + 1, new_total, bits | (1 << i));
        if (max_cost > biggest) {
            biggest = max_cost;
        }
    }

    return biggest;
}
