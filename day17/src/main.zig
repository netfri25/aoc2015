const std = @import("std");

// NOTE: fuck it. I'm going to leak everywhere.

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var buffer: []const u8 = undefined;
    const lines = try readLines(allocator, "./input.txt", &buffer);
    const conts = try parseInput(allocator, lines);
    const target = 150;

    const sol1 = part1(conts.items, target);
    std.debug.print("part 1: {}\n", .{ sol1 });

    const to_use = minContainers(conts.items, target, 0);
    const sol2 = part2(conts.items, target, to_use);
    std.debug.print("part 2: {}\n", .{ sol2 });
}

fn part1(slice: []const i32, target: i32) u32 {
    if (target == 0) {
        return 1;
    } else if (target < 0 or slice.len == 0) {
        return 0;
    } else {
        const item = slice[0];
        const tail = slice[1..];
        return part1(tail, target) + part1(tail, target - item);
    }
}

fn part2(slice: []const i32, target: i32, to_use: u32) u32 {
    if (target == 0) {
        return 1;
    } else if (target < 0 or slice.len == 0 or to_use == 0) {
        return 0;
    } else {
        const item = slice[0];
        const tail = slice[1..];
        return part2(tail, target, to_use) + part2(tail, target - item, to_use - 1);
    }
}

fn minContainers(slice: []const i32, target: i32, count: u32) u32 {
    if (target == 0) {
        return count;
    } else if (target < 0 or slice.len == 0) {
        return std.math.maxInt(u32);
    } else {
        const item = slice[0];
        const tail = slice[1..];
        return @min(minContainers(tail, target, count), minContainers(tail, target - item, count + 1));
    }
}

fn parseInput(allocator: std.mem.Allocator, lines: std.ArrayList([]const u8)) !std.ArrayList(i32) {
    var conts = std.ArrayList(i32).init(allocator);
    for (lines.items) |line| {
        const result = try std.fmt.parseInt(i32, line, 10);
        try conts.append(result);
    }

    return conts;
}

fn readLines(allocator: std.mem.Allocator, path: []const u8, buffer: *[]const u8) !std.ArrayList([]const u8) {
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    buffer.* = try file.readToEndAlloc(allocator, std.math.maxInt(usize));

    var lines = std.ArrayList([]const u8).init(allocator);
    var it = std.mem.tokenizeAny(u8, buffer.*, "\n");
    while (it.next()) |line| {
        try lines.append(line);
    }

    return lines;
}
