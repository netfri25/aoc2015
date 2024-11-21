local function add_vec(self, other)
    local x = self.x + other.x
    local y = self.y + other.y
    return { x = x, y = y }
end

local function table_len(tbl)
    local count = 0
    for _ in pairs(tbl) do count = count + 1 end
    return count
end

local f = assert(io.open("./input.txt"))
local input = f:read()
f:close()

local TABLE = {
    ['^'] = { x = 0, y = 1 },
    ['v'] = { x = 0, y = -1 },
    ['>'] = { x = 1, y = 0 },
    ['<'] = { x = -1, y = 0 },
}

local function part1()
    local santa = { x = 0, y = 0 }
    local visited = {}
    visited['0,0'] = true

    for c in input:gmatch(".") do
        local offset = TABLE[c]
        santa = add_vec(santa, offset)
        visited[tostring(santa.x) .. ',' .. tostring(santa.y)] = true
    end

    return table_len(visited)
end

local function part2()
    local santa = { x = 0, y = 0 }
    local robot = { x = 0, y = 0 }
    local visited = {}
    visited['0,0'] = true

    for c in input:gmatch(".") do
        local offset = TABLE[c]
        santa = add_vec(santa, offset)
        visited[tostring(santa.x) .. ',' .. tostring(santa.y)] = true
        robot, santa = santa, robot
    end

    return table_len(visited)
end

print("part 1:", part1())
print("part 2:", part2())
