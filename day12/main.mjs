import fs from "fs";

function get_sum(obj, is_valid) {
  if (!is_valid(obj)) {
    return 0;
  }

  switch (typeof obj) {
    case "number":
      return obj;
    case "object": {
      // includes arrays
      let sum = 0;
      for (let key in obj) {
        sum += get_sum(obj[key], is_valid);
      }
      return sum;
    }
    default:
      return 0;
  }
}

function has_red(obj) {
  if (typeof obj !== "object" || obj instanceof Array) {
    return false;
  }

  for (let key in obj) {
    if (obj[key] === "red") {
      return true;
    }
  }

  return false;
}

function part1(input) {
  return get_sum(input, (item) => true);
}

function part2(input) {
  return get_sum(input, (item) => !has_red(item));
}

const input = JSON.parse(fs.readFileSync("./input.txt"));

console.log(part1(input));
console.log(part2(input));
