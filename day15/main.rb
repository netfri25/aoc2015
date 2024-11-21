def parse_line(line)
  cookie = {}
  split = line.split ": "
  name = split[0]
  props = split[1]
  props.split(", ").each do |prop|
    prop_split = prop.split " "
    prop_name = prop_split[0]
    prop_value = prop_split[1]
    cookie[prop_name] = prop_value.to_i
  end
  return name, cookie
end

def read_input(path)
  cookies = {}
  File.foreach(path) do |line|
    name, props = parse_line line
    cookies[name] = props
  end
  return cookies
end

def calculate_score(input, cookie_spoons, exact_calories)
  totals = {}
  input.each do |cookie, props|
    props.each do |prop, prop_value|
      value = prop_value * cookie_spoons[cookie]
      if !totals.member? prop
        totals[prop] = 0
      end

      totals[prop] += value
    end
  end

  return 0 if exact_calories != nil and totals["calories"] != exact_calories

  result = 1
  totals.each do |prop, value|
    next if prop == "calories"
    return 0 if value <= 0
    result *= value
  end

  return result
end

# returns the best possible score achievable
def best_score(input, spoons, exact_calories)
  def best_score_rec(input, spoons, cookie_spoon, cookies, index, biggest, exact_calories)
    cookie = cookies[index]
    if index == cookies.length - 1
      cookie_spoon[cookie] = spoons
      return calculate_score(input, cookie_spoon, exact_calories)
    end

    for n in 0..spoons
      cookie_spoon[cookie] = n
      result = best_score_rec(input, spoons - n, cookie_spoon, cookies, index + 1, biggest, exact_calories)
      biggest = [biggest, result].max
    end

    return biggest
  end

  cookies = input.keys
  return best_score_rec(input, spoons, {}, cookies, 0, 0, exact_calories)
end

def part1(input)
  return best_score(input, 100, nil)
end

def part2(input)
  return best_score(input, 100, 500)
end

input = read_input "./input.txt"
puts "part 1: " + part1(input).to_s
puts "part 2: " + part2(input).to_s
