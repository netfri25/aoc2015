import os

fn main() {
  input_path := "./input.txt"
  input_content := os.read_file(input_path) or {
    eprintln("${err}")
    return
  }
  mut input := parse_input(input_content)

  template_path := "./template.txt"
  template_content := os.read_file(template_path) or {
    eprintln("${err}")
    return
  }
  template := parse_sue(template_content)

  sol1 := part1(input, template)
  println("part 1: ${sol1}")

  sol2 := part2(mut input, template)
  println("part 2: ${sol2}")
}

type Sue = map[string]int
type Sues = []Sue

fn part1(input Sues, template Sue) int {
  for index, sue in input {
    if verify_sue(sue, template) {
      return index + 1
    }
  }

  return -1
}

fn verify_sue(sue Sue, template Sue) bool {
  return sue.keys().all(sue[it] == template[it])
}

fn part2(mut input Sues, template Sue) int {
  for index, mut sue in input {
    if real_sue(mut sue, template) {
      return index + 1
    }
  }

  return -1
}

fn real_sue(mut sue Sue, template Sue) bool {
  cats := !("cats" in sue) || template['cats'] < sue['cats']
  if !cats { return false }
  trees := !("trees" in sue) || template['trees'] < sue['trees']
  if !trees { return false }
  pomeranians := !("pomeranians" in sue) || template['pomeranians'] > sue['pomeranians']
  if !pomeranians { return false }
  goldfish := !("goldfish" in sue) || template['goldfish'] > sue['goldfish']
  if !goldfish { return false }

  sue.delete('cats')
  sue.delete('trees')
  sue.delete('pomeranians')
  sue.delete('goldfish')

  return verify_sue(sue, template)
}

fn parse_input(content string) Sues {
  return content.split_into_lines().map(parse_sue(it))
}

fn parse_sue(data string) Sue {
  mut sue := Sue(map[string]int{})
  props := data.split(", ")
  for prop_text in props {
    prop_split := prop_text.split(": ")
    prop_name := prop_split[0]
    prop_value := prop_split[1].int()
    sue[prop_name] = prop_value
  }
  return sue
}
