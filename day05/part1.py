import operator

def is_nice(word: str) -> bool:
    # duplicates
    if not any(map(operator.eq, word, word[1:])):
        return False

    VOWELS = "aeiou"
    if len(list(filter(lambda c: c in VOWELS, word))) < 3:
        return False

    BANNED = ["ab", "cd", "pq", "xy"]
    if any(map(lambda w: w in word, BANNED)):
        return False

    return True

with open("./input.txt") as f:
    words = map(str.strip, f.readlines())

print(len(list(filter(is_nice, words))))
