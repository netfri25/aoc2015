import hashlib
from itertools import count

KEY = "iwrupvqb"

for n in count(0):
    text = hashlib.md5((KEY + str(n)).encode()).hexdigest()
    if text.startswith("000000"):
        print(n)
        break
