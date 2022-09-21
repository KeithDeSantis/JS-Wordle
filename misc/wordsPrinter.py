
with open("../words.txt") as f:
    lines = f.read()

list = lines.split("\n")
print(f'allwords = {list}')