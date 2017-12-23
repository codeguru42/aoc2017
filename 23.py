def parse():
    file = open('23.txt')
    instructions = []
    for line in file:
        instructions.append(line.split())
    return instructions


def main():
    instructions = parse()
    print(instructions)


if __name__ == '__main__':
    main()
