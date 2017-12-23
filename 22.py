def parse():
    file = open('22.txt')
    mem = []
    for line in file:
        mem.append(list(line.strip()))
    return mem


def main():
    mem = parse()
    print(mem)


if __name__ == '__main__':
    main()
