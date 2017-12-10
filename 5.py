def do_jumps(jumps, f):
    count = 0
    address = 0
    while 0 <= address < len(jumps):
        new_address = address + jumps[address]
        f(jumps, address)
        address = new_address
        count += 1
    return count


def part1(jumps):
    def f(jumps, address):
        jumps[address] += 1
    return do_jumps(jumps, f)


def main():
    file = open('5.txt')
    jumps = [int(line) for line in file]
    print(part1(jumps.copy()))

if __name__ == '__main__':
    main()
