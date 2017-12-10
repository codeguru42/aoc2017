def part1(jumps):
    count = 0
    address = 0
    while 0 <= address < len(jumps):
        new_address = address + jumps[address]
        jumps[address] += 1
        address = new_address
        count += 1
    return count

def main():
    file = open('5.txt')
    jumps = [int(line) for line in file]
    print(part1(jumps))

if __name__ == '__main__':
    main()
