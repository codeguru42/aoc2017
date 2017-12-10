def main():
    file = open('5.txt')
    jumps = [int(line) for line in file]

    count = 0
    address = 0
    while 0 <= address < len(jumps):
        print(address, count)
        new_address = address + jumps[address]
        jumps[address] += 1
        address = new_address
        count += 1
    print(count)

if __name__ == '__main__':
    main()
