def main():
    file = open('6.txt')
    banks = []
    for line in file:
        cells = [int(i) for i in line.split()]
        banks.extend(cells)
    print(banks)


if __name__ == '__main__':
    main()
