def main():
    file = open('6.txt')
    banks = []
    for line in file:
        cells = [int(i) for i in line.split()]
        banks.extend(cells)

    all_banks = []
    while banks not in all_banks:
        all_banks.append(banks.copy())
        i, m = max(enumerate(banks), key=lambda x: x[1])
        banks[i] = 0
        for x in range(m):
            banks[(i + x + 1) % len(banks)] += 1
    print(len(all_banks))


if __name__ == '__main__':
    main()
