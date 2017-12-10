def main():
    file = open('5.txt')
    jumps = [int(line) for line in file]
    print(jumps)

if __name__ == '__main__':
    main()
