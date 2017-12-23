def parse():
    file = open('23.txt')
    instructions = []
    for line in file:
        instructions.append(line.split())
    return instructions


def execute(instructions):
    registers = {x: 0 for x in 'abcdefgh'}
    count = 0
    curr = 0
    while curr < len(instructions):
        instruction = instructions[curr]
        if instruction[2].isalpha():
            rvalue = int(registers[instruction[2]])
        else:
            rvalue = int(instruction[2])

        if instruction[0] == 'set':
            registers[instruction[1]] = rvalue
        elif instruction[0] == 'sub':
            registers[instruction[1]] -= rvalue
        elif instruction[0] == 'mul':
            registers[instruction[1]] *= rvalue
            count += 1
        elif instruction[0] == 'jnz':
            if instruction[1].isalpha():
                if registers[instruction[1]] != 0:
                    curr += rvalue - 1
            elif int(instruction[1]) != 0:
                curr += rvalue - 1
        else:
            print(f'Invalid instruction {instruction[0]}')
        curr += 1
    print(count)


def main():
    instructions = parse()
    execute(instructions)


if __name__ == '__main__':
    main()
