use std::fs;

const INPUT_PATH: &str = "./input.txt";

fn main() {
    let input = fs::read_to_string(INPUT_PATH).unwrap();
    let insts: Vec<Inst> = input.lines().map(|line| parse_inst(line).unwrap()).collect();

    // part 1
    let mut regs = Regs::default();
    simulate(&insts, &mut regs);
    println!("{:?}", regs);

    // part 2
    let mut regs = [1, 0];
    simulate(&insts, &mut regs);
    println!("{:?}", regs);
}

type Regs = [usize; 2];

fn simulate(insts: &[Inst], regs: &mut Regs) {
    let mut ip: i32 = 0;

    while let Some(inst) = insts.get(ip as usize) {
        match *inst {
            Inst::Inc(reg) => regs[reg as usize] += 1,
            Inst::Hlf(reg) => regs[reg as usize] /= 2,
            Inst::Tpl(reg) => regs[reg as usize] *= 3,
            Inst::Jmp(off) => {
                ip += off;
                continue
            },
            Inst::Jio(reg, off) => {
                if regs[reg as usize] == 1 {
                    ip += off;
                    continue
                }
            },
            Inst::Jie(reg, off) => {
                if regs[reg as usize] % 2 == 0 {
                    ip += off;
                    continue
                }
            },
        }

        ip += 1;
    }
}

fn parse_inst(input: &str) -> Option<Inst> {
    let mut iter = input.split(' ');
    Some(match iter.next()? {
        "inc" => Inst::Inc(parse_reg(iter.next()?)?),
        "hlf" => Inst::Hlf(parse_reg(iter.next()?)?),
        "tpl" => Inst::Tpl(parse_reg(iter.next()?)?),
        "jmp" => Inst::Jmp(iter.next()?.parse().ok()?),
        "jio" => {
            let reg = parse_reg(iter.next()?)?;
            let off = iter.next()?.parse().ok()?;
            Inst::Jio(reg, off)
        }
        "jie" => {
            let reg = parse_reg(iter.next()?)?;
            let off = iter.next()?.parse().ok()?;
            Inst::Jie(reg, off)
        }
        _ => return None
    })
}

fn parse_reg(input: &str) -> Option<Reg> {
    Some(match input {
        "a" => Reg::A,
        "b" => Reg::B,
        _ => return None
    })
}

#[derive(Debug, Clone)]
enum Inst {
    Inc(Reg),
    Hlf(Reg),
    Tpl(Reg),
    Jmp(i32),
    Jio(Reg, i32),
    Jie(Reg, i32),
}

#[derive(Debug, Clone, Copy)]
enum Reg {
    A = 0,
    B = 1,
}
