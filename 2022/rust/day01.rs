use std::fs::File;
use std::io::BufReader;
use std::io::Read;

/*
fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}
*/

fn parse_multiline_string(one_big_string: &String) -> Vec<Vec<i64>> {
    let mut output: Vec<Vec<i64>> = vec![];
    let mut current_elf: Vec<i64> = vec![];
    for mystr in one_big_string.lines() {
        if mystr.is_empty() {
            output.push(current_elf);
            current_elf = vec![];
        } else {
            let intval = mystr.parse::<i64>().unwrap();
            current_elf.push(intval);
        }
    }
    output
}

fn parse_file(filename: &str) -> Vec<Vec<i64>> {
    let file = match File::open(&filename) {
        Err(why) => panic!("{}", why),
        Ok(file) => file,
    };
    let mut contents = String::new();
    match BufReader::new(file).read_to_string(&mut contents) {
        Err(why) => panic!("{}", why),
        Ok(_usize) => parse_multiline_string(&contents),
    }
}

fn total_weight_by_elf(elves: &Vec<Vec<i64>>) -> Vec<i64> {
    elves.iter().map(|x| x.iter().sum()).collect()
}

fn max_weight_of_one_elf(elves: &Vec<Vec<i64>>) -> i64 {
    elves.iter().map(|x| x.iter().sum::<i64>()).max().unwrap()
}

fn solve_part_2(elves: &Vec<Vec<i64>>) -> i64 {
    let mut elf_weights = total_weight_by_elf(elves);
    elf_weights.sort();
    elf_weights.as_slice()[elf_weights.len() - 3..].iter().sum()
}

fn main() {
    let elves = parse_file("../data/input01.txt");
    println!("Part 1 solution: {}", max_weight_of_one_elf(&elves));
    println!("Part 2 solution: {}", solve_part_2(&elves));
}
