use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn parse_file(filename: &str) -> Vec<Vec<i64>> {
    let file = match File::open(&filename) {
        // The `description` method of `io::Error` returns a string that describes the error
        Err(_why) => panic!("fuck this"),
        Ok(file) => file,
    };
    let lines = BufReader::new(file).lines();
    let mut output: Vec<Vec<i64>> = vec![];
    let mut current_elf: Vec<i64> = vec![];
    for line in lines {
        let mystr = line.unwrap();
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

fn main() {
    println!(
        "can you print a vec of vecs? {0:?}",
        parse_file("../data/input01.txt")
    );
}
