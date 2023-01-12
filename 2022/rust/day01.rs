use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
// it should be a vector of vectors of ints

fn parse_file(filename: &str) -> Vec<Vec<i64>> {
    /*    let file = File::open(filename).expect("Can't open file!");
        let mut contents = String::new();
        file.read_to_string(&mut contents).expect("Unable to read to line.");

        let mut vec_numbers = Vec::<i32>::new();
        for line in contents.lines().into_iter(){
            vec_numbers.push(line.parse::<i32>().unwrap());
        }
    */
    let file = match File::open(&filename) {
        // The `description` method of `io::Error` returns a string that describes the error
        Err(why) => panic!("fuck this"),
        Ok(file) => file,
    };
    let lines = BufReader::new(file).lines();
    for line in lines {
        let mystr = line.unwrap();
        if mystr.is_empty() {
            println!("this is where we would start a new vector");
        } else {
            let intval = mystr.parse::<i64>().unwrap();
            println!("I found an int of value {}", intval);
        }
    }
    let mut output: Vec<Vec<i64>> = vec![];
    let mut current_elf: Vec<i64>;
    panic!("At least it compiled.");
}

//fn parse_lines(lines: Iterator<String>) -> Vec<Vec<i64>> {
//}

fn main() {
    parse_file("../data/input01.txt");
}
