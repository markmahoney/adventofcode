use std::io;
use std::str::Chars;

// const CONVERSIONS: [(&str, &str); 9] = [
//     ("one", "1"),
//     ("two", "2"),
//     ("three", "3"),
//     ("four", "4"),
//     ("five", "5"),
//     ("six", "6"),
//     ("seven", "7"),
//     ("eight", "8"),
//     ("nine", "9")
// ];

// fn convert_to_nums(line: String) -> String {
//     CONVERSIONS.iter().fold(line, |acc, (name, num)| acc.replace(name, num))
// }

// fn scan_for_match<I: Iterator<Item = char>>(mut chars: &mut I) -> Option<char> {
//     println!("{}", chars.next().unwrap());
//     None
// }
fn scan_for_match(input: &mut Chars, find: &str, convert: char) -> Option<char> {
    let mut p = input.peekable();
    println!("{}", p.peek().unwrap());
    None
}

fn extract_nums(line: &str) -> (u32, u32) {
    let mut first: Option<u32> = None;
    let mut last: Option<u32> = None;
    let mut chars = line.chars();
    
    // for c in chars {
    while let Some(c) = chars.next() {
        (first, last) = match c {
            v @ '0'..='9' => Some(v),
            't' => scan_for_match(&mut chars, "wo", '2'),
            _ => None,
        }.map_or((first, last), |v| {
            match first {
                Some(_) => (first, v.to_digit(10)),
                None => (v.to_digit(10), v.to_digit(10)),
            }
        })
    }

    (first.expect("first unexpectedly empty"), last.expect("last unexpectedly empty"))
}

fn main() {
    let lines = io::stdin().lines();
    let mut values: Vec<u32> = Vec::new();

    for line in lines {
        // let converted = convert_to_nums(line.unwrap());
        let (first, last) = extract_nums(&line.unwrap()[..]);
        println!("first {}, last {}, total {}", first, last, first * 10 + last);
        values.push(first * 10 + last);
    }

    println!("final value: {}", values.into_iter().reduce(|acc, v| acc + v).unwrap());
}
