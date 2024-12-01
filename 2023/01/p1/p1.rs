use std::io;

fn extract_nums(line: &String) -> (u32, u32) {
    let mut first: Option<u32> = None;
    let mut last: Option<u32> = None;
    let chars = line.chars();
    
    for c in chars {
        (first, last) = match c {
            v @ '0'..='9' => Some(v),
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
        let (first, last) = extract_nums(&(line.unwrap()));
        println!("first {}, last {}, total {}", first, last, first * 10 + last);
        values.push(first * 10 + last);
    }

    println!("final value: {}", values.into_iter().reduce(|acc, v| acc + v).unwrap());
}
