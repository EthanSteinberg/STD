extern crate csv;

use std::error::Error;
use std::io;
use std::process;
use std::collections::HashMap;

fn read_state_fips() -> Result<std::collections::HashMap<i32, String>, Box<Error>> {
    let mut rdr = csv::ReaderBuilder::new()
         .delimiter(b'\t').from_path("../data/fips.csv")?;
    let headers = rdr.headers()?;

    println!("{:?}", headers);

    let mut map = std::collections::HashMap::new();

    for result in rdr.records() {
        let record = result?;
        let code = record.get(0).unwrap().parse::<i32>().unwrap();
        let value = record.get(1).unwrap().to_string();

        if code == 0 {
            continue
        }

        map.insert(code, value);
    }
    Ok(map)
}


fn decimal_mark2(s: String) -> String {
    let mut result = String::with_capacity(s.len() + ((s.len() - 1) / 3));
    let mut i = s.len();
    for c in s.chars() {
        result.push(c);
        i -= 1;
        if i > 0 && i % 3 == 0 {
            result.push(',');
        }
    }
    result
}

fn get_bucket(income: i32) -> String {
    let starting_bucket = 15000;

    let levels = [15_000, 150_000, 200_000, 250_000];

    let current_level = levels.iter().position(|x| income <= *x).unwrap_or(4);

    if current_level == 0 {
        "Less than $14,999".to_string()
    } else if current_level == 4 {
        "$250,000 or more".to_string()
    } else {
        let increment = match current_level {
            1 => 5_000,
            2 => 10_000,
            3 => 25_000,
            a => panic!("Expected thingy {:?}", a)
        };

        let start = (income / increment) * increment;
        let end = start + increment - 1;

        format!("${} - ${}", decimal_mark2(start.to_string()), decimal_mark2(end.to_string()))
    }
}

fn get_education(education: i32) -> &'static str {
    if education <= 15 {
        "Some High School or Less"
    } else if education <= 17 {
        "High School"
    } else if education <= 19 {
        "Some College"
    } else if education <= 21 {
        "College"
    } else {
        "Graduate School"
    }
}

fn example() -> Result<(), Box<Error>> {
    let age_groups: HashMap<&str, (i32, i32)> = [
         ("0-17 years old", (0, 17)),
         ("18-24 years old", (18, 24)),
         ("25-34 years old", (25, 34)),
         ("35-44 years old", (35, 44)),
         ("45-54 years old", (45, 54)),
         ("55-64 years old", (55, 64)),
         ("65-74 years old", (65, 74)),
         ("75+ years old", (75, 100000)),
        ].iter().cloned().collect();

    let mut counts : std::collections::HashMap<(String, String, String, String, String), f64> =  std::collections::HashMap::new();

    let fips = read_state_fips().unwrap();

    // Build the CSV reader and iterate over each record.
    let mut rdr = csv::Reader::from_path("../data/census/psam_all.csv")?;
    let headers = rdr.headers()?;

    println!("{:?}", headers);

    let income_column = headers.iter().position(|x| x == "PINCP").unwrap();
    let sex_column = headers.iter().position(|x| x == "SEX").unwrap();
    let education_column = headers.iter().position(|x| x == "SCHL").unwrap();
    let age_column = headers.iter().position(|x| x == "AGEP").unwrap();
    let state_column = headers.iter().position(|x| x == "ST").unwrap();
    let weight_column = headers.iter().position(|x| x == "PWGTP").unwrap();

    for result in rdr.records() {
        // The iterator yields Result<StringRecord, Error>, so we check the
        // error here.
        let record = result?;

        let income_str = record.get(income_column).unwrap();

        if income_str == "" {
            continue
        }

        let income = get_bucket(income_str.parse::<i32>().unwrap());
        let sex = match record.get(sex_column).unwrap().parse::<i32>().unwrap() {
            1 => "Male",
            2 => "Female",
            a => {panic!("Did not expect {:?}", a);}
        };
        let education = get_education(record.get(education_column).unwrap().parse::<i32>().unwrap()).to_string();
        let age_integer = record.get(age_column).unwrap().parse::<i32>().unwrap();

        let age = age_groups.iter().find(|(name, (start, end))| age_integer >= *start && age_integer <= *end).unwrap().0;

        let state = fips.get(&record.get(state_column).unwrap().parse::<i32>().unwrap()).unwrap();
        let weight = record.get(weight_column).unwrap().parse::<i32>().unwrap();

        let key = (age.to_string(), sex.to_string(), income.to_string(), education.to_string(), state.to_string());

        // println!("{:?}", income);
        // println!("{:?}", sex);
        // println!("{:?}", education);
        // println!("{:?}", age);
        // println!("{:?}", state);
        // println!("{:?}", weight);
        // println!("FOO");

        let entry = counts.entry(key).and_modify(|v| *v += weight as f64).or_insert(weight as f64);
    }

    let mut wtr = csv::WriterBuilder::new()
        .delimiter(b'\t').from_path("../data/census_aggregated.csv")?;

    // When writing records without Serde, the header record is written just
    // like any other record.
    // (age.to_string(), sex.to_string(), income.to_string(), education.to_string(), state.to_string());

    wtr.write_record(&["age", "gender", "income", "education", "state", "count"])?;

    for (key, value) in counts.iter() {
        wtr.write_record(&[key.0.clone(), key.1.clone(), key.2.clone(), key.3.clone(), key.4.clone(), value.to_string()])?;
    }

    wtr.flush()?;

    println!("{:?}", counts);
    Ok(())
}

fn main() {
    if let Err(err) = example() {
        println!("error running example: {}", err);
        process::exit(1);
    }
}
