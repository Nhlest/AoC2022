use std::io::{ErrorKind, Read};
use std::str::FromStr;
use crate::Solution;

pub struct Day2;

#[repr(u8)]
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum RPS {
  Rock,
  Paper,
  Scissors
}

impl FromStr for RPS {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s.chars().next().unwrap() {
      'A' => Ok(Self::Rock),
      'B' => Ok(Self::Paper),
      'C' => Ok(Self::Scissors),
      'X' => Ok(Self::Rock),
      'Y' => Ok(Self::Paper),
      'Z' => Ok(Self::Scissors),
      _ => Err(())
    }
  }
}

#[repr(u8)]
enum WLD {
  Win,
  Draw,
  Lose
}

impl WLD {
  fn score(self) -> i32 {
    match self {
      WLD::Win => 6,
      WLD::Draw => 3,
      WLD::Lose => 0
    }
  }
}

impl RPS {
  fn to_state(self) -> WLD {
    match self {
      RPS::Rock => WLD::Lose,
      RPS::Paper => WLD::Draw,
      RPS::Scissors => WLD::Win,
    }
  }
  fn stronger(self) -> Self {
    match self {
      RPS::Rock => RPS::Paper,
      RPS::Paper => RPS::Scissors,
      RPS::Scissors => RPS::Rock
    }
  }
  fn weaker(self) -> Self {
    match self {
      RPS::Rock => RPS::Scissors,
      RPS::Paper => RPS::Rock,
      RPS::Scissors => RPS::Paper
    }
  }
  fn outcome(self, other: Self) -> WLD {
    if self == other {
      WLD::Draw
    } else if self.weaker() == other {
      WLD::Lose
    } else {
      WLD::Win
    }
  }
  fn score(self) -> i32 {
    match self {
      RPS::Rock => 1,
      RPS::Paper => 2,
      RPS::Scissors => 3
    }
  }
}

impl Solution for Day2 {
  type PreparedInput = Vec<(RPS, RPS)>;
  type Output1 = i32;

  fn prepare_input(input: String) -> Self::PreparedInput {
    input
      .trim()
      .split("\n")
      .map(|line:&str| {
        let mut words = line.split(" ");
        let a = words.next().unwrap().parse().unwrap();
        let b = words.next().unwrap().parse().unwrap();
        (a,b)
      })
      .collect()
  }

  fn run_part_1(input: &Self::PreparedInput) -> Self::Output1 {
    input.iter().map(|(a, b)| a.outcome(*b).score() + b.score()).sum()
  }

  fn run_part_2(input: &Self::PreparedInput) -> Self::Output2 {
    input.iter().map(|(a, b)| {
      let b = match b.to_state() {
        WLD::Win => a.stronger(),
        WLD::Draw => *a,
        WLD::Lose => a.weaker(),
      };
      a.outcome(b).score() + b.score()
    }).sum()
  }
}
