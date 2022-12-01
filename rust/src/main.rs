#![feature(associated_type_defaults)]

use std::fs;
use crate::solutions::day1::Day1;

mod solutions;

type Day = u32;

fn load_input(day: Day) -> String {
  fs::read_to_string(format!("../inputs/day{:02}", day)).unwrap()
}

trait Solution {
  type PreparedInput;
  type Output1;
  type Output2 = Self::Output1;
  fn prepare_input(input: String) -> Self::PreparedInput;
  fn run_part_1(input: &Self::PreparedInput) -> Self::Output1;
  fn run_part_2(input: &Self::PreparedInput) -> Self::Output2;
  fn run_both_parts(input: String) -> (Self::Output1, Self::Output2) {
    let input = Self::prepare_input(input);
    (Self::run_part_1(&input), Self::run_part_2(&input))
  }
}

fn main() {
  println!("{:?}", Day1::run_both_parts(load_input(1)));
}
