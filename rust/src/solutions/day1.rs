use crate::Solution;

pub struct Day1;

impl Solution for Day1 {
  type PreparedInput = Vec<Vec<i32>>;
  type Output1 = i32;

  fn prepare_input(input: String) -> Self::PreparedInput {
    input
      .split("\n\n")
      .map(|group| group.split("\n").map(|number| number.parse().unwrap()).collect())
      .collect()
  }

  fn run_part_1(input: &Self::PreparedInput) -> Self::Output1 {
    input.iter().map(|x| x.iter().sum()).max().unwrap()
  }

  fn run_part_2(input: &Self::PreparedInput) -> Self::Output2 {
    let mut sums = input.iter().map(|x| x.iter().sum::<i32>()).collect::<Vec<_>>();
    sums.sort_by(|a, b| b.cmp(a));
    sums.iter().take(3).sum()
  }
}
