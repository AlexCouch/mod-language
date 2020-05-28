export fn factorial (n: s32) -> s32 {
  if n < 2 { n }
  else { n * factorial(n - 1)}
}