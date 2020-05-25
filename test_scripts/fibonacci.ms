export fn fibonacci (n: s32) -> s32 {
  if n < 2 {
    n
  } else {
    fibonacci(n - 1) + fibonacci(n - 2)
  }
}