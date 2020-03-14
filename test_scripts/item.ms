global x: u32;
global y: s32 = 9990;


fn fib (n: s32) -> s32 {
  if n <= 1 {
    n
  } else {
    fib(n - 1) + fib(n - 2)
  }
}

fn add (a: s32, b: s32) -> s32 {
  a + b
}

fn subrt () {
  print(x % 3, y * 2);
}

fn noret (a: s32) {
  print(a)
}