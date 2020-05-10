mod X {
  export global A: s32;
}

fn set_a (new_value: s32) {
  X::A = new_value;
}

fn get_a () -> s32 {
  X::A
}

fn test_get_set -> bool {
  let my_a = 99;
  set_a(my_a + 1);
  my_a == get_a() - 1
}

fn test_branch -> bool {
  if X::A == 99 {
    X::A
  } else {
    5
  }
}