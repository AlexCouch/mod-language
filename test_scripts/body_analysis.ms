ns X {
  export global A: s32;
}

fn set_a (new_value: s32) {
  X::A = new_value;
}

fn get_a () -> s32 {
  X::A
}

export fn test_get_set -> bool {
  let my_a = 99;
  set_a(my_a + 1);
  my_a == get_a() - 1
}

fn deref_and_add (ptr: ^s32) -> s32 {
  @ptr + 10
}

export fn spill () -> s32 {
  deref_and_add(^32)
}

export fn test_branch -> bool {
  if X::A == 99 {
    X::A
  } else {
    5
  }
}

export fn ret_true -> bool {
  true
}

export fn ret_false -> bool {
  false
}

export fn ret_void_ptr -> ^void {
  null
}

export fn ret_coerce_ptr -> ^s32 {
  null
}