ns A {
  alias ::B::C::D;
  alias {
    D::X,
    D::Y,
  }
  export { X, Y, D::Z }
}

export ns B {
  export ::C;
}

export ns C {
  export ns D {
    alias core::s32 as X;
    export X;
    export fn Y { }
    export global Z: s32 = 99;
  }
}

alias A::X;
export X as Y;
export s32 as i32;