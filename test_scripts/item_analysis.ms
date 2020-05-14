ns U { }

ns A {
  alias ::B::C::D;
  alias {
    D::X,
    D::Y,
  }
  ns L {
    export fn F { }
  }
  export L::F;
  export { X, Y, D::Z }
  export ns H { }
}

export ns B {
  export type S32PtrPtr: ^::C::D::S32Ptr;
  export ::C;
}

export ns C {
  export ns D {
    alias core::s32 as X;
    export X;
    export fn Y { }
    export global Z: s32 = 99;
    export type S32Ptr: ^s32;
    export struct MyStruct {
      a: S32Ptr,
      b: ::B::S32PtrPtr,
    }
  }
}

alias A::X;
export X as Y;
export s32 as i32;
export C::D::MyStruct;
export { A::H, A::F }