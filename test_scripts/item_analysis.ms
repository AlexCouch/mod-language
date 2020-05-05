mod A {
  import ::B::C::D;
  import {
    D::X,
    D::Y,
    D::Z,
  }
  export { X, Y, Z }
}

export mod B {
  import ::C;
  export C;
}

export mod C {
  export mod D {
    import core::s32 as X;
    export X;
    export fn Y { }
    export global Z: s32 = 99;
  }
}

import A::X;
export X as Y;
export s32 as i32;