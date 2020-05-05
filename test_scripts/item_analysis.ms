mod A {
  import { X, Y, Z } from ::B::C::D;
  export { X, Y, Z }
}

export mod B {
  import C from ::;
  export C;
}

export mod C {
  export mod D {
    import s32 as X from core;
    export X;
    export fn Y { }
    export global Z: s32 = 99;
  }
}

import X from A;
export X as Y;
export s32 as i32;