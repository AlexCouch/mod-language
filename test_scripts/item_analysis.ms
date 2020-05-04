mod A {
  import X from ::B::C::D;
  export X;
}

export mod B {
  import C from ::;
  export C;
}

export mod C {
  export mod D {
    export fn X { }
  }
}

import X from A;
export X;