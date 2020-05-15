global X: s32 = get_y() + 1;
global Y: s32 = 99;

fn get_y -> s32 {
  Y
}

global A: s32 = get_a_plus_one() + 1;

fn get_a_plus_one -> s32 { get_a() + 1 }
fn get_a -> s32 { A }

global use_import: s32 = GlobalDef::X + 1;

import GlobalDef;