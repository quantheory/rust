// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::ops::AddAssign;
//~^ error: use of unstable library feature 'op_assign_traits'

struct Int(i32);

impl AddAssign for Int {
    //~^ error: use of unstable library feature 'op_assign_traits'
    fn add_assign(&mut self, _: Int) {
        //~^ error: use of unstable library feature 'op_assign_traits'
        unimplemented!()
    }
}

fn main() {}
