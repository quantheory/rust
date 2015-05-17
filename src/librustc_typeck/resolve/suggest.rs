// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use CrateCtxt;

use middle::def;
use metadata::{csearch, cstore, decoder};
use syntax::{ast, ast_util};

use std::cell;
use std::cmp::Ordering;

/// Retrieve all traits in this crate and any dependent crates.
pub fn all_traits<'a>(ccx: &'a CrateCtxt) -> AllTraits<'a> {
    if ccx.all_traits.borrow().is_none() {
        use syntax::visit;

        let mut traits = vec![];

        // Crate-local:
        //
        // meh.
        struct Visitor<'a> {
            traits: &'a mut AllTraitsVec,
        }
        impl<'v, 'a> visit::Visitor<'v> for Visitor<'a> {
            fn visit_item(&mut self, i: &'v ast::Item) {
                match i.node {
                    ast::ItemTrait(..) => {
                        self.traits.push(TraitInfo::new(ast_util::local_def(i.id)));
                    }
                    _ => {}
                }
                visit::walk_item(self, i)
            }
        }
        visit::walk_crate(&mut Visitor {
            traits: &mut traits
        }, ccx.tcx.map.krate());

        // Cross-crate:
        fn handle_external_def(traits: &mut AllTraitsVec,
                               ccx: &CrateCtxt,
                               cstore: &cstore::CStore,
                               dl: decoder::DefLike) {
            match dl {
                decoder::DlDef(def::DefTrait(did)) => {
                    traits.push(TraitInfo::new(did));
                }
                decoder::DlDef(def::DefMod(did)) => {
                    csearch::each_child_of_item(cstore, did, |dl, _, _| {
                        handle_external_def(traits, ccx, cstore, dl)
                    })
                }
                _ => {}
            }
        }
        let cstore = &ccx.tcx.sess.cstore;
        cstore.iter_crate_data(|cnum, _| {
            csearch::each_top_level_item_of_crate(cstore, cnum, |dl, _, _| {
                handle_external_def(&mut traits, ccx, cstore, dl)
            })
        });

        *ccx.all_traits.borrow_mut() = Some(traits);
    }

    let borrow = ccx.all_traits.borrow();
    assert!(borrow.is_some());
    AllTraits {
        borrow: borrow,
        idx: 0
    }
}

#[derive(Copy, Clone)]
pub struct TraitInfo {
    pub def_id: ast::DefId,
}

impl TraitInfo {
    fn new(def_id: ast::DefId) -> TraitInfo {
        TraitInfo {
            def_id: def_id,
        }
    }
}
impl PartialEq for TraitInfo {
    fn eq(&self, other: &TraitInfo) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}
impl Eq for TraitInfo {}
impl PartialOrd for TraitInfo {
    fn partial_cmp(&self, other: &TraitInfo) -> Option<Ordering> { Some(self.cmp(other)) }
}
impl Ord for TraitInfo {
    fn cmp(&self, other: &TraitInfo) -> Ordering {
        // accessible traits are more important/relevant than
        // inaccessible ones, local crates are more important than
        // remote ones (local: cnum == 0), and NodeIds just for
        // totality.

        let lhs = (other.def_id.krate, other.def_id.node);
        let rhs = (self.def_id.krate, self.def_id.node);
        lhs.cmp(&rhs)
    }
}

pub type AllTraitsVec = Vec<TraitInfo>;

pub struct AllTraits<'a> {
    borrow: cell::Ref<'a, Option<AllTraitsVec>>,
    idx: usize
}

impl<'a> Iterator for AllTraits<'a> {
    type Item = TraitInfo;

    fn next(&mut self) -> Option<TraitInfo> {
        let AllTraits { ref borrow, ref mut idx } = *self;
        // ugh.
        borrow.as_ref().unwrap().get(*idx).map(|info| {
            *idx += 1;
            *info
        })
    }
}
