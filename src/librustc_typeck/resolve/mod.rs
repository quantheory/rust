// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Resolution of associated items when they can't be handled in `rustc_resolve`
//! because they require trait lookup. UFCS <T>::Foo and T::Foo are handled
//! here, and `probe` is also used for method calls.

use super::CrateCtxt;

use astconv::AstConv;
use middle::def;
use middle::infer;
use middle::privacy::{AllPublic, DependsOn, LastPrivate, LastMod};
use middle::subst;
use middle::ty::{self, ImplOrTraitItem, Ty};
use syntax::ast;
use syntax::codemap::Span;

use std::rc::Rc;

pub use self::ResolveError::*;

pub mod probe;
pub mod suggest;

pub enum ResolveError {
    // Did not find an applicable item, but we did find various
    // static methods that may apply, as well as a list of
    // not-in-scope traits which may work.
    NoMatch(Vec<CandidateSource>, Vec<ast::DefId>, probe::Mode),

    // Multiple items might apply.
    Ambiguity(Vec<CandidateSource>),

    // Using a `Fn`/`FnMut`/etc method on a raw closure type before we have inferred its kind.
    ClosureAmbiguity(/* DefId of fn trait */ ast::DefId),
}

// A pared down enum describing just the places from which a
// candidate can arise. Used for error reporting only.
#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum CandidateSource {
    ImplSource(ast::DefId),
    TraitSource(/* trait id */ ast::DefId),
}

type ItemIndex = usize; // just for doc purposes

// Abstraction handling the difference between `collect` and later stages.
// This is slightly embarrassing in that several of these functions really
// don't make sense for `collect` anyway, but are needed in function bodies
// nonetheless.
pub trait ResolveCtxt<'a, 'tcx>: AstConv<'tcx> {
    fn ccx(&self) -> &CrateCtxt<'a, 'tcx>;

    fn infcx(&self) -> &infer::InferCtxt<'a, 'tcx>;

    // Return copies of any predicates which constrain type parameters in the
    // current scope to implement specific traits.
    fn trait_predicates(&self) -> Vec<ty::Predicate<'tcx>>;

    // Create the steps for a method call, or the single "step" for UFCS
    // resolution.
    fn create_steps(&self, Span, Ty<'tcx>, probe::Mode)
                    -> Option<Vec<probe::CandidateStep<'tcx>>>;

    // If the trait given is a `Fn` trait, pick out matching closures in the
    // vector of steps.
    fn filter_closure_steps(&self, trait_def_id: ast::DefId,
                            steps: Rc<Vec<probe::CandidateStep<'tcx>>>)
                            -> Result<Vec<probe::CandidateStep<'tcx>>, ResolveError>;

    // Get the type of an impl and generate substitutions with placeholders.
    // The type is expected to have any associated types normalized.
    fn impl_ty_and_substs(&self, span: Span, impl_def_id: ast::DefId)
                          -> (Ty<'tcx>, subst::Substs<'tcx>);

    // Within this context, check that obligations may be satisfied.
    fn check_impl_obligations(&self, span: Span, impl_def_id: ast::DefId,
                              substs: &subst::Substs<'tcx>) -> bool;
}

pub fn resolve_ufcs<'a, 'tcx: 'a, T>(rcx: &'a T,
                                     span: Span,
                                     item_name: ast::Name,
                                     self_ty: Ty<'tcx>,
                                     expr_id: ast::NodeId)
                                     -> Result<(def::Def, LastPrivate), ResolveError>
    where T: ResolveCtxt<'a, 'tcx> + 'a
{
    let mode = probe::Mode::Path;
    let pick = try!(probe::probe(rcx, span, mode, item_name, self_ty, expr_id));
    let def_id = pick.item.def_id();
    let mut lp = LastMod(AllPublic);
    let provenance = match pick.kind {
        probe::InherentImplPick(impl_def_id) => {
            if pick.item.vis() != ast::Public {
                lp = LastMod(DependsOn(def_id));
            }
            def::FromImpl(impl_def_id)
        }
        _ => def::FromTrait(pick.item.container().id())
    };
    let def_result = match pick.item {
        ImplOrTraitItem::MethodTraitItem(..) => def::DefMethod(def_id, provenance),
        ImplOrTraitItem::ConstTraitItem(..) => def::DefAssociatedConst(def_id, provenance),
        ImplOrTraitItem::TypeTraitItem(..) => {
            rcx.tcx().sess
               .span_bug(span, "resolve_ufcs: probe picked associated type");
        }
    };
    Ok((def_result, lp))
}
