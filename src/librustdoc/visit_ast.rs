// Copyright 2012-2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Rust AST Visitor. Extracts useful information and massages it into a form
//! usable for clean

use std::collections::HashSet;
use std::mem;

use syntax::abi;
use syntax::ast;
use syntax::attr;
use syntax::attr::AttrMetaMethods;
use syntax::codemap::Span;

use rustc::front::map as hir_map;
use rustc::middle::def_id::DefId;
use rustc::middle::stability;

use rustc_front::hir;

use core;
use doctree::*;

// looks to me like the first two of these are actually
// output parameters, maybe only mutated once; perhaps
// better simply to have the visit method return a tuple
// containing them?

// also, is there some reason that this doesn't use the 'visit'
// framework from syntax?

pub struct RustdocVisitor<'a, 'tcx: 'a> {
    pub module: Module,
    pub attrs: Vec<ast::Attribute>,
    pub cx: &'a core::DocContext<'a, 'tcx>,
    pub analysis: Option<&'a core::CrateAnalysis>,
    view_item_stack: HashSet<ast::NodeId>,
    inlining_from_glob: bool,
}

impl<'a, 'tcx> RustdocVisitor<'a, 'tcx> {
    pub fn new(cx: &'a core::DocContext<'a, 'tcx>,
               analysis: Option<&'a core::CrateAnalysis>) -> RustdocVisitor<'a, 'tcx> {
        // If the root is reexported, terminate all recursion.
        let mut stack = HashSet::new();
        stack.insert(ast::CRATE_NODE_ID);
        RustdocVisitor {
            module: Module::new(None),
            attrs: Vec::new(),
            cx: cx,
            analysis: analysis,
            view_item_stack: stack,
            inlining_from_glob: false,
        }
    }

    fn stability(&self, id: ast::NodeId) -> Option<attr::Stability> {
        self.cx.tcx_opt().and_then(
            |tcx| stability::lookup(tcx, DefId::local(id)).map(|x| x.clone()))
    }

    pub fn visit(&mut self, krate: &hir::Crate) {
        self.attrs = krate.attrs.clone();

        self.module = self.visit_mod_contents(krate.span,
                                              krate.attrs.clone(),
                                              hir::Public,
                                              ast::CRATE_NODE_ID,
                                              &krate.module,
                                              None);
        // attach the crate's exported macros to the top-level module:
        self.module.macros = krate.exported_macros.iter()
            .map(|def| self.visit_macro(def)).collect();
        self.module.is_crate = true;
    }

    pub fn visit_struct_def(&mut self, item: &hir::Item,
                            name: ast::Ident, sd: &hir::StructDef,
                            generics: &hir::Generics) -> Struct {
        debug!("Visiting struct");
        let struct_type = struct_type_from_def(&*sd);
        Struct {
            id: item.id,
            struct_type: struct_type,
            name: name,
            vis: item.vis,
            stab: self.stability(item.id),
            attrs: item.attrs.clone(),
            generics: generics.clone(),
            fields: sd.fields.clone(),
            whence: item.span
        }
    }

    pub fn visit_enum_def(&mut self, it: &hir::Item,
                          name: ast::Ident, def: &hir::EnumDef,
                          params: &hir::Generics) -> Enum {
        debug!("Visiting enum");
        Enum {
            name: name,
            variants: def.variants.iter().map(|v| Variant {
                name: v.node.name,
                attrs: v.node.attrs.clone(),
                stab: self.stability(v.node.id),
                id: v.node.id,
                kind: v.node.kind.clone(),
                whence: v.span,
            }).collect(),
            vis: it.vis,
            stab: self.stability(it.id),
            generics: params.clone(),
            attrs: it.attrs.clone(),
            id: it.id,
            whence: it.span,
        }
    }

    pub fn visit_fn(&mut self, item: &hir::Item,
                    name: ast::Ident, fd: &hir::FnDecl,
                    unsafety: &hir::Unsafety,
                    constness: hir::Constness,
                    abi: &abi::Abi,
                    gen: &hir::Generics) -> Function {
        debug!("Visiting fn");
        Function {
            id: item.id,
            vis: item.vis,
            stab: self.stability(item.id),
            attrs: item.attrs.clone(),
            decl: fd.clone(),
            name: name,
            whence: item.span,
            generics: gen.clone(),
            unsafety: *unsafety,
            constness: constness,
            abi: *abi,
        }
    }

    pub fn visit_mod_contents(&mut self, span: Span, attrs: Vec<ast::Attribute> ,
                              vis: hir::Visibility, id: ast::NodeId,
                              m: &hir::Mod,
                              name: Option<ast::Ident>) -> Module {
        let mut om = Module::new(name);
        om.where_outer = span;
        om.where_inner = m.inner;
        om.attrs = attrs;
        om.vis = vis;
        om.stab = self.stability(id);
        om.id = id;
        for i in &m.items {
            self.visit_item(&**i, None, &mut om);
        }
        om
    }

    fn visit_view_path(&mut self, path: hir::ViewPath_,
                       om: &mut Module,
                       id: ast::NodeId,
                       please_inline: bool) -> Option<hir::ViewPath_> {
        match path {
            hir::ViewPathSimple(dst, base) => {
                if self.resolve_id(id, Some(dst), false, om, please_inline) {
                    None
                } else {
                    Some(hir::ViewPathSimple(dst, base))
                }
            }
            hir::ViewPathList(p, paths) => {
                let mine = paths.into_iter().filter(|path| {
                    !self.resolve_id(path.node.id(), None, false, om,
                                     please_inline)
                }).collect::<Vec<hir::PathListItem>>();

                if mine.is_empty() {
                    None
                } else {
                    Some(hir::ViewPathList(p, mine))
                }
            }

            // these are feature gated anyway
            hir::ViewPathGlob(base) => {
                if self.resolve_id(id, None, true, om, please_inline) {
                    None
                } else {
                    Some(hir::ViewPathGlob(base))
                }
            }
        }

    }

    fn resolve_id(&mut self, id: ast::NodeId, renamed: Option<ast::Ident>,
                  glob: bool, om: &mut Module, please_inline: bool) -> bool {
        let tcx = match self.cx.tcx_opt() {
            Some(tcx) => tcx,
            None => return false
        };
        let def = tcx.def_map.borrow()[&id].def_id();
        if !def.is_local() { return false }
        let analysis = match self.analysis {
            Some(analysis) => analysis, None => return false
        };
        if !please_inline && analysis.public_items.contains(&def.node) {
            return false
        }
        if !self.view_item_stack.insert(def.node) { return false }

        let ret = match tcx.map.get(def.node) {
            hir_map::NodeItem(it) => {
                if glob {
                    let prev = mem::replace(&mut self.inlining_from_glob, true);
                    match it.node {
                        hir::ItemMod(ref m) => {
                            for i in &m.items {
                                self.visit_item(&**i, None, om);
                            }
                        }
                        hir::ItemEnum(..) => {}
                        _ => { panic!("glob not mapped to a module or enum"); }
                    }
                    self.inlining_from_glob = prev;
                } else {
                    self.visit_item(it, renamed, om);
                }
                true
            }
            _ => false,
        };
        self.view_item_stack.remove(&id);
        return ret;
    }

    pub fn visit_item(&mut self, item: &hir::Item,
                      renamed: Option<ast::Ident>, om: &mut Module) {
        debug!("Visiting item {:?}", item);
        let name = renamed.unwrap_or(item.ident);
        match item.node {
            hir::ItemExternCrate(ref p) => {
                let path = match *p {
                    None => None,
                    Some(x) => Some(x.to_string()),
                };
                om.extern_crates.push(ExternCrate {
                    name: name,
                    path: path,
                    vis: item.vis,
                    attrs: item.attrs.clone(),
                    whence: item.span,
                })
            }
            hir::ItemUse(ref vpath) => {
                let node = vpath.node.clone();
                let node = if item.vis == hir::Public {
                    let please_inline = item.attrs.iter().any(|item| {
                        match item.meta_item_list() {
                            Some(list) => {
                                list.iter().any(|i| &i.name()[..] == "inline")
                            }
                            None => false,
                        }
                    });
                    match self.visit_view_path(node, om, item.id, please_inline) {
                        None => return,
                        Some(p) => p
                    }
                } else {
                    node
                };
                om.imports.push(Import {
                    id: item.id,
                    vis: item.vis,
                    attrs: item.attrs.clone(),
                    node: node,
                    whence: item.span,
                });
            }
            hir::ItemMod(ref m) => {
                om.mods.push(self.visit_mod_contents(item.span,
                                                     item.attrs.clone(),
                                                     item.vis,
                                                     item.id,
                                                     m,
                                                     Some(name)));
            },
            hir::ItemEnum(ref ed, ref gen) =>
                om.enums.push(self.visit_enum_def(item, name, ed, gen)),
            hir::ItemStruct(ref sd, ref gen) =>
                om.structs.push(self.visit_struct_def(item, name, &**sd, gen)),
            hir::ItemFn(ref fd, ref unsafety, constness, ref abi, ref gen, _) =>
                om.fns.push(self.visit_fn(item, name, &**fd, unsafety,
                                          constness, abi, gen)),
            hir::ItemTy(ref ty, ref gen) => {
                let t = Typedef {
                    ty: ty.clone(),
                    gen: gen.clone(),
                    name: name,
                    id: item.id,
                    attrs: item.attrs.clone(),
                    whence: item.span,
                    vis: item.vis,
                    stab: self.stability(item.id),
                };
                om.typedefs.push(t);
            },
            hir::ItemStatic(ref ty, ref mut_, ref exp) => {
                let s = Static {
                    type_: ty.clone(),
                    mutability: mut_.clone(),
                    expr: exp.clone(),
                    id: item.id,
                    name: name,
                    attrs: item.attrs.clone(),
                    whence: item.span,
                    vis: item.vis,
                    stab: self.stability(item.id),
                };
                om.statics.push(s);
            },
            hir::ItemConst(ref ty, ref exp) => {
                let s = Constant {
                    type_: ty.clone(),
                    expr: exp.clone(),
                    id: item.id,
                    name: name,
                    attrs: item.attrs.clone(),
                    whence: item.span,
                    vis: item.vis,
                    stab: self.stability(item.id),
                };
                om.constants.push(s);
            },
            hir::ItemTrait(unsafety, ref gen, ref b, ref items) => {
                let t = Trait {
                    unsafety: unsafety,
                    name: name,
                    items: items.clone(),
                    generics: gen.clone(),
                    bounds: b.iter().cloned().collect(),
                    id: item.id,
                    attrs: item.attrs.clone(),
                    whence: item.span,
                    vis: item.vis,
                    stab: self.stability(item.id),
                };
                om.traits.push(t);
            },
            hir::ItemImpl(unsafety, polarity, ref gen, ref tr, ref ty, ref items) => {
                let i = Impl {
                    unsafety: unsafety,
                    polarity: polarity,
                    generics: gen.clone(),
                    trait_: tr.clone(),
                    for_: ty.clone(),
                    items: items.clone(),
                    attrs: item.attrs.clone(),
                    id: item.id,
                    whence: item.span,
                    vis: item.vis,
                    stab: self.stability(item.id),
                };
                // Don't duplicate impls when inlining glob imports, we'll pick
                // them up regardless of where they're located.
                if !self.inlining_from_glob {
                    om.impls.push(i);
                }
            },
            hir::ItemDefaultImpl(unsafety, ref trait_ref) => {
                let i = DefaultImpl {
                    unsafety: unsafety,
                    trait_: trait_ref.clone(),
                    id: item.id,
                    attrs: item.attrs.clone(),
                    whence: item.span,
                };
                // see comment above about ItemImpl
                if !self.inlining_from_glob {
                    om.def_traits.push(i);
                }
            }
            hir::ItemForeignMod(ref fm) => {
                om.foreigns.push(fm.clone());
            }
        }
    }

    // convert each exported_macro into a doc item
    fn visit_macro(&self, def: &hir::MacroDef) -> Macro {
        Macro {
            id: def.id,
            attrs: def.attrs.clone(),
            name: def.ident,
            whence: def.span,
            stab: self.stability(def.id),
            imported_from: def.imported_from,
        }
    }
}
