//! Analysis functions and callgraph generation

use std::iter::repeat;

use crate::types::{FunPtr, Reg};
use crate::{Bytecode, Function, Native, Opcode, RefFun, RefType, Resolve, Type, TypeObj};

#[cfg(feature = "graph")]
pub mod graph;

pub mod files;
pub mod usage;

impl Bytecode {
    /// Iterate on every instruction of every function
    pub fn ops(&self) -> impl Iterator<Item = (&Function, (usize, &Opcode))> {
        self.functions
            .iter()
            .flat_map(|f| repeat(f).zip(f.ops.iter().enumerate()))
    }
}

impl RefFun {
    /// return true if the function is from the standard library
    pub fn is_from_std(&self, code: &Bytecode) -> bool {
        match code.get(*self) {
            FunPtr::Fun(fun) => fun.is_from_std(code),
            FunPtr::Native(n) => n.is_from_std(code),
        }
    }
}

impl Function {
    /// return true if the function is from the standard library
    pub fn is_from_std(&self, code: &Bytecode) -> bool {
        if let Some(debug_info) = &self.debug_info {
            // We look at the Ret opcode which is probably not from inlined code.
            let (file, _) = debug_info[self.ops.len() - 1];
            let filename = &code.debug_files.as_ref().unwrap()[file];
            filename.contains("std")
        } else {
            false
        }
    }

    /// Find any outbound references to other functions in a function
    pub fn find_fun_refs(&self) -> impl Iterator<Item = (usize, &Opcode, RefFun)> + '_ {
        // TODO method calls (CallMethod & CallThis)
        self.ops.iter().enumerate().filter_map(|(i, o)| match o {
            // Direct call
            Opcode::Call0 { fun, .. } => Some((i, o, *fun)),
            Opcode::Call1 { fun, .. } => Some((i, o, *fun)),
            Opcode::Call2 { fun, .. } => Some((i, o, *fun)),
            Opcode::Call3 { fun, .. } => Some((i, o, *fun)),
            Opcode::Call4 { fun, .. } => Some((i, o, *fun)),
            Opcode::CallN { fun, .. } => Some((i, o, *fun)),
            // Reference through closure
            Opcode::StaticClosure { fun, .. } => Some((i, o, *fun)),
            Opcode::InstanceClosure { fun, .. } => Some((i, o, *fun)),
            _ => None,
        })
    }

    /// Find any outbound references to other elements in a function
    pub fn find_elem_refs(&self, elem_idx: usize) -> impl Iterator<Item = (usize, &Opcode)> + '_ {
        self.ops.iter().enumerate().filter_map(move |(i, o)| match o {
            Opcode::Type { ty, .. } if ty.0 == elem_idx => Some((i, o)),
            
            Opcode::Field { field, .. } if field.0 == elem_idx => Some((i, o)),
            Opcode::SetField { field, .. } if field.0 == elem_idx => Some((i, o)),
            Opcode::GetThis { field, .. } if field.0 == elem_idx => Some((i, o)),
            Opcode::SetThis { field, .. } if field.0 == elem_idx => Some((i, o)),
            Opcode::CallMethod { field, .. } if field.0 == elem_idx => Some((i, o)),
            Opcode::CallThis { field, .. } if field.0 == elem_idx => Some((i, o)),
            Opcode::SetEnumField { field, .. } if field.0 == elem_idx => Some((i, o)),
            Opcode::EnumField { field, .. } if field.0 == elem_idx => Some((i, o)),
            
            Opcode::MakeEnum { construct, .. } if construct.0 == elem_idx => Some((i, o)),
            Opcode::EnumAlloc { construct, .. } if construct.0 == elem_idx => Some((i, o)),
            
            Opcode::GetGlobal { global, .. } if global.0 == elem_idx => Some((i, o)),
            Opcode::SetGlobal { global, .. } if global.0 == elem_idx => Some((i, o)),
            
            Opcode::String { ptr, .. } if ptr.0 == elem_idx => Some((i, o)),
            Opcode::Bytes { ptr, .. } if ptr.0 == elem_idx => Some((i, o)),
            Opcode::Int { ptr, .. } if ptr.0 == elem_idx => Some((i, o)),
            Opcode::Float { ptr, .. } if ptr.0 == elem_idx => Some((i, o)),

            Opcode::Call0 { fun, .. } if fun.0 == elem_idx => Some((i, o)),
            Opcode::Call1 { fun, .. } if fun.0 == elem_idx => Some((i, o)),
            Opcode::Call2 { fun, .. } if fun.0 == elem_idx => Some((i, o)), 
            Opcode::Call3 { fun, .. } if fun.0 == elem_idx => Some((i, o)),
            Opcode::Call4 { fun, .. } if fun.0 == elem_idx => Some((i, o)),
            Opcode::CallN { fun, .. } if fun.0 == elem_idx => Some((i, o)),
            Opcode::StaticClosure { fun, .. } if fun.0 == elem_idx => Some((i, o)),
            Opcode::InstanceClosure { fun, .. } if fun.0 == elem_idx => Some((i, o)),

            Opcode::Prefetch { field, .. } if field.0 == elem_idx => Some((i, o)),
            
            _ => None,
        })
    }

    /// Starting from a position in a function, finds the last time a register has been assigned a closure
    pub fn find_last_closure_assign(
        &self,
        code: &Bytecode,
        reg: Reg,
        pos: usize,
    ) -> Option<RefFun> {
        self.ops
            .iter()
            .rev()
            .skip(self.ops.len() - pos)
            .find_map(|o| match *o {
                Opcode::StaticClosure { dst, fun } if dst == reg => Some(fun),
                Opcode::InstanceClosure { dst, fun, .. } if dst == reg => Some(fun),
                // We could have assigned anything through this field but we know from the register type it is a function
                Opcode::Field { dst, obj, field } if dst == reg => self
                    .regtype(obj)
                    .as_obj(code)
                    .and_then(|o| o.bindings.get(&field).copied()),
                _ => None,
            })
    }
}

impl Native {
    /// return true if the native function is from the standard library
    pub fn is_from_std(&self, code: &Bytecode) -> bool {
        code[self.lib] == "std"
    }
}

impl RefType {
    /// return true if the type is from the standard library
    pub fn is_from_std(&self, code: &Bytecode) -> bool {
        code[*self].is_from_std(code)
    }
}

impl Type {
    /// return true if the type is from the standard library.
    /// Every type is from the standard library except enums and obj where we have to check.
    pub fn is_from_std(&self, code: &Bytecode) -> bool {
        match self {
            Type::Obj(obj) => obj.is_from_std(code),
            Type::Enum { .. } => false,
            _ => true,
        }
    }
}

impl TypeObj {
    /// return true if this obj is from the standard library.
    /// Relies on an heuristic checking the name and methods.
    pub fn is_from_std(&self, code: &Bytecode) -> bool {
        if let [first, ..] = &self.protos[..] {
            first.findex.is_from_std(code)
        } else if let Some(&fun) = self.bindings.values().next() {
            fun.is_from_std(code)
        } else {
            let name = &code[self.name];
            name.starts_with("hl")
                || name.starts_with("haxe")
                || name == "Std"
                || name == "Sys"
                || name == "Type"
        }
    }
}
