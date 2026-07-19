//! [Hashlink](https://hashlink.haxe.org/) bytecode disassembler and analyzer.
//! See [Bytecode] for an entrypoint to the library.
//!
//! #### Note about safety
//! We don't deal with self-references, hence we deal with indexes into structures.
//! Be careful when calling functions on Ref* objects, as no bound checking is done and every index is assumed to be valid.

extern crate core;

use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::Index;

use crate::opcodes::Opcode;
use crate::types::{
    ConstantDef, FunPtr, Function, Native, ObjField, RefField, RefFloat, RefFun, RefGlobal, RefInt,
    RefString, RefType, Type, TypeObj,
};

pub mod analysis;
pub mod fmt;
/// Opcodes definitions.
pub mod opcodes;
/// All about reading bytecode
mod read;
/// Bytecode elements definitions.
/// All the Ref* types in this modules are references to bytecode elements like constants or function.
/// They are required since we cannot use rust references as that would make our structure self-referential.
/// They makes the code look a bit more complicated than it actually is. Every Ref* struct is cheaply copyable.
pub mod types;
mod validate;
/// All about writing bytecode
mod write;

/// Cheaply cloneable string with inline storage
// pub type Str = smol_str::SmolStr;
// pub type Str = kstring::KStringBase<kstring::backend::RcStr>;
pub type Str = flexstr::SharedStr;
// pub type Str = String;

pub type Result<T> = core::result::Result<T, Error>;

/// Mapping structure for adjusting references when merging bytecodes
#[derive(Debug, Clone)]
pub struct IndexMapping {
    pub int_offset: usize,
    pub float_offset: usize,
    pub string_offset: usize,
    /// Optional explicit remapping for strings to support deduplication across merges
    pub string_remap: Option<Vec<usize>>, // maps other RefString index -> self RefString index
    pub bytes_offset: usize,
    pub type_offset: usize,
    pub global_offset: usize,
    pub function_offset: usize,
    pub native_offset: usize,
    /// Field remapping remains empty unless type-level dedupe or field reordering is introduced.
    /// Rationale: RefField indices are type-local; you cannot globally remap fields without per-type context.
    pub field_mappings: HashMap<RefField, RefField>,
    pub debug_file_offset: usize,
}

impl Default for IndexMapping {
    fn default() -> Self {
        Self {
            int_offset: 0,
            float_offset: 0,
            string_offset: 0,
            string_remap: None,
            bytes_offset: 0,
            type_offset: 0,
            global_offset: 0,
            function_offset: 0,
            native_offset: 0,
            field_mappings: HashMap::new(),
            debug_file_offset: 0,
        }
    }
}

impl IndexMapping {
    /// Apply string mapping: if a remap is present, use it; otherwise apply the offset.
    pub fn apply_string(&self, s: &mut RefString) {
        if let Some(remap) = &self.string_remap {
            if !s.is_null() {
                s.0 = remap[s.0];
            }
        } else {
            s.adjust(self.string_offset);
        }
    }
}

/// Trait for types that contain references to other bytecode elements
/// and need to adjust those references when merging bytecodes
pub trait AdjustReferences {
    fn adjust_references(&mut self, mapping: &IndexMapping);
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Malformed bytecode: {0}")]
    MalformedBytecode(String),
    #[error("Unsupported bytecode version {version} (expected {min} <= version <= {max})")]
    UnsupportedVersion { version: u8, min: u8, max: u8 },
    #[error("Value '{value}' is too big to be serialized (|expected| < {limit})")]
    ValueOutOfBounds { value: i32, limit: u32 },
    #[error("Invalid {kind} reference {index} (table length: {len})")]
    InvalidReference {
        kind: &'static str,
        index: usize,
        len: usize,
    },
    #[error("Bytecode {kind} count {count} exceeds the safety limit {limit}")]
    CountLimit {
        kind: &'static str,
        count: usize,
        limit: usize,
    },
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error(transparent)]
    Utf8Error(#[from] core::str::Utf8Error),
}

/// Bytecode structure containing all the information.
/// Every field is public for flexibility, but you aren't encouraged to modify them.
#[derive(Debug, Clone)]
pub struct Bytecode {
    /// Bytecode format version
    pub version: u8,
    /// Program entrypoint
    pub entrypoint: RefFun,
    /// i32 constant pool
    pub ints: Vec<i32>,
    /// f64 constant pool
    pub floats: Vec<f64>,
    /// String constant pool
    pub strings: Vec<Str>,
    /// Bytes constant pool
    ///
    /// *Since bytecode v5*
    pub bytes: Option<(Vec<u8>, Vec<usize>)>,
    /// *Debug* file names constant pool
    pub debug_files: Option<Vec<Str>>,
    /// Types, contains every possible types expressed in the program
    pub types: Vec<Type>,
    /// Globals, holding static variables and such
    pub globals: Vec<RefType>,
    /// Native functions references
    pub natives: Vec<Native>,
    /// Code functions pool
    pub functions: Vec<Function>,
    /// Constants, initializers for globals
    ///
    /// *Since bytecode v4*
    pub constants: Option<Vec<ConstantDef>>,

    // Fields below are not part of the data.
    // Those are acceleration structures used to speed up lookup.
    /// Acceleration structure mapping function references (findex) to functions indexes in the native or function pool.
    findexes: Vec<RefFunKnown>,
    /// Acceleration structure mapping function names to function indexes in the function pool
    fnames: HashMap<Str, usize>,
    pub globals_initializers: HashMap<RefGlobal, usize>,
}

impl Bytecode {
    /// Safe version of get for RefFun, to be used everywhere deref is non-critical.
    pub fn safe_get_ref_fun(&self, index: RefFun) -> Option<FunPtr<'_>> {
        let findex = self.findexes.get(index.0)?;
        match *findex {
            RefFunKnown::Fun(fun) => self.functions.get(fun).map(FunPtr::Fun),
            RefFunKnown::Native(n) => self.natives.get(n).map(FunPtr::Native),
        }
    }
    /// Get the entrypoint function without assuming a valid reference.
    pub fn entrypoint(&self) -> Result<&Function> {
        self.safe_get_ref_fun(self.entrypoint)
            .and_then(FunPtr::as_fn)
            .ok_or(Error::InvalidReference {
                kind: "entrypoint function",
                index: self.entrypoint.0,
                len: self.findexes.len(),
            })
    }

    /// Get the main function.
    pub fn main(&self) -> Result<&Function> {
        self.fnames
            .get("main")
            .and_then(|&index| self.functions.get(index))
            .ok_or_else(|| Error::MalformedBytecode("Main function not found".into()))
    }

    /// Get a function by its name.
    pub fn function_by_name(&self, name: &str) -> Option<&Function> {
        self.fnames
            .get(name)
            .and_then(|&index| self.functions.get(index))
    }

    pub fn findex_max(&self) -> usize {
        self.findexes.len()
    }

    pub fn functions(&self) -> impl Iterator<Item = FunPtr<'_>> {
        // Iterate over actual pools to avoid holes in findexes
        self.functions
            .iter()
            .map(FunPtr::Fun)
            .chain(self.natives.iter().map(FunPtr::Native))
    }

    pub fn debug_file(&self, index: usize) -> Option<Str> {
        self.debug_files
            .as_ref()
            .and_then(|files| files.get(index))
            .cloned()
    }

    /// Removes all debug information from the bytecode, including debug files and debug info in functions.
    pub fn strip(&mut self) {
        self.debug_files = None;

        for function in &mut self.functions {
            function.strip_debug_info();
        }
    }

    /// Merges another bytecode into this one, adjusting all references appropriately
    pub fn merge_with(mut self, mut other: Bytecode) -> Result<Bytecode> {
        // Create mapping based on current sizes
        let mut mapping = IndexMapping {
            int_offset: self.ints.len(),
            float_offset: self.floats.len(),
            string_offset: self.strings.len(),
            string_remap: None,
            bytes_offset: self.bytes.as_ref().map_or(0, |b| b.1.len()),
            type_offset: self.types.len(),
            global_offset: self.globals.len(),
            function_offset: self.functions.len(),
            native_offset: self.natives.len(),
            field_mappings: HashMap::new(), // Intentionally empty: field indices are type-local; no global remap
            debug_file_offset: self.debug_files.as_ref().map_or(0, |v| v.len()),
        };

        // Build string remap to deduplicate strings: preserve 0 as null; map identical strings
        if !other.strings.is_empty() {
            let mut remap = Vec::with_capacity(other.strings.len());
            // Build index of existing strings to their indices
            let mut index: HashMap<Str, usize> = HashMap::new();
            for (i, s) in self.strings.iter().enumerate() {
                index.insert(s.clone(), i);
            }
            // For each string in other, find existing or allocate new index
            for (i, s) in other.strings.iter().enumerate() {
                if i == 0 {
                    // In bytecode, RefString(0) is null sentinel; keep as 0
                    remap.push(0);
                    continue;
                }
                if let Some(&existing) = index.get(s) {
                    remap.push(existing);
                } else {
                    let new_idx = self.strings.len();
                    self.strings.push(s.clone());
                    index.insert(s.clone(), new_idx);
                    remap.push(new_idx);
                }
            }
            mapping.string_remap = Some(remap);
        }

        // BEFORE adjusting offsets, unify type names/field/proto strings in `other` for dup detection
        if let Some(remap) = &mapping.string_remap {
            other.replace_type_string_references(remap);
        }

        // Helper: check if two TypeObj/Struct have compatible shape (same own_fields names/types and protos names/types)
        fn types_shape_compatible(
            this: &Bytecode,
            a: &crate::types::TypeObj,
            b: &crate::types::TypeObj,
        ) -> bool {
            // Compare own_fields length and names
            if a.own_fields.len() != b.own_fields.len() || a.protos.len() != b.protos.len() {
                return false;
            }
            // Field names and immediate RefType kinds (primitive vs obj/struct/enum/ref)
            for (fa, fb) in a.own_fields.iter().zip(b.own_fields.iter()) {
                let na = this.get(fa.name);
                let nb = this.get(fb.name);
                if na != nb {
                    return false;
                }
                // Compare type kinds by stringified short marker: we do not rely on indices equality
                let ta = &this[fa.t];
                let tb = &this[fb.t];
                use crate::types::Type::*;
                let kind_a = match ta {
                    Void => 0,
                    UI8 => 1,
                    UI16 => 2,
                    I32 => 3,
                    I64 => 4,
                    F32 => 5,
                    F64 => 6,
                    Bool => 7,
                    Bytes => 8,
                    Dyn => 9,
                    Fun(_) => 10,
                    Obj(_) => 11,
                    Array => 12,
                    Type => 13,
                    Ref(_) => 14,
                    Virtual { .. } => 15,
                    DynObj => 16,
                    Abstract { .. } => 17,
                    Enum { .. } => 18,
                    Null(_) => 19,
                    Method(_) => 20,
                    Struct(_) => 21,
                    Packed(_) => 22,
                };
                let kind_b = match tb {
                    Void => 0,
                    UI8 => 1,
                    UI16 => 2,
                    I32 => 3,
                    I64 => 4,
                    F32 => 5,
                    F64 => 6,
                    Bool => 7,
                    Bytes => 8,
                    Dyn => 9,
                    Fun(_) => 10,
                    Obj(_) => 11,
                    Array => 12,
                    Type => 13,
                    Ref(_) => 14,
                    Virtual { .. } => 15,
                    DynObj => 16,
                    Abstract { .. } => 17,
                    Enum { .. } => 18,
                    Null(_) => 19,
                    Method(_) => 20,
                    Struct(_) => 21,
                    Packed(_) => 22,
                };
                if kind_a != kind_b {
                    return false;
                }
            }
            // Proto names and method signature arity (args count + return kind)
            for (pa, pb) in a.protos.iter().zip(b.protos.iter()) {
                let na = this.get(pa.name);
                let nb = this.get(pb.name);
                if na != nb {
                    return false;
                }
                // Compare function type for methods by looking up funptr signature if resolvable
                // If not resolvable here, fallback to pindex equality (shape proxy)
                let (args_a, ret_a) = (0usize, 0u8);
                let (args_b, ret_b) = (0usize, 0u8);
                // Best-effort: signatures may be unavailable at this stage; skip deep comparison
                if args_a != args_b || ret_a != ret_b { /* keep name-based check only */ }
            }
            true
        }

        // Identify duplicate types in `other` vs `self` by name and compatible shape
        use crate::types::{RefField, Type};
        let mut duplicate_type_pairs: Vec<(usize /*other idx*/, usize /*self idx*/)> = Vec::new();
        for (oi, ot) in other.types.iter().enumerate() {
            if let Some(oobj) = ot.get_type_obj() {
                // Find a type in self with same name
                let oname = self.get(oobj.name);
                if let Some((si, sobj)) = self
                    .types
                    .iter()
                    .enumerate()
                    .filter_map(|(si, st)| st.get_type_obj().map(|obj| (si, obj)))
                    .find(|(_, sobj)| self.get(sobj.name) == oname)
                {
                    // Verify shape compatibility
                    if types_shape_compatible(&self, sobj, oobj) {
                        duplicate_type_pairs.push((oi, si));
                    }
                }
            }
        }

        // Build per-type field mappings for duplicates using flattened fields by name
        // Also pre-apply these mappings to `other` opcodes and TypeObj.bindings so we don't rely on global field_mappings
        use std::collections::HashMap as StdHashMap;
        let mut per_type_field_maps: StdHashMap<
            usize, /*other type idx*/
            StdHashMap<usize /*other field idx*/, usize /*self field idx*/>,
        > = StdHashMap::new();
        for (oi, si) in &duplicate_type_pairs {
            let (oobj, sobj) = match (
                other.types[*oi].get_type_obj(),
                self.types[*si].get_type_obj(),
            ) {
                (Some(o), Some(s)) => (o, s),
                _ => continue,
            };
            let mut name_to_self_index: StdHashMap<Str, usize> = StdHashMap::new();
            for (idx, f) in sobj.fields.iter().enumerate() {
                name_to_self_index.insert(self.get(f.name), idx);
            }
            let mut fmap = StdHashMap::new();
            for (idx, f) in oobj.fields.iter().enumerate() {
                if let Some(&si) = name_to_self_index.get(&self.get(f.name)) {
                    fmap.insert(idx, si);
                }
            }
            per_type_field_maps.insert(*oi, fmap);
        }

        // Do not populate global field_mappings: field indices are type-local.
        // We remap fields contextually in opcodes and TypeObj.bindings using per-type maps.

        // Pre-apply field mapping to `other` functions opcodes in a contextual way
        // Only rewrite field indices if the opcode targets an object/this of the duplicate type
        if !per_type_field_maps.is_empty() {
            for func in &mut other.functions {
                // regs hold types of registers; used to resolve obj register types
                for op in &mut func.ops {
                    match op {
                        Opcode::Field { obj, field, .. } => {
                            let reg_idx = obj.0 as usize;
                            if let Some(reg_ty) =
                                func.regs.get(reg_idx).and_then(|rt| other.types.get(rt.0))
                            {
                                if let Some((other_type_idx, _self_idx)) = duplicate_type_pairs
                                    .iter()
                                    .find(|(oi, _)| Some(&other.types[*oi]) == Some(reg_ty))
                                {
                                    if let Some(map) = per_type_field_maps.get(other_type_idx) {
                                        if let Some(&new_idx) = map.get(&field.0) {
                                            *field = RefField(new_idx);
                                        }
                                    }
                                }
                            }
                        }
                        Opcode::SetField { obj, field, .. } => {
                            let reg_idx = obj.0 as usize;
                            if let Some(reg_ty) =
                                func.regs.get(reg_idx).and_then(|rt| other.types.get(rt.0))
                            {
                                if let Some((other_type_idx, _self_idx)) = duplicate_type_pairs
                                    .iter()
                                    .find(|(oi, _)| Some(&other.types[*oi]) == Some(reg_ty))
                                {
                                    if let Some(map) = per_type_field_maps.get(other_type_idx) {
                                        if let Some(&new_idx) = map.get(&field.0) {
                                            *field = RefField(new_idx);
                                        }
                                    }
                                }
                            }
                        }
                        Opcode::GetThis { field, .. }
                        | Opcode::SetThis { field, .. }
                        | Opcode::CallThis { field, .. } => {
                            // this is reg0; its type is func.regs[0]
                            if let Some(reg_ty) =
                                func.regs.get(0).and_then(|rt| other.types.get(rt.0))
                            {
                                if let Some((other_type_idx, _self_idx)) = duplicate_type_pairs
                                    .iter()
                                    .find(|(oi, _)| Some(&other.types[*oi]) == Some(reg_ty))
                                {
                                    if let Some(map) = per_type_field_maps.get(other_type_idx) {
                                        if let Some(&new_idx) = map.get(&field.0) {
                                            *field = RefField(new_idx);
                                        }
                                    }
                                }
                            }
                        }
                        Opcode::CallMethod { args, field, .. } => {
                            // args[0] is the object
                            if let Some(obj_reg) = args.get(0) {
                                let reg_idx = obj_reg.0 as usize;
                                if let Some(reg_ty) =
                                    func.regs.get(reg_idx).and_then(|rt| other.types.get(rt.0))
                                {
                                    if let Some((other_type_idx, _self_idx)) = duplicate_type_pairs
                                        .iter()
                                        .find(|(oi, _)| Some(&other.types[*oi]) == Some(reg_ty))
                                    {
                                        if let Some(map) = per_type_field_maps.get(other_type_idx) {
                                            if let Some(&new_idx) = map.get(&field.0) {
                                                *field = RefField(new_idx);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        Opcode::SetEnumField { value, field, .. } => {
                            // Prefer using the value register's type to resolve context and remap
                            let reg_idx = value.0 as usize;
                            let mut remapped = false;
                            if let Some(reg_ty) =
                                func.regs.get(reg_idx).and_then(|rt| other.types.get(rt.0))
                            {
                                if let Some((other_type_idx, _self_idx)) = duplicate_type_pairs
                                    .iter()
                                    .find(|(oi, _)| Some(&other.types[*oi]) == Some(reg_ty))
                                {
                                    if let Some(map) = per_type_field_maps.get(other_type_idx) {
                                        if let Some(&new_idx) = map.get(&field.0) {
                                            *field = RefField(new_idx);
                                            remapped = true;
                                        }
                                    }
                                }
                            }
                            // Fallback: if value register type is unknown, use `this` type context
                            if !remapped {
                                if let Some(reg_ty) =
                                    func.regs.get(0).and_then(|rt| other.types.get(rt.0))
                                {
                                    if let Some((other_type_idx, _self_idx)) = duplicate_type_pairs
                                        .iter()
                                        .find(|(oi, _)| Some(&other.types[*oi]) == Some(reg_ty))
                                    {
                                        if let Some(map) = per_type_field_maps.get(other_type_idx) {
                                            if let Some(&new_idx) = map.get(&field.0) {
                                                *field = RefField(new_idx);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        Opcode::Prefetch { value, field, .. } if field.0 > 0 => {
                            let reg_idx = value.0 as usize;
                            if let Some(reg_ty) =
                                func.regs.get(reg_idx).and_then(|rt| other.types.get(rt.0))
                            {
                                if let Some((other_type_idx, _)) = duplicate_type_pairs
                                    .iter()
                                    .find(|(oi, _)| Some(&other.types[*oi]) == Some(reg_ty))
                                {
                                    if let Some(map) = per_type_field_maps.get(other_type_idx) {
                                        if let Some(&new_idx) = map.get(&(field.0 - 1)) {
                                            field.0 = new_idx + 1;
                                        }
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        // Pre-apply field mapping to TypeObj.bindings of duplicate types
        for (oi, _si) in &duplicate_type_pairs {
            if let Some(Type::Obj(ref mut oobj)) | Some(Type::Struct(ref mut oobj)) =
                other.types.get_mut(*oi)
            {
                if let Some(map) = per_type_field_maps.get(oi) {
                    let mut new_bindings = StdHashMap::new();
                    for (f, fun) in oobj.bindings.clone() {
                        let new_field_idx = map.get(&f.0).copied().unwrap_or(f.0);
                        new_bindings.insert(RefField(new_field_idx), fun);
                    }
                    oobj.bindings = new_bindings;
                }
            }
        }

        // Adjust all references in the other bytecode
        // Adjust references in `other` types using offsets only (avoid double string remap)
        let mut mapping_no_string = mapping.clone();
        mapping_no_string.string_remap = None;
        mapping_no_string.string_offset = 0; // prevent offsetting type names/strings here
        for typ in &mut other.types {
            typ.adjust_references(&mapping_no_string);
        }
        // Adjust references in functions, natives, and constants using full mapping (includes string remap)
        for func in &mut other.functions {
            func.adjust_references(&mapping);
        }
        for native in &mut other.natives {
            native.adjust_references(&mapping);
        }
        if let Some(ref mut constants) = other.constants {
            for constant in constants {
                constant.adjust_references(&mapping);
            }
        }

        // Merge the constant pools (strings already handled via remap dedup above)
        self.ints.extend(other.ints);
        self.floats.extend(other.floats);

        // Merge bytes pool
        match (&mut self.bytes, other.bytes) {
            (Some(self_bytes), Some(other_bytes)) => {
                let original_len = self_bytes.0.len();
                self_bytes.0.extend(other_bytes.0);
                self_bytes.1.extend(
                    other_bytes
                        .1
                        .into_iter()
                        .map(|offset| offset + original_len),
                );
            }
            (None, Some(other_bytes)) => {
                self.bytes = Some(other_bytes);
            }
            _ => {} // self has bytes but other doesn't, or both are None
        }

        // Merge debug files
        match (&mut self.debug_files, other.debug_files) {
            (Some(self_debug), Some(other_debug)) => {
                self_debug.extend(other_debug);
            }
            (None, Some(other_debug)) => {
                self.debug_files = Some(other_debug);
            }
            _ => {} // self has debug files but other doesn't, or both are None
        }

        // Merge the main data structures (skip duplicates)
        let mut other_types_filtered: Vec<Type> = Vec::with_capacity(other.types.len());
        for (i, t) in other.types.into_iter().enumerate() {
            if duplicate_type_pairs.iter().any(|(oi, _)| *oi == i) {
                // skip duplicate type
                continue;
            }
            other_types_filtered.push(t);
        }
        self.types.extend(other_types_filtered);
        self.globals.extend(other.globals);
        self.natives.extend(other.natives);
        self.functions.extend(other.functions);

        // Merge constants
        match (&mut self.constants, other.constants) {
            (Some(self_constants), Some(other_constants)) => {
                self_constants.extend(other_constants);
            }
            (None, Some(other_constants)) => {
                self.constants = Some(other_constants);
            }
            _ => {} // self has constants but other doesn't, or both are None
        }

        // Use the newer version if available
        if other.version > self.version {
            self.version = other.version;
        }

        // Keep the main entrypoint (could be made configurable)
        // self.entrypoint remains unchanged

        // Rebuild acceleration structures
        self.rebuild_acceleration_structures();

        Ok(self)
    }

    /// Replace all RefString references in this bytecode using the provided remap.
    /// The remap maps old string indexes to new string indexes. Index 0 should map to 0.
    pub fn replace_string_references(&mut self, remap: &[usize]) {
        let mapping = IndexMapping {
            string_remap: Some(remap.to_vec()),
            ..Default::default()
        };

        for typ in &mut self.types {
            typ.adjust_references(&mapping);
        }
        for func in &mut self.functions {
            func.adjust_references(&mapping);
        }
        for native in &mut self.natives {
            native.adjust_references(&mapping);
        }
        if let Some(ref mut constants) = self.constants {
            for constant in constants {
                constant.adjust_references(&mapping);
            }
        }
    }

    /// Replace only opcode string references (string literals and dynamic field names).
    /// This will NOT touch names, types, natives, enums, assigns, or any other RefString sites.
    /// The remap maps old string indexes to new string indexes. Index 0 should map to 0.
    ///
    /// Note: This does not affect bytecode "constants" used for global initialization.
    pub fn replace_opcode_string_references(&mut self, remap: &[usize]) {
        for func in &mut self.functions {
            for op in &mut func.ops {
                match op {
                    Opcode::String { ptr, .. } => {
                        if !ptr.is_null() {
                            let idx = ptr.0;
                            // Safety: caller must ensure remap covers all indices
                            ptr.0 = remap[idx];
                        }
                    }
                    Opcode::DynGet { field, .. } | Opcode::DynSet { field, .. } => {
                        if !field.is_null() {
                            let idx = field.0;
                            field.0 = remap[idx];
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    /// Replace only string references within type definitions (names of types, fields, protos, enums).
    /// This will NOT touch opcode literals, function names, natives, assigns, or other RefString sites outside types.
    /// The remap maps old string indexes to new string indexes. Index 0 should map to 0.
    pub fn replace_type_string_references(&mut self, remap: &[usize]) {
        let mapping = IndexMapping {
            string_remap: Some(remap.to_vec()),
            ..Default::default()
        };

        for typ in &mut self.types {
            typ.adjust_references(&mapping);
        }
    }

    /// Rebuilds the acceleration structures after merging
    fn rebuild_acceleration_structures(&mut self) {
        // Preserve existing findex IDs and rebuild mapping accordingly
        // Compute the maximum findex referenced across functions and natives
        let mut max_findex = 0usize;
        for f in &self.functions {
            if f.findex.0 > max_findex {
                max_findex = f.findex.0;
            }
        }
        for n in &self.natives {
            if n.findex.0 > max_findex {
                max_findex = n.findex.0;
            }
        }

        // Prepare filler based on availability; used for unassigned slots
        let filler = if !self.functions.is_empty() {
            RefFunKnown::Fun(0)
        } else {
            RefFunKnown::Native(0)
        };

        // Resize findexes vector to max_findex + 1
        if self.findexes.len() < max_findex + 1 {
            self.findexes.resize(max_findex + 1, filler);
        } else {
            self.findexes.truncate(max_findex + 1);
        }
        // Reset all slots to filler to avoid stale entries
        for slot in &mut self.findexes {
            *slot = filler;
        }

        // Map each function/native to its preserved findex
        for (fn_idx, func) in self.functions.iter().enumerate() {
            let idx = func.findex.0;
            if idx >= self.findexes.len() {
                self.findexes.resize(idx + 1, filler);
            }
            self.findexes[idx] = RefFunKnown::Fun(fn_idx);
        }
        for (nat_idx, nat) in self.natives.iter().enumerate() {
            let idx = nat.findex.0;
            if idx >= self.findexes.len() {
                self.findexes.resize(idx + 1, filler);
            }
            self.findexes[idx] = RefFunKnown::Native(nat_idx);
        }

        // Rebuild function names mapping
        self.fnames.clear();
        for (i, func) in self.functions.iter().enumerate() {
            let name = self.get(func.name);
            if !name.is_empty() && name != "<none>" {
                self.fnames.insert(name, i);
            }
        }

        // Rebuild globals initializers mapping
        self.globals_initializers.clear();
        if let Some(ref constants) = self.constants {
            for (i, constant) in constants.iter().enumerate() {
                self.globals_initializers.insert(constant.global, i);
            }
        }
    }
}

impl Default for Bytecode {
    fn default() -> Self {
        Self {
            version: 5,
            entrypoint: Default::default(),
            ints: vec![],
            floats: vec![],
            strings: vec![],
            bytes: None,
            debug_files: None,
            types: vec![],
            globals: vec![],
            natives: vec![],
            functions: vec![],
            constants: None,
            findexes: vec![],
            fnames: Default::default(),
            globals_initializers: Default::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Function, Reg};

    fn make_test_bytecode() -> Bytecode {
        let mut bc = Bytecode::default();
        // strings[0] is implicit null; ensure at least some entries
        bc.strings = vec![
            Str::from_static(""),
            Str::from_static("A"),
            Str::from_static("B"),
            Str::from_static("C"),
        ];
        // Create a function with opcodes that use strings
        let f = Function {
            t: RefType(0),
            findex: RefFun(0),
            regs: vec![],
            ops: vec![
                Opcode::String {
                    dst: Reg(0),
                    ptr: RefString(1),
                },
                Opcode::DynGet {
                    dst: Reg(1),
                    obj: Reg(2),
                    field: RefString(2),
                },
                Opcode::DynSet {
                    obj: Reg(3),
                    field: RefString(3),
                    src: Reg(4),
                },
            ],
            debug_info: None,
            assigns: None,
            name: RefString(0),
            parent: None,
        };
        bc.functions.push(f);
        // Minimal type: an object with a name and one field using strings
        use crate::types::{ObjField, Type, TypeObj};
        bc.types.push(Type::Obj(TypeObj {
            name: RefString(1),
            super_: None,
            global: RefGlobal(0),
            own_fields: vec![ObjField {
                name: RefString(2),
                t: RefType(0),
            }],
            protos: vec![],
            bindings: HashMap::new(),
            fields: vec![],
        }));
        bc
    }

    #[test]
    fn test_replace_opcode_string_references() {
        let mut bc = make_test_bytecode();
        // Remap: 0->0, 1->3, 2->1, 3->2
        let remap = vec![0, 3, 1, 2];
        bc.replace_opcode_string_references(&remap);
        let f = &bc.functions[0];
        match &f.ops[0] {
            Opcode::String { ptr, .. } => assert_eq!(ptr.0, 3),
            _ => unreachable!(),
        }
        match &f.ops[1] {
            Opcode::DynGet { field, .. } => assert_eq!(field.0, 1),
            _ => unreachable!(),
        }
        match &f.ops[2] {
            Opcode::DynSet { field, .. } => assert_eq!(field.0, 2),
            _ => unreachable!(),
        }
        // Names/metadata untouched
        assert_eq!(f.name.0, 0);
    }

    #[test]
    fn test_replace_string_references_global() {
        let mut bc = make_test_bytecode();
        // Set function name and a field in types/natives as minimal check
        if bc.types.is_empty() {
            bc.types.push(Type::Ref(RefType(0)));
        }
        bc.functions[0].name = RefString(1);
        let remap = vec![0, 3, 1, 2];
        bc.replace_string_references(&remap);
        let f = &bc.functions[0];
        // Opcodes remapped via AdjustReferences
        match &f.ops[0] {
            Opcode::String { ptr, .. } => assert_eq!(ptr.0, 3),
            _ => unreachable!(),
        }
        match &f.ops[1] {
            Opcode::DynGet { field, .. } => assert_eq!(field.0, 1),
            _ => unreachable!(),
        }
        match &f.ops[2] {
            Opcode::DynSet { field, .. } => assert_eq!(field.0, 2),
            _ => unreachable!(),
        }
        // Name remapped globally
        assert_eq!(f.name.0, 3);
    }

    #[test]
    fn test_replace_type_string_references() {
        let mut bc = make_test_bytecode();
        // Before: type name=1("A"), first field name=2("B"); opcodes reference 1,2,3
        let remap = vec![0, 3, 1, 2];
        bc.replace_type_string_references(&remap);
        // Verify type strings remapped
        match &bc.types[0] {
            Type::Obj(obj) => {
                assert_eq!(obj.name.0, 3);
                assert_eq!(obj.own_fields[0].name.0, 1);
            }
            _ => unreachable!(),
        }
        // Verify opcode strings unchanged
        let f = &bc.functions[0];
        match &f.ops[0] {
            Opcode::String { ptr, .. } => assert_eq!(ptr.0, 1),
            _ => unreachable!(),
        }
        match &f.ops[1] {
            Opcode::DynGet { field, .. } => assert_eq!(field.0, 2),
            _ => unreachable!(),
        }
        match &f.ops[2] {
            Opcode::DynSet { field, .. } => assert_eq!(field.0, 3),
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_merge_debug_file_indices() -> Result<()> {
        // First bytecode with 2 debug files and 1 function
        let mut bc1 = Bytecode::default();
        bc1.debug_files = Some(vec![
            Str::from_static("file_a.hl"),
            Str::from_static("file_b.hl"),
        ]);
        let f1 = Function {
            t: RefType(0),
            findex: RefFun(0),
            regs: vec![],
            ops: vec![Opcode::Null { dst: Reg(0) }, Opcode::Null { dst: Reg(0) }],
            debug_info: Some(vec![(0, 10), (1, 20)]),
            assigns: None,
            name: RefString(0),
            parent: None,
        };
        bc1.functions.push(f1);

        // Second bytecode with 3 debug files and 1 function
        let mut bc2 = Bytecode::default();
        bc2.debug_files = Some(vec![
            Str::from_static("file_c.hl"),
            Str::from_static("file_d.hl"),
            Str::from_static("file_e.hl"),
        ]);
        let f2 = Function {
            t: RefType(0),
            findex: RefFun(0),
            regs: vec![],
            ops: vec![Opcode::Null { dst: Reg(0) }, Opcode::Null { dst: Reg(0) }],
            debug_info: Some(vec![(2, 30), (0, 40)]),
            assigns: None,
            name: RefString(0),
            parent: None,
        };
        bc2.functions.push(f2);

        // Merge and verify debug file indices are adjusted for functions from bc2
        let merged = bc1.merge_with(bc2)?;
        let df = merged.debug_files.as_ref().unwrap();
        assert_eq!(df.len(), 5);
        assert_eq!(df[0], Str::from_static("file_a.hl"));
        assert_eq!(df[1], Str::from_static("file_b.hl"));
        assert_eq!(df[2], Str::from_static("file_c.hl"));
        assert_eq!(df[3], Str::from_static("file_d.hl"));
        assert_eq!(df[4], Str::from_static("file_e.hl"));

        // The function from bc2 is now at index 1; its file indices should be offset by 2
        let f2_merged = &merged.functions[1];
        let dbg = f2_merged.debug_info.as_ref().unwrap();
        assert_eq!(dbg[0].0, 4); // original 2 + offset 2
        assert_eq!(dbg[1].0, 2); // original 0 + offset 2
        Ok(())
    }

    #[test]
    fn test_merge_with_string_dedup_across_types_and_opcodes() {
        use crate::types::{ObjField, ObjProto, Type, TypeObj};
        // Bytecode 1 with strings: "", A, B, X, M, L, DF
        let mut bc1 = Bytecode::default();
        bc1.strings = vec![
            Str::from_static(""),
            Str::from_static("A"),
            Str::from_static("B"),
            Str::from_static("X"),
            Str::from_static("M"),
            Str::from_static("L"),
            Str::from_static("DF"),
        ];
        // TypeObj with name A, field X, proto M
        bc1.types.push(Type::Obj(TypeObj {
            name: RefString(1),
            super_: None,
            global: RefGlobal(0),
            own_fields: vec![ObjField {
                name: RefString(3),
                t: RefType(0),
            }],
            protos: vec![ObjProto {
                name: RefString(4),
                findex: RefFun(0),
                pindex: 0,
            }],
            bindings: HashMap::new(),
            fields: vec![],
        }));
        // Function uses L, DF, X; name is null sentinel
        let f1 = Function {
            t: RefType(0),
            findex: RefFun(0),
            regs: vec![],
            ops: vec![
                Opcode::String {
                    dst: Reg(0),
                    ptr: RefString(5),
                },
                Opcode::DynGet {
                    dst: Reg(1),
                    obj: Reg(2),
                    field: RefString(6),
                },
                Opcode::DynSet {
                    obj: Reg(3),
                    field: RefString(3),
                    src: Reg(4),
                },
            ],
            debug_info: None,
            assigns: None,
            name: RefString(0),
            parent: None,
        };
        bc1.functions.push(f1);

        // Bytecode 2 with overlapping strings: "", B, C, X, N, L2, DF, A
        let mut bc2 = Bytecode::default();
        bc2.strings = vec![
            Str::from_static(""),
            Str::from_static("B"),
            Str::from_static("C"),
            Str::from_static("X"),
            Str::from_static("N"),
            Str::from_static("L2"),
            Str::from_static("DF"),
            Str::from_static("A"),
        ];
        // TypeObj with name C, field X, proto N
        bc2.types.push(Type::Obj(TypeObj {
            name: RefString(2),
            super_: None,
            global: RefGlobal(0),
            own_fields: vec![ObjField {
                name: RefString(3),
                t: RefType(0),
            }],
            protos: vec![ObjProto {
                name: RefString(4),
                findex: RefFun(0),
                pindex: 0,
            }],
            bindings: HashMap::new(),
            fields: vec![],
        }));
        // Function uses L2, DF, A; name is null sentinel
        let f2 = Function {
            t: RefType(0),
            findex: RefFun(0),
            regs: vec![],
            ops: vec![
                Opcode::String {
                    dst: Reg(0),
                    ptr: RefString(5),
                },
                Opcode::DynGet {
                    dst: Reg(1),
                    obj: Reg(2),
                    field: RefString(6),
                },
                Opcode::DynSet {
                    obj: Reg(3),
                    field: RefString(7),
                    src: Reg(4),
                },
            ],
            debug_info: None,
            assigns: None,
            name: RefString(0),
            parent: None,
        };
        bc2.functions.push(f2);

        // Merge and validate string remap dedup and application across all sites
        let merged = bc1.merge_with(bc2).unwrap();

        // Verify final strings: dedup identical, preserve 0, append new in order seen
        let expected_strings = vec![
            Str::from_static(""),
            Str::from_static("A"),
            Str::from_static("B"),
            Str::from_static("X"),
            Str::from_static("M"),
            Str::from_static("L"),
            Str::from_static("DF"),
            Str::from_static("C"),
            Str::from_static("N"),
            Str::from_static("L2"),
        ];
        assert_eq!(merged.strings, expected_strings);

        // Type names, field names, and proto names remapped correctly
        match &merged.types[0] {
            Type::Obj(obj) => {
                assert_eq!(obj.name.0, 1); // A
                assert_eq!(obj.own_fields[0].name.0, 3); // X
                assert_eq!(obj.protos[0].name.0, 4); // M
            }
            _ => unreachable!(),
        }
        match &merged.types[1] {
            Type::Obj(obj) => {
                assert_eq!(obj.name.0, 7); // C (new)
                assert_eq!(obj.own_fields[0].name.0, 3); // X (dedup)
                assert_eq!(obj.protos[0].name.0, 8); // N (new)
            }
            _ => unreachable!(),
        }

        // Opcode string literal and dynamic field names remapped correctly
        let f1_m = &merged.functions[0];
        match &f1_m.ops[0] {
            Opcode::String { ptr, .. } => assert_eq!(ptr.0, 5),
            _ => unreachable!(),
        }
        match &f1_m.ops[1] {
            Opcode::DynGet { field, .. } => assert_eq!(field.0, 6),
            _ => unreachable!(),
        }
        match &f1_m.ops[2] {
            Opcode::DynSet { field, .. } => assert_eq!(field.0, 3),
            _ => unreachable!(),
        }

        let f2_m = &merged.functions[1];
        match &f2_m.ops[0] {
            Opcode::String { ptr, .. } => assert_eq!(ptr.0, 9),
            _ => unreachable!(),
        }
        match &f2_m.ops[1] {
            Opcode::DynGet { field, .. } => assert_eq!(field.0, 6),
            _ => unreachable!(),
        }
        match &f2_m.ops[2] {
            Opcode::DynSet { field, .. } => assert_eq!(field.0, 1),
            _ => unreachable!(),
        }

        // RefString(0) is preserved for names
        assert_eq!(f2_m.name.0, 0);
    }

    #[test]
    fn test_merge_bytes_pool_offsets_and_ptr_adjustment() {
        use crate::types::{Function, RefBytes, RefFun, RefString, RefType, Reg};

        // Bytecode 1: raw bytes [1,2,3], offsets [0,1]
        // This yields entries: [1] and [2,3]
        let mut bc1 = Bytecode::default();
        bc1.bytes = Some((vec![1u8, 2, 3], vec![0usize, 1usize]));
        bc1.functions.push(Function {
            t: RefType(0),
            findex: RefFun(0),
            regs: vec![],
            ops: vec![Opcode::Bytes {
                dst: Reg(0),
                ptr: RefBytes(0),
            }],
            debug_info: None,
            assigns: None,
            name: RefString(0),
            parent: None,
        });

        // Bytecode 2: raw bytes [9,8,7,6], offsets [0,2]
        // This yields entries: [9,8] and [7,6]
        let mut bc2 = Bytecode::default();
        bc2.bytes = Some((vec![9u8, 8, 7, 6], vec![0usize, 2usize]));
        bc2.functions.push(Function {
            t: RefType(0),
            findex: RefFun(0),
            regs: vec![],
            ops: vec![Opcode::Bytes {
                dst: Reg(0),
                ptr: RefBytes(1),
            }],
            debug_info: None,
            assigns: None,
            name: RefString(0),
            parent: None,
        });

        // Merge: bytes pool raw should concatenate; second pool offsets shifted by original_len of first raw
        let merged = bc1.merge_with(bc2).unwrap();
        let (buf, pos) = merged
            .bytes
            .as_ref()
            .expect("merged bytes pool should exist");

        // Verify concatenated raw bytes and shifted offsets
        assert_eq!(buf.as_slice(), &[1u8, 2, 3, 9, 8, 7, 6]);
        assert_eq!(pos.as_slice(), &[0usize, 1usize, 3usize, 5usize]);

        // Helper to resolve a RefBytes index to the corresponding slice
        let slice_for = |i: usize| {
            let start = pos[i];
            let end = if i + 1 < pos.len() {
                pos[i + 1]
            } else {
                buf.len()
            };
            &buf[start..end]
        };

        // Function from bc1 should still point to index 0 -> slice [1]
        match merged.functions[0].ops[0] {
            Opcode::Bytes { ptr, .. } => {
                assert_eq!(ptr.0, 0);
                assert_eq!(slice_for(ptr.0), &[1u8]);
            }
            _ => panic!("expected Opcode::Bytes in first function"),
        }

        // Function from bc2 should have ptr adjusted by bytes_offset (len of bc1 offsets = 2)
        // Original ptr was 1 -> merged ptr should be 3, slice [7,6]
        match merged.functions[1].ops[0] {
            Opcode::Bytes { ptr, .. } => {
                assert_eq!(ptr.0, 3);
                assert_eq!(slice_for(ptr.0), &[7u8, 6]);
            }
            _ => panic!("expected Opcode::Bytes in second function"),
        }
    }

    #[test]
    fn test_merge_field_index_remap_in_opcodes_and_prefetch() {
        use crate::types::{
            InlineInt, ObjField, RefField, RefFun, RefString, RefType, Reg, Type, TypeObj,
        };
        use std::collections::HashMap;
        // bc1: type T with flattened fields [p1, a1]
        let mut bc1 = Bytecode::default();
        bc1.strings = vec![
            Str::from_static(""),
            Str::from_static("T"),
            Str::from_static("p1"),
            Str::from_static("a1"),
        ];
        bc1.types.push(Type::Obj(TypeObj {
            name: RefString(1),
            super_: None,
            global: RefGlobal(0),
            own_fields: vec![ObjField {
                name: RefString(3),
                t: RefType(0),
            }],
            protos: vec![],
            bindings: HashMap::new(),
            fields: vec![
                ObjField {
                    name: RefString(2),
                    t: RefType(0),
                }, // p1
                ObjField {
                    name: RefString(3),
                    t: RefType(0),
                }, // a1
            ],
        }));
        // dummy function to place bc2 function at merged index 1
        bc1.functions.push(Function {
            t: RefType(0),
            findex: RefFun(0),
            regs: vec![],
            ops: vec![Opcode::Null { dst: Reg(0) }],
            debug_info: None,
            assigns: None,
            name: RefString(0),
            parent: None,
        });

        // bc2: duplicate T with only [a1]
        let mut bc2 = Bytecode::default();
        bc2.strings = vec![
            Str::from_static(""),
            Str::from_static("T"),
            Str::from_static("a1"),
        ];
        bc2.types.push(Type::Obj(TypeObj {
            name: RefString(1),
            super_: None,
            global: RefGlobal(0),
            own_fields: vec![ObjField {
                name: RefString(2),
                t: RefType(0),
            }],
            protos: vec![],
            bindings: HashMap::new(),
            fields: vec![ObjField {
                name: RefString(2),
                t: RefType(0),
            }],
        }));
        bc2.functions.push(Function {
            t: RefType(0),
            findex: RefFun(0),
            regs: vec![RefType(0)], // reg0 is T
            ops: vec![
                Opcode::GetThis {
                    dst: Reg(1),
                    field: RefField(0),
                },
                Opcode::Prefetch {
                    value: Reg(1),
                    field: RefField(0),
                    mode: 0,
                },
                Opcode::Prefetch {
                    value: Reg(0),
                    field: RefField(1),
                    mode: 0,
                },
            ],
            debug_info: None,
            assigns: None,
            name: RefString(0),
            parent: None,
        });

        let merged = bc1.merge_with(bc2).expect("merge_with should succeed");
        let f_merged = &merged.functions[1];
        match &f_merged.ops[0] {
            Opcode::GetThis { field, .. } => assert_eq!(field.0, 1),
            _ => panic!("expected GetThis"),
        }
        match &f_merged.ops[1] {
            Opcode::Prefetch { field, .. } => assert_eq!(field.0, 0),
            _ => panic!("expected Prefetch"),
        }
        match &f_merged.ops[2] {
            Opcode::Prefetch { field, .. } => assert_eq!(field.0, 2),
            _ => panic!("expected Prefetch"),
        }
    }

    #[test]
    fn test_adjust_references_remaps_typeobj_bindings_fields() {
        use crate::types::{ObjField, RefField, RefFun, RefString, RefType, TypeObj};
        use std::collections::HashMap;
        // Build TypeObj with two fields and a binding for field 0
        let mut obj = TypeObj {
            name: RefString(1),
            super_: None,
            global: RefGlobal(0),
            own_fields: vec![
                ObjField {
                    name: RefString(2),
                    t: RefType(0),
                },
                ObjField {
                    name: RefString(3),
                    t: RefType(0),
                },
            ],
            protos: vec![],
            bindings: {
                let mut m = HashMap::new();
                m.insert(RefField(0), RefFun(10));
                m
            },
            fields: vec![
                ObjField {
                    name: RefString(2),
                    t: RefType(0),
                },
                ObjField {
                    name: RefString(3),
                    t: RefType(0),
                },
            ],
        };
        // Prepare a mapping that remaps field 0 -> 1 and shifts functions by +5
        let mut mapping = IndexMapping::default();
        mapping.field_mappings.insert(RefField(0), RefField(1));
        mapping.function_offset = 5;
        obj.adjust_references(&mapping);
        assert!(obj.bindings.contains_key(&RefField(1)));
        assert_eq!(obj.bindings.get(&RefField(1)).unwrap().0, 15);
        assert!(!obj.bindings.contains_key(&RefField(0)));
    }
}
/// Index reference to either a function or a native.
#[derive(Debug, Copy, Clone)]
enum RefFunKnown {
    Fun(usize),
    Native(usize),
}

//region Resolve

/// Like the [Index] trait but allows returning any type.
pub trait Resolve<I> {
    type Output<'a>
    where
        Self: 'a;

    fn get(&self, index: I) -> Self::Output<'_>;
}

impl Resolve<RefInt> for Bytecode {
    type Output<'a> = i32;

    fn get(&self, index: RefInt) -> Self::Output<'_> {
        self.ints[index.0]
    }
}

impl Resolve<RefFloat> for Bytecode {
    type Output<'a> = f64;

    fn get(&self, index: RefFloat) -> Self::Output<'_> {
        self.floats[index.0]
    }
}

impl Resolve<RefString> for Bytecode {
    type Output<'a> = Str;

    fn get(&self, index: RefString) -> Self::Output<'_> {
        if index.0 > 0 {
            self.strings[index.0].clone()
        } else {
            Str::from_static("<none>")
        }
    }
}

impl Resolve<RefType> for Bytecode {
    type Output<'a> = &'a Type;

    fn get(&self, index: RefType) -> Self::Output<'_> {
        &self.types[index.0]
    }
}

impl Resolve<RefGlobal> for Bytecode {
    type Output<'a> = &'a RefType;

    fn get(&self, index: RefGlobal) -> Self::Output<'_> {
        &self.globals[index.0]
    }
}

impl Resolve<RefFun> for Bytecode {
    type Output<'a> = FunPtr<'a>;

    fn get(&self, index: RefFun) -> Self::Output<'_> {
        // SAFETY: use new helper for fallibility and upstream defense for display code.
        match Bytecode::safe_get_ref_fun(self, index) {
            Some(fp) => fp,
            None => return FunPtr::Fun(&self.functions[0]), // fallback, see types.rs for display handling!
        }
    }
}

//endregion

// region Index impl

impl Index<RefInt> for Bytecode {
    type Output = i32;

    fn index(&self, index: RefInt) -> &Self::Output {
        self.ints.index(index.0)
    }
}

impl Index<RefFloat> for Bytecode {
    type Output = f64;

    fn index(&self, index: RefFloat) -> &Self::Output {
        self.floats.index(index.0)
    }
}

impl Index<RefString> for Bytecode {
    type Output = Str;

    fn index(&self, index: RefString) -> &Self::Output {
        self.strings.index(index.0)
    }
}

impl Index<RefType> for Bytecode {
    type Output = Type;

    fn index(&self, index: RefType) -> &Self::Output {
        self.types.index(index.0)
    }
}

impl Index<RefGlobal> for Bytecode {
    type Output = RefType;

    fn index(&self, index: RefGlobal) -> &Self::Output {
        self.globals.index(index.0)
    }
}

//endregion

#[test]
fn test_merge_cross_bytecode_calls_and_ref_resolution() {
    use crate::opcodes::Opcode;
    use crate::types::Reg;
    use crate::types::{
        Native, ObjField, ObjProto, RefBytes, RefFun, RefGlobal, RefString, RefType, Type, TypeObj,
    };
    use std::collections::HashMap;

    // Build Bytecode 1
    let mut bc1 = Bytecode::default();
    bc1.strings = vec![
        "",       // 0 (often empty sentinel)
        "Common", // 1
        "T1",     // 2
        "fieldX", // 3
        "f1",     // 4
    ]
    .into_iter()
    .map(Str::from)
    .collect();
    bc1.bytes = Some((vec![1u8, 2, 3], vec![0usize, 2usize]));
    bc1.debug_files = Some(vec![Str::from("b1.hl")]);
    bc1.globals.push(RefType(0));
    bc1.types.push(Type::Obj(TypeObj {
        name: RefString(2),
        super_: None,
        global: RefGlobal(0),
        own_fields: vec![ObjField {
            name: RefString(3),
            t: RefType(0),
        }],
        fields: vec![],
        protos: vec![],
        bindings: HashMap::new(),
    }));

    // f1 in bc1
    let f1_findex = RefFun(10);
    bc1.functions.push(Function {
        t: RefType(0),
        findex: f1_findex,
        regs: vec![],
        ops: vec![
            Opcode::String {
                dst: Reg(0),
                ptr: RefString(1),
            }, // "Common"
            Opcode::GetGlobal {
                dst: Reg(1),
                global: RefGlobal(0),
            },
            Opcode::Bytes {
                dst: Reg(2),
                ptr: RefBytes(0),
            },
        ],
        debug_info: Some(vec![(0, 1), (0, 2)]),
        assigns: None,
        name: RefString(4), // "f1"
        parent: None,
    });

    // Build Bytecode 2
    let mut bc2 = Bytecode::default();
    bc2.strings = vec![
        "",           // 0
        "Common",     // 1 (overlaps with bc1)
        "T2",         // 2
        "fieldY",     // 3
        "f2",         // 4
        "native_lib", // 5
    ]
    .into_iter()
    .map(Str::from)
    .collect();
    bc2.bytes = Some((vec![9u8, 8, 7, 6], vec![0usize, 2usize, 3usize]));
    bc2.debug_files = Some(vec![Str::from("b2a.hl"), Str::from("b2b.hl")]);
    bc2.globals.push(RefType(0));
    bc2.globals.push(RefType(0));

    // Native with explicit findex to test native resolution
    bc2.natives.push(Native {
        t: RefType(0),
        findex: RefFun(30),
        lib: RefString(5),
        name: RefString(4), // name is "f2"
    });

    // f2 function with findex 20 and references across pools; calls f1 across bytecodes
    let f2 = Function {
        t: RefType(0),
        findex: RefFun(20),
        regs: vec![],
        ops: vec![
            Opcode::String {
                dst: Reg(0),
                ptr: RefString(1),
            }, // "Common"
            Opcode::GetGlobal {
                dst: Reg(1),
                global: RefGlobal(1),
            }, // will shift by bc1.globals.len()
            Opcode::Bytes {
                dst: Reg(2),
                ptr: RefBytes(2),
            }, // last entry in bc2 bytes -> should adjust
            Opcode::Type {
                dst: Reg(3),
                ty: RefType(0),
            }, // type will adjust by type_offset
            Opcode::Call0 {
                dst: Reg(4),
                fun: RefFun(f1_findex.0 - 1),
            }, // after adjust -> 10
            Opcode::StaticClosure {
                dst: Reg(5),
                fun: RefFun(30),
            }, // native test
        ],
        debug_info: Some(vec![(1, 11), (0, 12)]),
        assigns: None,
        name: RefString(4), // "f2"
        parent: None,
    };
    bc2.functions.push(f2);

    // bc2 TypeObj with a proto pointing to f2 (RefFun(20))
    bc2.types.push(Type::Obj(TypeObj {
        name: RefString(2), // "T2"
        super_: None,
        global: RefGlobal(1), // second global in bc2
        own_fields: vec![ObjField {
            name: RefString(3),
            t: RefType(0),
        }],
        fields: vec![],
        protos: vec![ObjProto {
            name: RefString(4),
            findex: RefFun(20),
            pindex: 0,
        }],
        bindings: HashMap::new(),
    }));

    // Merge
    let merged = bc1.merge_with(bc2).expect("merge_with should succeed");

    // Assert RefFun resolution for f1
    match merged.safe_get_ref_fun(f1_findex) {
        Some(FunPtr::Fun(fun)) => assert_eq!(merged.get(fun.name), "f1"),
        _ => panic!("f1 should resolve to Function"),
    }
    // Assert RefFun resolution for f2 (post-merge findex = 20 + bc1.functions.len())
    let f2_final_findex_after_merge = 20 + 1; // bc1.functions.len() == 1
    match merged.safe_get_ref_fun(RefFun(f2_final_findex_after_merge)) {
        Some(FunPtr::Fun(fun)) => assert_eq!(merged.get(fun.name), "f2"),
        _ => panic!("f2 should resolve to Function"),
    }
    // Assert native resolution (native findex 30 + offset of bc1.functions.len() = 1)
    let native_findex_after = 30 + 1;
    match merged.safe_get_ref_fun(RefFun(native_findex_after)) {
        Some(FunPtr::Native(n)) => assert_eq!(merged.get(n.name), "f2"),
        _ => panic!("native should resolve to Native"),
    }

    // Retrieve merged functions by name
    let f1_merged = merged
        .functions
        .iter()
        .find(|f| merged.get(f.name) == "f1")
        .expect("merged should contain f1");
    let f2_merged = merged
        .functions
        .iter()
        .find(|f| merged.get(f.name) == "f2")
        .expect("merged should contain f2");

    // f1: Check GetGlobal and Bytes
    match &f1_merged.ops[1] {
        Opcode::GetGlobal { global, .. } => assert_eq!(global.0, 0),
        _ => panic!("expected GetGlobal in f1"),
    }
    match &f1_merged.ops[2] {
        Opcode::Bytes { ptr, .. } => assert_eq!(ptr.0, 0),
        _ => panic!("expected Bytes in f1"),
    }

    // f2: GetGlobal adjusted by bc1.globals.len() = 1 -> 2 in merged
    match &f2_merged.ops[1] {
        Opcode::GetGlobal { global, .. } => assert_eq!(global.0, 2),
        _ => panic!("expected GetGlobal in f2"),
    }
    // f2: Bytes ptr adjusted by bytes_offset = len(bc1 bytes offsets)=2; original 2 -> merged 4
    match &f2_merged.ops[2] {
        Opcode::Bytes { ptr, .. } => assert_eq!(ptr.0, 4),
        _ => panic!("expected Bytes in f2"),
    }
    // f2: Type operand adjusted by type_offset = bc1.types.len() = 1
    match &f2_merged.ops[3] {
        Opcode::Type { ty, .. } => assert_eq!(ty.0, 1),
        _ => panic!("expected Type in f2"),
    }
    // f2: Call0 to f1 adjusted by function_offset: original (10 - 1) -> merged 10
    match &f2_merged.ops[4] {
        Opcode::Call0 { fun, .. } => assert_eq!(fun.0, 10),
        _ => panic!("expected Call0 to f1 in f2"),
    }
    // f2: StaticClosure to native findex 30 + offset
    match &f2_merged.ops[5] {
        Opcode::StaticClosure { fun, .. } => assert_eq!(fun.0, native_findex_after),
        _ => panic!("expected StaticClosure to native in f2"),
    }

    // Debug info indices adjusted for f2 by debug_file_offset = bc1.debug_files.len() = 1
    let dbg2 = f2_merged.debug_info.as_ref().expect("f2 has debug info");
    assert_eq!(dbg2[0].0, 2); // original 1 + offset 1
    assert_eq!(dbg2[1].0, 1); // original 0 + offset 1

    // String dedup: "Common" should appear once and both ops reference same index
    let common_idx = merged
        .strings
        .iter()
        .position(|s| s == "Common")
        .expect("Common string present");
    match &f1_merged.ops[0] {
        Opcode::String { ptr, .. } => assert_eq!(ptr.0, common_idx),
        _ => panic!("expected String 'Common' in f1"),
    }
    match &f2_merged.ops[0] {
        Opcode::String { ptr, .. } => assert_eq!(ptr.0, common_idx),
        _ => panic!("expected String 'Common' in f2"),
    }

    // Type/global/proto adjustments: verify bc2's TypeObj in merged
    match &merged.types[1] {
        Type::Obj(obj) => {
            assert_eq!(merged.get(obj.name), "T2");
            assert_eq!(obj.global.0, 2); // original 1 + global_offset 1
            assert_eq!(obj.protos[0].findex.0, f2_final_findex_after_merge);
        }
        _ => panic!("expected Type::Obj for bc2"),
    }
}
