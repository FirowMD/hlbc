use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::io::{BufRead, BufReader, Read};
use std::path::Path;
use std::str::from_utf8;

use byteorder::{LittleEndian, ReadBytesExt};
use indexmap::IndexMap;

use crate::types::{
    EnumConstruct, Function, Native, ObjField, ObjProto, RefField, RefFloat, RefInt, RefString,
    RefType, Type, TypeFun, TypeObj,
};
use crate::{Bytecode, ConstantDef, Opcode, RefFun, RefFunKnown, RefGlobal, Str};
use crate::{Error, Result};

const MAX_TABLE_ITEMS: usize = 16 * 1024 * 1024;
const MAX_BLOB_BYTES: usize = 512 * 1024 * 1024;

pub(crate) fn checked_count(r: &mut impl Read, kind: &'static str) -> Result<usize> {
    let count = read_varu(r)? as usize;
    if count > MAX_TABLE_ITEMS {
        Err(Error::CountLimit {
            kind,
            count,
            limit: MAX_TABLE_ITEMS,
        })
    } else {
        Ok(count)
    }
}

fn checked_blob_len(r: &mut impl Read, kind: &'static str) -> Result<usize> {
    let signed = r.read_i32::<LittleEndian>()?;
    if signed < 0 {
        return Err(Error::MalformedBytecode(format!(
            "Negative {kind} byte length {signed}"
        )));
    }
    let count = signed as usize;
    if count > MAX_BLOB_BYTES {
        Err(Error::CountLimit {
            kind,
            count,
            limit: MAX_BLOB_BYTES,
        })
    } else {
        Ok(count)
    }
}

fn invalid_ref(kind: &'static str, index: usize, len: usize) -> Error {
    Error::InvalidReference { kind, index, len }
}

impl Bytecode {
    /// Read the bytecode from a file. This method will skip bytes until the magic header is found.
    ///
    /// It uses a 512KiB buffer.
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self> {
        Self::deserialize(&mut BufReader::with_capacity(
            512 * 1024,
            fs::File::open(path)?,
        ))
    }

    /// Load the bytecode from any source. This method skips bytes until the magic header is found.
    pub fn deserialize(mut r: impl BufRead) -> Result<Self> {
        let mut bytes = Vec::new();
        r.read_to_end(&mut bytes)?;
        let index = memchr::memmem::find(&bytes, b"HLB").ok_or_else(|| {
            Error::MalformedBytecode("HashLink magic header not found before end of input".into())
        })?;
        Self::deserialize_exact(&mut &bytes[index..])
    }

    /// Load the bytecode from any source.
    /// Must be a valid hashlink bytecode binary that starts with the magic header.
    fn deserialize_exact(r: &mut impl Read) -> Result<Self> {
        let mut header = [0u8; 3];
        r.read_exact(&mut header)?;
        if header != [b'H', b'L', b'B'] {
            return Err(Error::MalformedBytecode(format!(
                "Invalid magic header (expected: {:?}, got: {header:?})",
                b"HLB"
            )));
        }
        let version = r.read_u8()?;
        if version < 2 || version > 5 {
            return Err(Error::UnsupportedVersion {
                version,
                min: 2,
                max: 5,
            });
        }
        let flags = read_varu(r)?;
        let has_debug = flags & 1 == 1;
        let nints = checked_count(r, "integer")?;
        let nfloats = checked_count(r, "float")?;
        let nstrings = checked_count(r, "string")?;
        let nbytes = if version >= 5 {
            Some(checked_count(r, "bytes")?)
        } else {
            None
        };
        let ntypes = checked_count(r, "type")?;
        let nglobals = checked_count(r, "global")?;
        let nnatives = checked_count(r, "native")?;
        let nfunctions = checked_count(r, "function")?;
        let nconstants = if version >= 4 {
            Some(checked_count(r, "constant")?)
        } else {
            None
        };
        let entrypoint = RefFun::read(r)?;

        let mut ints = vec![0i32; nints];
        for i in ints.iter_mut() {
            *i = r.read_i32::<LittleEndian>()?;
        }

        let mut floats = vec![0f64; nfloats];
        for i in floats.iter_mut() {
            *i = r.read_f64::<LittleEndian>()?;
        }

        let strings = read_strings(r, nstrings)?;

        let bytes = if let Some(nbytes) = nbytes {
            let size = checked_blob_len(r, "bytes")?;
            let mut bytes = vec![0; size];
            r.read_exact(&mut bytes)?;
            let mut pos = Vec::with_capacity(nbytes);
            for _ in 0..nbytes {
                pos.push(read_varu(r)? as usize);
            }
            Some((bytes, pos))
        } else {
            None
        };

        let debug_files = if has_debug {
            let n = checked_count(r, "debug file")?;
            Some(read_strings(r, n)?)
        } else {
            None
        };

        let mut types = Vec::with_capacity(ntypes);
        for _ in 0..ntypes {
            types.push(Type::read(r)?);
        }

        let mut globals = Vec::with_capacity(nglobals);
        for _ in 0..nglobals {
            globals.push(RefType::read(r)?);
        }

        let mut natives = Vec::with_capacity(nnatives);
        for _ in 0..nnatives {
            natives.push(Native::read(r)?);
        }

        let mut functions = Vec::with_capacity(nfunctions);
        for _ in 0..nfunctions {
            functions.push(Function::read(r, has_debug, version)?);
        }

        let constants = if let Some(n) = nconstants {
            let mut constants = Vec::with_capacity(n);
            for _ in 0..n {
                constants.push(ConstantDef::read(r)?)
            }
            Some(constants)
        } else {
            None
        };

        // Parsing is finished, we now build links between everything

        // Global function indexes
        let mut findexes = vec![None; nfunctions + nnatives];
        for (i, f) in functions.iter().enumerate() {
            let len = findexes.len();
            let slot = findexes
                .get_mut(f.findex.0)
                .ok_or_else(|| invalid_ref("function index", f.findex.0, len))?;
            if slot.replace(RefFunKnown::Fun(i)).is_some() {
                return Err(Error::MalformedBytecode(format!(
                    "Duplicate function index {}",
                    f.findex.0
                )));
            }
        }
        for (i, n) in natives.iter().enumerate() {
            let len = findexes.len();
            let slot = findexes
                .get_mut(n.findex.0)
                .ok_or_else(|| invalid_ref("native function index", n.findex.0, len))?;
            if slot.replace(RefFunKnown::Native(i)).is_some() {
                return Err(Error::MalformedBytecode(format!(
                    "Duplicate function index {}",
                    n.findex.0
                )));
            }
        }
        let findexes: Vec<_> = findexes
            .into_iter()
            .enumerate()
            .map(|(i, value)| {
                value.ok_or_else(|| Error::MalformedBytecode(format!("Missing function index {i}")))
            })
            .collect::<Result<_>>()?;

        // Flatten types fields
        // Start by collecting every field in the hierarchy
        // The order is important because we refer to fields by index
        let mut new_fields: Vec<Option<Vec<ObjField>>> = Vec::with_capacity(types.len());
        for t in &types {
            if let Some(obj) = t.get_type_obj() {
                let mut parent = obj.super_;
                let mut seen_parents = HashSet::new();
                let mut acc = VecDeque::with_capacity(obj.own_fields.len());
                acc.extend(obj.own_fields.clone());
                while let Some(parent_ref) = parent {
                    if !seen_parents.insert(parent_ref.0) {
                        return Err(Error::MalformedBytecode(format!(
                            "Cycle in object inheritance at type {}",
                            parent_ref.0
                        )));
                    }
                    let parent_type = types
                        .get(parent_ref.0)
                        .ok_or_else(|| invalid_ref("parent type", parent_ref.0, types.len()))?;
                    let Some(p) = parent_type.get_type_obj() else {
                        return Err(Error::MalformedBytecode(format!(
                            "Parent type {} is not an object or struct",
                            parent_ref.0
                        )));
                    };
                    for f in p.own_fields.iter().rev() {
                        acc.push_front(f.clone());
                    }
                    parent = p.super_;
                }
                new_fields.push(Some(acc.into()));
            } else {
                new_fields.push(None);
            }
        }
        // Apply new fields
        for (t, new) in types.iter_mut().zip(new_fields.into_iter()) {
            if let Some(fields) = new {
                if let Some(obj) = t.get_type_obj_mut() {
                    obj.fields = fields;
                }
            }
        }

        // Give functions name based on object fields bindings and methods
        for (i, t) in types.iter().enumerate() {
            if let Some(TypeObj {
                protos, bindings, ..
            }) = t.get_type_obj()
            {
                for p in protos {
                    let target = findexes.get(p.findex.0).ok_or_else(|| {
                        invalid_ref("prototype function", p.findex.0, findexes.len())
                    })?;
                    if let RefFunKnown::Fun(x) = *target {
                        functions[x].name = p.name;
                        functions[x].parent = Some(RefType(i));
                    }
                }
                for (fid, findex) in bindings {
                    let field_len = t.get_type_obj().map_or(0, |o| o.fields.len());
                    let field = t
                        .get_type_obj()
                        .and_then(|o| o.fields.get(fid.0))
                        .ok_or_else(|| invalid_ref("object field", fid.0, field_len))?;
                    let target = findexes
                        .get(findex.0)
                        .ok_or_else(|| invalid_ref("binding function", findex.0, findexes.len()))?;
                    if let RefFunKnown::Fun(x) = *target {
                        functions[x].name = field.name;
                        functions[x].parent = Some(RefType(i));
                    }
                }
            }
        }

        // Function names
        let mut fnames = HashMap::with_capacity(functions.len());
        for (i, f) in functions.iter().enumerate() {
            // FIXME duplicates ?
            if !f.name.is_null() {
                let name = strings
                    .get(f.name.0)
                    .ok_or_else(|| invalid_ref("function name", f.name.0, strings.len()))?;
                fnames.insert(name.clone(), i);
            }
        }
        let entry = findexes
            .get(entrypoint.0)
            .ok_or_else(|| invalid_ref("entrypoint", entrypoint.0, findexes.len()))?;
        fnames.insert(
            Str::from("init"),
            match *entry {
                RefFunKnown::Fun(x) => x,
                RefFunKnown::Native(_) => {
                    return Err(Error::MalformedBytecode(
                        "Entrypoint references a native function".into(),
                    ))
                }
            },
        );

        let globals_initializers = if let Some(constants) = &constants {
            let mut tmp = HashMap::with_capacity(constants.len());
            for (i, c) in constants.iter().enumerate() {
                tmp.insert(c.global, i);
            }
            tmp
        } else {
            HashMap::new()
        };

        let code = Bytecode {
            version,
            entrypoint,
            ints,
            floats,
            strings,
            bytes,
            debug_files,
            types,
            globals,
            natives,
            functions,
            constants,
            findexes,
            fnames,
            globals_initializers,
        };
        code.validate()?;
        Ok(code)
    }
}

impl RefInt {
    pub(crate) fn read(r: &mut impl Read) -> Result<Self> {
        Ok(Self(read_varu(r)? as usize))
    }
}

impl RefFloat {
    pub(crate) fn read(r: &mut impl Read) -> Result<Self> {
        Ok(Self(read_varu(r)? as usize))
    }
}

impl RefString {
    pub(crate) fn read(r: &mut impl Read) -> Result<Self> {
        Ok(Self(read_varu(r)? as usize))
    }
}

impl RefGlobal {
    pub(crate) fn read(r: &mut impl Read) -> Result<Self> {
        Ok(Self(read_varu(r)? as usize))
    }
}

impl RefFun {
    pub(crate) fn read(r: &mut impl Read) -> Result<Self> {
        Ok(Self(read_varu(r)? as usize))
    }
}

impl RefType {
    pub(crate) fn read(r: &mut impl Read) -> Result<Self> {
        Ok(Self(read_varu(r)? as usize))
    }
}

impl RefField {
    pub(crate) fn read(r: &mut impl Read) -> Result<Self> {
        Ok(Self(read_varu(r)? as usize))
    }
}

impl ObjField {
    pub(crate) fn read(r: &mut impl Read) -> Result<Self> {
        Ok(ObjField {
            name: RefString::read(r)?,
            t: RefType::read(r)?,
        })
    }
}

impl TypeFun {
    pub(crate) fn read(r: &mut impl Read) -> Result<Self> {
        let nargs = r.read_u8()?;
        let mut args = Vec::with_capacity(nargs as usize);
        for _ in 0..nargs {
            args.push(RefType::read(r)?);
        }
        Ok(TypeFun {
            args,
            ret: RefType::read(r)?,
        })
    }
}

impl TypeObj {
    pub(crate) fn read(r: &mut impl Read) -> Result<Self> {
        let name = RefString::read(r)?;
        let super_ = read_vari(r)?;
        let global = RefGlobal::read(r)?;
        let nfields = checked_count(r, "object field")?;
        let nprotos = checked_count(r, "object prototype")?;
        let nbindings = checked_count(r, "object binding")?;
        let mut own_fields = Vec::with_capacity(nfields);
        for _ in 0..nfields {
            own_fields.push(ObjField::read(r)?);
        }
        let mut protos = Vec::with_capacity(nprotos);
        for _ in 0..nprotos {
            protos.push(ObjProto {
                name: RefString::read(r)?,
                findex: RefFun::read(r)?,
                pindex: read_vari(r)?,
            });
        }
        let mut bindings = IndexMap::with_capacity(nbindings);
        for _ in 0..nbindings {
            let field = RefField::read(r)?;
            let function = RefFun::read(r)?;
            if bindings.insert(field, function).is_some() {
                return Err(Error::MalformedBytecode(format!(
                    "Duplicate object binding for field {}",
                    field.0
                )));
            }
        }
        Ok(TypeObj {
            name,
            super_: if super_ < 0 {
                None
            } else {
                Some(RefType(super_ as usize))
            },
            global,
            own_fields,
            fields: Vec::new(),
            protos,
            bindings,
        })
    }
}

impl Type {
    pub(crate) fn read(r: &mut impl Read) -> Result<Self> {
        use crate::Type::*;
        match r.read_u8()? {
            0 => Ok(Void),
            1 => Ok(UI8),
            2 => Ok(UI16),
            3 => Ok(I32),
            4 => Ok(I64),
            5 => Ok(F32),
            6 => Ok(F64),
            7 => Ok(Bool),
            8 => Ok(Bytes),
            9 => Ok(Dyn),
            10 => Ok(Fun(TypeFun::read(r)?)),
            11 => Ok(Obj(TypeObj::read(r)?)),
            12 => Ok(Array),
            13 => Ok(Type),
            14 => Ok(Ref(RefType::read(r)?)),
            15 => {
                let nfields = checked_count(r, "virtual field")?;
                let mut fields = Vec::with_capacity(nfields);
                for _ in 0..nfields {
                    fields.push(ObjField::read(r)?);
                }
                Ok(Virtual { fields })
            }
            16 => Ok(DynObj),
            17 => Ok(Abstract {
                name: RefString::read(r)?,
            }),
            18 => {
                let name = RefString::read(r)?;
                let global = RefGlobal::read(r)?;
                let nconstructs = checked_count(r, "enum constructor")?;
                let mut constructs = Vec::with_capacity(nconstructs);
                for _ in 0..nconstructs {
                    let name = RefString::read(r)?;
                    let nparams = checked_count(r, "enum parameter")?;
                    let mut params = Vec::with_capacity(nparams);
                    for _ in 0..nparams {
                        params.push(RefType::read(r)?);
                    }
                    constructs.push(EnumConstruct { name, params })
                }
                Ok(Enum {
                    name,
                    global,
                    constructs,
                })
            }
            19 => Ok(Null(RefType::read(r)?)),
            20 => Ok(Method(TypeFun::read(r)?)),
            21 => Ok(Struct(TypeObj::read(r)?)),
            22 => Ok(Packed(RefType::read(r)?)),
            23 => Ok(Guid),
            other => Err(Error::MalformedBytecode(format!(
                "Invalid type kind '{other}'"
            ))),
        }
    }
}

impl Native {
    pub(crate) fn read(r: &mut impl Read) -> Result<Self> {
        Ok(Native {
            lib: RefString::read(r)?,
            name: RefString::read(r)?,
            t: RefType::read(r)?,
            findex: RefFun::read(r)?,
        })
    }
}

impl Function {
    pub(crate) fn read(r: &mut impl Read, has_debug: bool, version: u8) -> Result<Self> {
        let t = RefType::read(r)?;
        let findex = RefFun::read(r)?;
        let nregs = checked_count(r, "function register")?;
        let nops = checked_count(r, "function opcode")?;
        let mut regs = Vec::with_capacity(nregs);
        for _ in 0..nregs {
            regs.push(RefType::read(r)?);
        }
        let mut ops = Vec::with_capacity(nops);
        for _ in 0..nops {
            ops.push(Opcode::read(r)?);
        }

        // This is extracted from the hashlink source code, do not count on me to explain what it does
        let debug_info = if has_debug {
            let mut tmp = Vec::with_capacity(nops);
            let mut currfile: i32 = -1;
            let mut currline: i32 = 0;
            let mut i = 0;
            while i < nops {
                let mut c = r.read_u8()? as i32;
                if c & 1 != 0 {
                    c >>= 1;
                    currfile = (c << 8) | (r.read_u8()? as i32);
                } else if c & 2 != 0 {
                    let delta = c >> 6;
                    let mut count = (c >> 2) & 15;
                    while count > 0 {
                        count -= 1;
                        tmp.push((currfile as usize, currline as usize));
                        i += 1;
                    }
                    currline += delta;
                } else if c & 4 != 0 {
                    currline += c >> 3;
                    tmp.push((currfile as usize, currline as usize));
                    i += 1;
                } else {
                    let b2 = r.read_u8()? as i32;
                    let b3 = r.read_u8()? as i32;
                    currline = (c >> 3) | (b2 << 5) | (b3 << 13);
                    tmp.push((currfile as usize, currline as usize));
                    i += 1;
                }
            }
            Some(tmp)
        } else {
            None
        };

        let assigns = if has_debug && version >= 3 {
            let len = checked_count(r, "debug assignment")?;
            let mut assigns = Vec::with_capacity(len);
            for _ in 0..len {
                assigns.push((RefString::read(r)?, read_vari(r)? as usize));
            }
            Some(assigns)
        } else {
            None
        };
        Ok(Function {
            name: RefString(0),
            t,
            findex,
            regs,
            ops,
            debug_info,
            assigns,
            parent: None,
        })
    }
}

impl ConstantDef {
    pub(crate) fn read(r: &mut impl Read) -> Result<Self> {
        let global = RefGlobal::read(r)?;
        let nfields = checked_count(r, "constant field")?;
        let mut fields = Vec::with_capacity(nfields);
        for _ in 0..nfields {
            fields.push(read_varu(r)? as usize);
        }
        Ok(ConstantDef { global, fields })
    }
}

pub(crate) fn read_vari(r: &mut impl Read) -> Result<i32> {
    let b = r.read_u8()? as i32;
    if b & 0x80 == 0 {
        Ok(b & 0x7F)
    } else if b & 0x40 == 0 {
        let v = r.read_u8()? as i32 | ((b & 31) << 8);
        Ok(if b & 0x20 == 0 { v } else { -v })
    } else {
        let c = r.read_u8()? as i32;
        let d = r.read_u8()? as i32;
        let e = r.read_u8()? as i32;
        let v = ((b & 31) << 24) | (c << 16) | (d << 8) | e;
        Ok(if b & 0x20 == 0 { v } else { -v })
    }
}

pub(crate) fn read_varu(r: &mut impl Read) -> Result<u32> {
    let i = read_vari(r)?;
    if i < 0 {
        Err(Error::MalformedBytecode(format!(
            "Got negative index '{i}' (expected >= 0)"
        )))
    } else {
        Ok(i as u32)
    }
}

fn read_strings(r: &mut impl Read, nstrings: usize) -> Result<Vec<Str>> {
    let mut strings = Vec::with_capacity(nstrings);
    let mut string_data = vec![0u8; checked_blob_len(r, "string")?];
    r.read_exact(&mut string_data)?;
    let mut acc: usize = 0;
    for _ in 0..nstrings {
        let ssize = read_varu(r)? as usize + 1;
        //println!("size: {ssize} {:?}", &string_data[acc..(acc + ssize)]);
        //let cstr = unsafe { CStr::from_bytes_with_nul_unchecked(&string_data[acc..(acc + ssize)]) };
        let end = acc
            .checked_add(ssize)
            .ok_or_else(|| Error::MalformedBytecode("String table offset overflow".into()))?;
        if end > string_data.len() || ssize == 0 {
            return Err(Error::MalformedBytecode(format!(
                "String table entry extends past blob (end {end}, blob {})",
                string_data.len()
            )));
        }
        if string_data[end - 1] != 0 {
            return Err(Error::MalformedBytecode(
                "String table entry is missing its NUL delimiter".into(),
            ));
        }
        strings.push(Str::from_ref_counted(std::sync::Arc::<str>::from(
            from_utf8(&string_data[acc..end - 1])?,
        )));
        acc += ssize;
    }
    if acc != string_data.len() {
        return Err(Error::MalformedBytecode(format!(
            "String table contains {} unreferenced trailing bytes",
            string_data.len() - acc
        )));
    }
    Ok(strings)
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::Bytecode;

    #[test]
    fn test_deserialize_all() {
        let dir = "../../data";
        let mut discovered = 0;
        for entry in fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if let Some(ext) = path.extension() {
                if ext == "hl" {
                    discovered += 1;
                    let code = Bytecode::from_file(&path);
                    assert!(code.is_ok(), "{}: {:?}", path.display(), code.err());
                }
            }
        }
        assert!(
            discovered > 0,
            "zero .hl fixtures found; compile data/*.hx first"
        );
    }

    #[test]
    fn test_deserialize_optional_stress_input() {
        let Some(path) = std::env::var_os("HLBC_HLBOOT") else {
            return;
        };
        let code = Bytecode::from_file(&path)
            .unwrap_or_else(|error| panic!("{}: {error:?}", std::path::Path::new(&path).display()));
        let original = fs::read(&path).expect("read stress input");
        let mut serialized = Vec::new();
        code.serialize(&mut serialized)
            .expect("serialize stress input");
        assert_eq!(
            serialized, original,
            "stress input changed during round trip"
        );
    }

    #[test]
    fn test_special_opcodes() -> crate::Result<()> {
        let path = "../../data/SpecialOpcodes.hl";
        if !std::path::Path::new(path).is_file() {
            return Ok(());
        }
        let code = Bytecode::from_file(path)?;
        dbg!(&code.function_by_name("main").unwrap().ops);
        Ok(())
    }
}
