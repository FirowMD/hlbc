use std::io::Write;

use byteorder::{LittleEndian, WriteBytesExt};

use crate::types::{RefField, RefFloat, RefFun, RefGlobal, RefInt, RefString, RefType, TypeFun};
use crate::{Bytecode, ConstantDef, Function, Native, ObjField, Str, Type, TypeObj};
use crate::{Error, Result};

impl Bytecode {
    /// Serialize the bytecode to any sink.
    /// Bytecode is serialized to the same format.
    pub fn serialize(&self, w: &mut impl Write) -> Result<()> {
        self.validate()?;
        w.write_all(&[b'H', b'L', b'B'])?;
        w.write_u8(self.version)?;
        write_var(w, if self.debug_files.is_some() { 1 } else { 0 })?;
        write_var(w, self.ints.len() as i32)?;
        write_var(w, self.floats.len() as i32)?;
        write_var(w, self.strings.len() as i32)?;
        if self.version >= 5 {
            write_count(
                w,
                self.bytes.as_ref().map_or(0, |(_, pos)| pos.len()),
                "bytes",
            )?;
        }
        write_var(w, self.types.len() as i32)?;
        write_var(w, self.globals.len() as i32)?;
        write_var(w, self.natives.len() as i32)?;
        write_var(w, self.functions.len() as i32)?;
        if self.version >= 4 {
            write_count(w, self.constants.as_ref().map_or(0, Vec::len), "constant")?;
        }
        self.entrypoint.write(w)?;
        for &i in &self.ints {
            w.write_i32::<LittleEndian>(i)?;
        }
        for &f in &self.floats {
            w.write_f64::<LittleEndian>(f)?;
        }
        write_strings(w, &self.strings)?;
        if self.version >= 5 {
            let (bytes, pos) = self
                .bytes
                .as_ref()
                .map_or((&[][..], &[][..]), |(b, p)| (b.as_slice(), p.as_slice()));
            let byte_len = i32::try_from(bytes.len()).map_err(|_| Error::CountLimit {
                kind: "bytes blob",
                count: bytes.len(),
                limit: i32::MAX as usize,
            })?;
            w.write_i32::<LittleEndian>(byte_len)?;
            w.write_all(bytes)?;
            for &p in pos {
                write_var(w, p as i32)?;
            }
        }
        if let Some(debug_files) = &self.debug_files {
            write_var(w, debug_files.len() as i32)?;
            write_strings(w, debug_files)?;
        }
        for t in &self.types {
            t.write(w)?;
        }
        for g in &self.globals {
            g.write(w)?;
        }
        for n in &self.natives {
            n.write(w)?;
        }
        for f in &self.functions {
            f.write(w, self.debug_files.is_some(), self.version)?;
        }
        if let Some(constants) = &self.constants {
            for c in constants {
                c.write(w)?;
            }
        }
        Ok(())
    }
}

impl RefInt {
    pub(crate) fn write(&self, w: &mut impl Write) -> Result<()> {
        write_var(w, self.0 as i32)
    }
}

impl RefFloat {
    pub(crate) fn write(&self, w: &mut impl Write) -> Result<()> {
        write_var(w, self.0 as i32)
    }
}

impl RefString {
    pub(crate) fn write(&self, w: &mut impl Write) -> Result<()> {
        write_var(w, self.0 as i32)
    }
}

impl RefGlobal {
    pub(crate) fn write(&self, w: &mut impl Write) -> Result<()> {
        write_var(w, self.0 as i32)
    }
}

impl RefFun {
    pub(crate) fn write(&self, w: &mut impl Write) -> Result<()> {
        write_var(w, self.0 as i32)
    }
}

impl RefType {
    pub(crate) fn write(&self, w: &mut impl Write) -> Result<()> {
        write_var(w, self.0 as i32)
    }
}

impl RefField {
    pub(crate) fn write(&self, w: &mut impl Write) -> Result<()> {
        write_var(w, self.0 as i32)
    }
}

impl ObjField {
    pub(crate) fn write(&self, w: &mut impl Write) -> Result<()> {
        self.name.write(w)?;
        self.t.write(w)
    }
}

impl TypeFun {
    pub(crate) fn write(&self, w: &mut impl Write) -> Result<()> {
        let count = u8::try_from(self.args.len()).map_err(|_| Error::CountLimit {
            kind: "function argument",
            count: self.args.len(),
            limit: u8::MAX as usize,
        })?;
        w.write_u8(count)?;
        for arg in &self.args {
            arg.write(w)?;
        }
        self.ret.write(w)
    }
}

impl TypeObj {
    pub(crate) fn write(&self, w: &mut impl Write) -> Result<()> {
        self.name.write(w)?;
        write_var(w, self.super_.map(|s| s.0 as i32).unwrap_or(-1))?;
        self.global.write(w)?;
        write_var(w, self.own_fields.len() as i32)?;
        write_var(w, self.protos.len() as i32)?;
        write_var(w, self.bindings.len() as i32)?;
        for f in &self.own_fields {
            f.write(w)?;
        }
        for p in &self.protos {
            p.name.write(w)?;
            p.findex.write(w)?;
            write_var(w, p.pindex)?;
        }
        for (fi, fun) in &self.bindings {
            fi.write(w)?;
            fun.write(w)?;
        }
        Ok(())
    }
}

impl Type {
    pub(crate) fn write(&self, w: &mut impl Write) -> Result<()> {
        match self {
            Type::Void => w.write_u8(0)?,
            Type::UI8 => w.write_u8(1)?,
            Type::UI16 => w.write_u8(2)?,
            Type::I32 => w.write_u8(3)?,
            Type::I64 => w.write_u8(4)?,
            Type::F32 => w.write_u8(5)?,
            Type::F64 => w.write_u8(6)?,
            Type::Bool => w.write_u8(7)?,
            Type::Bytes => w.write_u8(8)?,
            Type::Dyn => w.write_u8(9)?,
            Type::Fun(fun) => {
                w.write_u8(10)?;
                fun.write(w)?;
            }
            Type::Obj(obj) => {
                w.write_u8(11)?;
                obj.write(w)?;
            }
            Type::Array => w.write_u8(12)?,
            Type::Type => w.write_u8(13)?,
            Type::Ref(inner) => {
                w.write_u8(14)?;
                inner.write(w)?;
            }
            Type::Virtual { fields } => {
                w.write_u8(15)?;
                write_var(w, fields.len() as i32)?;
                for f in fields {
                    f.write(w)?;
                }
            }
            Type::DynObj => w.write_u8(16)?,
            Type::Abstract { name } => {
                w.write_u8(17)?;
                name.write(w)?;
            }
            Type::Enum {
                name,
                global,
                constructs,
            } => {
                w.write_u8(18)?;
                name.write(w)?;
                global.write(w)?;
                write_var(w, constructs.len() as i32)?;
                for c in constructs {
                    c.name.write(w)?;
                    write_var(w, c.params.len() as i32)?;
                    for p in &c.params {
                        p.write(w)?;
                    }
                }
            }
            Type::Null(inner) => {
                w.write_u8(19)?;
                inner.write(w)?;
            }
            Type::Method(fun) => {
                w.write_u8(20)?;
                fun.write(w)?;
            }
            Type::Struct(obj) => {
                w.write_u8(21)?;
                obj.write(w)?;
            }
            Type::Packed(inner) => {
                w.write_u8(22)?;
                inner.write(w)?;
            }
            Type::Guid => w.write_u8(23)?,
        }
        Ok(())
    }
}

impl Native {
    pub(crate) fn write(&self, w: &mut impl Write) -> Result<()> {
        self.lib.write(w)?;
        self.name.write(w)?;
        self.t.write(w)?;
        self.findex.write(w)
    }
}

impl Function {
    pub(crate) fn write(&self, w: &mut impl Write, has_debug: bool, version: u8) -> Result<()> {
        self.t.write(w)?;
        self.findex.write(w)?;
        write_var(w, self.regs.len() as i32)?;
        write_var(w, self.ops.len() as i32)?;
        for r in &self.regs {
            r.write(w)?;
        }
        for o in &self.ops {
            o.write(w)?;
        }
        // https://github.com/HaxeFoundation/haxe/blob/ea57ab1ef60d212228c8657b7bc5b1085c62714e/src/generators/genhl.ml#L3910
        if has_debug {
            let debug_info = self.debug_info.as_ref().ok_or_else(|| {
                Error::MalformedBytecode(format!(
                    "Function {} is missing debug information",
                    self.findex.0
                ))
            })?;
            let mut curfile: i32 = -1;
            let mut curpos = 0;
            let mut rcount = 0;
            for &(f, p) in debug_info {
                if f as i32 != curfile {
                    flush_repeat(w, &mut curpos, &mut rcount, p)?;
                    curfile = f as i32;
                    w.write_u8(((f >> 7) | 1) as u8)?;
                    w.write_u8((f & 0xFF) as u8)?;
                }
                if p != curpos {
                    flush_repeat(w, &mut curpos, &mut rcount, p)?;
                }
                if p == curpos {
                    rcount += 1;
                } else {
                    let delta = p as i32 - curpos as i32;
                    if delta > 0 && delta < 32 {
                        w.write_u8(((delta << 3) | 4) as u8)?;
                    } else {
                        w.write_u8((p << 3) as u8)?;
                        w.write_u8((p >> 5) as u8)?;
                        w.write_u8((p >> 13) as u8)?;
                    }
                    curpos = p;
                }
            }
            let old_curpos = curpos;
            flush_repeat(w, &mut curpos, &mut rcount, old_curpos)?;
        }
        if has_debug && version >= 3 {
            let assigns = self.assigns.as_ref().ok_or_else(|| {
                Error::MalformedBytecode(format!(
                    "Function {} is missing debug assignments",
                    self.findex.0
                ))
            })?;
            write_var(w, assigns.len() as i32)?;
            for (s, p) in assigns {
                s.write(w)?;
                write_var(w, *p as i32)?;
            }
        }
        Ok(())
    }
}

impl ConstantDef {
    pub(crate) fn write(&self, w: &mut impl Write) -> Result<()> {
        self.global.write(w)?;
        write_var(w, self.fields.len() as i32)?;
        for f in &self.fields {
            write_var(w, *f as i32)?;
        }
        Ok(())
    }
}

// https://github.com/HaxeFoundation/haxe/blob/613b0291c4976a8169aa643cdcc408c7d6b69da9/src/generators/genhl.ml#L3698
/// Write an index with a variable size encoding
pub(crate) fn write_var(w: &mut impl Write, value: i32) -> Result<()> {
    if value < 0 {
        let value = value.checked_abs().ok_or(Error::ValueOutOfBounds {
            value,
            limit: 0x20000000,
        })?;
        if value < 0x2000 {
            w.write_u8(((value >> 8) | 0xA0) as u8)?;
            w.write_u8((value & 0xFF) as u8)?;
        } else if value < 0x20000000 {
            w.write_u8(((value >> 24) | 0xE0) as u8)?;
            w.write_u8(((value >> 16) & 0xFF) as u8)?;
            w.write_u8(((value >> 8) & 0xFF) as u8)?;
            w.write_u8((value & 0xFF) as u8)?;
        } else {
            return Err(Error::ValueOutOfBounds {
                value,
                limit: 0x20000000,
            });
        }
    } else if value < 0x80 {
        w.write_u8(value as u8)?;
    } else if value < 0x2000 {
        w.write_u8(((value >> 8) | 0x80) as u8)?;
        w.write_u8((value & 0xFF) as u8)?;
    } else if value < 0x20000000 {
        w.write_u8(((value >> 24) | 0xC0) as u8)?;
        w.write_u8(((value >> 16) & 0xFF) as u8)?;
        w.write_u8(((value >> 8) & 0xFF) as u8)?;
        w.write_u8((value & 0xFF) as u8)?;
    } else {
        return Err(Error::ValueOutOfBounds {
            value,
            limit: 0x20000000,
        });
    }
    Ok(())
}

pub(crate) fn write_strings(w: &mut impl Write, strings: &[Str]) -> Result<()> {
    let size = strings.iter().try_fold(0usize, |size, string| {
        size.checked_add(string.len() + 1)
            .ok_or(Error::ValueOutOfBounds {
                value: i32::MAX,
                limit: i32::MAX as u32,
            })
    })?;
    let size = i32::try_from(size).map_err(|_| Error::ValueOutOfBounds {
        value: i32::MAX,
        limit: i32::MAX as u32,
    })?;
    w.write_i32::<LittleEndian>(size)?;
    for string in strings {
        w.write_all(string.as_bytes())?;
        w.write_all(&[0])?;
    }
    for string in strings {
        write_count(w, string.len(), "string byte")?;
    }
    Ok(())
}

pub(crate) fn write_count(w: &mut impl Write, value: usize, kind: &'static str) -> Result<()> {
    let value = i32::try_from(value)
        .map_err(|_| Error::MalformedBytecode(format!("{kind} count {value} cannot be encoded")))?;
    write_var(w, value)
}

// Adapted from https://github.com/HaxeFoundation/haxe/blob/ea57ab1ef60d212228c8657b7bc5b1085c62714e/src/generators/genhl.ml#L3895
fn flush_repeat(
    w: &mut impl Write,
    curpos: &mut usize,
    rcount: &mut usize,
    pos: usize,
) -> Result<()> {
    if *rcount > 0 {
        if *rcount > 15 {
            w.write_u8((15 << 2) | 2)?;
            *rcount -= 15;
            flush_repeat(w, curpos, rcount, pos)?;
        } else {
            let mut delta = pos as i32 - *curpos as i32;
            delta = if delta > 0 && delta < 4 { delta } else { 0 };
            w.write_u8(((delta << 6) | ((*rcount as i32) << 2) | 2) as u8)?;
            *rcount = 0;
            *curpos += delta as usize;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::io::Cursor;

    use crate::{Bytecode, Error};

    use super::write_var;

    #[test]
    fn negative_four_byte_varints_use_hashlink_range() {
        for value in [-20_000_000, -0x1FFF_FFFF] {
            let mut encoded = Vec::new();
            write_var(&mut encoded, value).unwrap();
            assert_eq!(encoded.len(), 4);
            assert_eq!(
                crate::read::read_vari(&mut Cursor::new(encoded)).unwrap(),
                value
            );
        }

        let error = write_var(&mut Vec::new(), -0x2000_0000).unwrap_err();
        assert!(matches!(
            error,
            Error::ValueOutOfBounds {
                value: 0x2000_0000,
                limit: 0x2000_0000
            }
        ));
    }

    #[test]
    fn ser_eq_deser() {
        let data = fs::read("../../data/Anonymous.hl").unwrap();
        // Deserialize
        let code = Bytecode::deserialize(&mut data.as_slice()).unwrap();

        let mut out = Vec::with_capacity(data.len());
        // Then serialize back again
        code.serialize(&mut out).unwrap();

        // Test bytes
        assert_eq!(data, out);
    }

    #[test]
    fn ser_eq_deser_all() {
        for entry in fs::read_dir("../../data").unwrap() {
            let path = entry.unwrap().path();
            if let Some(ext) = path.extension() {
                if ext == "hl" {
                    let data = fs::read(&path).unwrap();
                    let code = Bytecode::deserialize(&mut data.as_slice()).unwrap();
                    let mut out = Vec::with_capacity(data.len());
                    code.serialize(&mut out).unwrap();
                    let new = Bytecode::deserialize(&mut out.as_slice()).unwrap();
                    assert_eq!(data, out);
                    assert_eq!(code.version, new.version);
                }
            }
        }
    }
}
