use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Data, DeriveInput, Expr, ExprLit, GenericArgument, Ident, Lit, LitStr, PathArguments, Type,
    Variant,
};

#[proc_macro_derive(OpcodeHelper)]
pub fn derive_opcode_helper(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse_macro_input!(input as DeriveInput);
    let variants = match &ast.data {
        Data::Enum(v) => Some(&v.variants),
        _ => None,
    }
    .unwrap();

    let name = &ast.ident;
    let opcode_numbers = 0..variants.len() as u8;
    let opcode_numbers2 = opcode_numbers.clone();
    let opcode_count = variants.len();

    let initr = variants.iter().map(|v| read_variant(name, v));
    let initw = variants
        .iter()
        .enumerate()
        .map(|(i, v)| write_variant(name, v, i as u8));
    let vname = variants.iter().map(|v| &v.ident);
    let vname2 = vname.clone();
    let vname3 = vname.clone();
    let vname_str = variants
        .iter()
        .map(|v| LitStr::new(&v.ident.to_string(), v.ident.span()));
    let vname_str2 = vname_str.clone();
    let metadata = variants.iter().enumerate().map(|(opcode, v)| {
        let name = LitStr::new(&v.ident.to_string(), v.ident.span());
        let operands = v.fields.iter().map(|field| {
            let field_name = field.ident.as_ref().expect("opcode fields must be named");
            let field_name = LitStr::new(&field_name.to_string(), field_name.span());
            let kind = LitStr::new(&ident(&field.ty), field_name.span());
            quote! {
                crate::opcodes::OperandMetadata { name: #field_name, kind: #kind }
            }
        });
        quote! {
            crate::opcodes::OpcodeMetadata {
                code: #opcode as u8,
                name: #name,
                operands: &[ #( #operands, )* ],
                semantics: &crate::opcodes::OPCODE_SEMANTICS[#opcode],
            }
        }
    });
    let operand_values = variants.iter().map(|v| {
        let vname = &v.ident;
        let field_names: Vec<_> = v.fields.iter().map(|f| f.ident.as_ref().unwrap()).collect();
        let values = v.fields.iter().map(|field| {
            let field_name = field.ident.as_ref().unwrap();
            let field_name_literal = LitStr::new(&field_name.to_string(), field_name.span());
            quote! {
                crate::opcodes::OpcodeOperand {
                    name: #field_name_literal,
                    value: format!("{:?}", #field_name),
                }
            }
        });
        quote! {
            #name::#vname { #( #field_names, )* } => vec![ #( #values, )* ]
        }
    });
    let register_operands = variants.iter().map(|v| {
        let vname = &v.ident;
        let field_names: Vec<_> = v.fields.iter().map(|f| f.ident.as_ref().unwrap()).collect();
        let register_values = v.fields.iter().filter_map(|field| {
            let field_name = field.ident.as_ref().unwrap();
            let field_name_literal = LitStr::new(&field_name.to_string(), field_name.span());
            match ident(&field.ty).as_str() {
                "Reg" => Some(quote! {
                    registers.push(crate::opcodes::OpcodeRegisterOperand {
                        name: #field_name_literal,
                        register: *#field_name,
                    });
                }),
                "Vec<Reg>" => Some(quote! {
                    registers.extend(#field_name.iter().copied().map(|register| {
                        crate::opcodes::OpcodeRegisterOperand {
                            name: #field_name_literal,
                            register,
                        }
                    }));
                }),
                _ => None,
            }
        });
        quote! {
            #name::#vname { #( #field_names, )* } => {
                let _ = ( #( #field_names, )* );
                #( #register_values )*
            }
        }
    });
    let all_defaults = variants.iter().map(|v| {
        let vname = &v.ident;
        let finit = v.fields.iter().map(|f| {
            let fname = f.ident.as_ref().unwrap();
            quote! { #fname: Default::default() }
        });
        quote! { #name::#vname { #( #finit, )* } }
    });
    let vdesc = variants.iter().map(|v| {
        let mut acc = String::new();
        for attr in &v.attrs {
            if let Ok(nv) = attr.meta.require_name_value() {
                if nv.path.is_ident("doc") {
                    match &nv.value {
                        Expr::Lit(ExprLit {
                            lit: Lit::Str(lit), ..
                        }) => {
                            let lstr = lit.value();
                            let to_acc = lstr.trim();
                            if !to_acc.is_empty() {
                                acc.push_str(to_acc);
                                acc.push('\n');
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        acc.trim().to_string()
    });
    let vdefault_init = variants.iter().map(|v| {
        let vname = &v.ident;
        let finit = v.fields.iter().map(|f| {
            let fname = f.ident.as_ref().unwrap();
            quote! {
                #fname: Default::default()
            }
        });
        quote! {
            #name::#vname { #( #finit,)* }
        }
    });

    proc_macro::TokenStream::from(quote! {
        impl #name {
            /// Number of real HashLink opcodes (the `OLast` sentinel is excluded).
            pub const COUNT: usize = #opcode_count;

            /// Declarative metadata in wire-format opcode order.
            pub const METADATA: &'static [crate::opcodes::OpcodeMetadata] = &[
                #( #metadata, )*
            ];

            /// Decode an instruction
            pub fn read(r: &mut impl std::io::Read) -> crate::Result<#name> {

                use byteorder::ReadBytesExt;
                use crate::types::*;
                use crate::read::{read_vari, read_varu};

                let op = r.read_u8()?;
                match op {
                    #( #opcode_numbers => #initr, )*
                    other => Err(crate::Error::MalformedBytecode(format!("Unknown opcode {}", op))),
                }
            }

            /// Encode an instruction
            pub fn write(&self, w: &mut impl std::io::Write) -> crate::Result<()> {

                use byteorder::WriteBytesExt;
                use crate::types::*;
                use crate::write::write_var;

                match self {
                    #( #initw )*
                }

                Ok(())
            }

            /// Get the opcode name
            pub fn name(&self) -> &'static str {
                match self {
                    #( #name::#vname { .. } => #vname_str, )*
                }
            }

            /// Get the opcode description
            pub fn description(&self) -> &'static str {
                match self {
                    #( #name::#vname2 { .. } => #vdesc, )*
                }
            }

            /// Get the opcode number used by the HashLink bytecode format.
            pub fn code(&self) -> u8 {
                match self {
                    #( #name::#vname3 { .. } => #opcode_numbers2, )*
                }
            }

            /// Metadata for this opcode, including ordered operand descriptions.
            pub fn metadata(&self) -> &'static crate::opcodes::OpcodeMetadata {
                &Self::METADATA[self.code() as usize]
            }

            /// Structured operand values used by diagnostics and reports.
            pub fn operands(&self) -> Vec<crate::opcodes::OpcodeOperand> {
                match self {
                    #( #operand_values, )*
                }
            }

            /// Return every register physically encoded by this opcode.
            pub fn register_operands(&self) -> Vec<crate::opcodes::OpcodeRegisterOperand> {
                let mut registers = Vec::new();
                match self {
                    #( #register_operands, )*
                }
                registers
            }

            /// Construct one default value for every known opcode.
            pub fn all_defaults() -> Vec<Self> {
                vec![ #( #all_defaults, )* ]
            }

            /// Get an opcode from its name. Returns a default value for the variant.
            pub fn from_name(name: &str) -> Option<Self> {
                match name {
                    #( #vname_str2 => Some(#vdefault_init), )*
                    _ => None
                }
            }
        }
    })
}

/// Print a type to string
fn ident(ty: &Type) -> String {
    match ty {
        Type::Path(path) => {
            let seg = &path.path.segments[0];
            match &seg.arguments {
                PathArguments::None => seg.ident.to_string(),
                PathArguments::AngleBracketed(a) => {
                    let a = match &a.args[0] {
                        GenericArgument::Type(ty) => ident(ty),
                        _ => unreachable!(),
                    };
                    format!("{}<{}>", seg.ident, a)
                }
                _ => unreachable!(),
            }
        }
        other => unreachable!("unknown type {:?}", other),
    }
}

fn read_variant(enum_name: &Ident, v: &Variant) -> TokenStream {
    let rvi32 = quote!(read_vari(r)?);
    let rvu32 = quote!(read_varu(r)?);
    let reg = quote!(Reg(#rvi32 as u32));

    let vname = &v.ident;
    let fname = v.fields.iter().map(|f| &f.ident);
    let fvalue = v.fields.iter().map(|f| match ident(&f.ty).as_str() {
        "InlineBool" => quote! {
            #rvi32 == 1
        },
        "InlineInt" => quote! {
            #rvi32
        },
        "JumpOffset" => quote! {
            #rvi32
        },
        "SwitchOffset" => quote! {
            #rvu32
        },
        "Vec<JumpOffset>" => quote! {
            {
                let n = #rvu32 as usize;
                let mut offsets = Vec::with_capacity(n);
                for _ in 0..n {
                    offsets.push(#rvi32 as JumpOffset);
                }
                offsets
            }
        },
        "Vec<SwitchOffset>" => quote! {
            {
                let n = crate::read::checked_count(r, "switch offset")?;
                let mut offsets = Vec::with_capacity(n);
                for _ in 0..n {
                    offsets.push(#rvu32 as SwitchOffset);
                }
                offsets
            }
        },
        "Reg" => reg.clone(),
        "Vec<Reg>" => quote! {
            {
                let n = r.read_u8()? as usize;
                let mut regs = Vec::with_capacity(n);
                for _ in 0..n {
                    regs.push(#reg);
                }
                regs
            }
        },
        "RefInt" => quote! {
            RefInt::read(r)?
        },
        "RefFloat" => quote! {
            RefFloat::read(r)?
        },
        "RefBytes" => quote! {
            RefBytes(#rvi32 as usize)
        },
        "RefString" => quote! {
            RefString::read(r)?
        },
        "RefType" => quote! {
            RefType::read(r)?
        },
        "RefFun" => quote! {
            RefFun::read(r)?
        },
        "RefField" => quote! {
            RefField::read(r)?
        },
        "RefGlobal" => quote! {
            RefGlobal::read(r)?
        },
        "RefEnumConstruct" => quote! {
            RefEnumConstruct(#rvi32 as usize)
        },
        _ => TokenStream::default(),
    });
    quote! {
        Ok(#enum_name::#vname {
            #( #fname: #fvalue, )*
        })
    }
}

fn write_variant(enum_name: &Ident, v: &Variant, i: u8) -> TokenStream {
    let vname = &v.ident;
    let fname = v.fields.iter().map(|f| &f.ident);
    let fwrite = v.fields.iter().map(|f| {
        let fname = f.ident.as_ref().unwrap();
        match ident(&f.ty).as_str() {
            "InlineBool" => quote! {
                write_var(w, if *#fname { 1 } else { 0 })?;
            },
            "InlineInt" => quote! {
                write_var(w, *#fname as i32)?;
            },
            "usize" => quote!(write_var(w, #fname as i32)?;),
            "i32" => quote! {
                write_var(w, #fname)?;
            },
            "JumpOffset" => quote! {
                write_var(w, *#fname as i32)?;
            },
            "SwitchOffset" => quote! {
                write_var(w, i32::try_from(*#fname).map_err(|_| crate::Error::ValueOutOfBounds {
                    value: i32::MAX,
                    limit: 0x20000000,
                })?)?;
            },
            "Vec<JumpOffset>" => quote! {
                {
                    write_var(w, #fname.len() as i32)?;
                    for r__ in #fname {
                        write_var(w, *r__ as i32)?;
                    }
                }
            },
            "Vec<SwitchOffset>" => quote! {
                {
                    crate::write::write_count(w, #fname.len(), "switch offset")?;
                    for r__ in #fname {
                        write_var(w, i32::try_from(*r__).map_err(|_| crate::Error::ValueOutOfBounds {
                            value: i32::MAX,
                            limit: 0x20000000,
                        })?)?;
                    }
                }
            },
            "Reg" => quote! {
                write_var(w, #fname.0 as i32)?;
            },
            "Vec<Reg>" => quote! {
                {
                    let count__ = u8::try_from(#fname.len()).map_err(|_| crate::Error::CountLimit {
                        kind: "opcode register argument",
                        count: #fname.len(),
                        limit: u8::MAX as usize,
                    })?;
                    w.write_u8(count__)?;
                    for r__ in #fname {
                        write_var(w, r__.0 as i32)?;
                    }
                }
            },
            "RefInt" | "RefFloat" | "RefString" | "RefType" | "RefFun" | "RefField"
            | "RefGlobal" => quote! {
                #fname.write(w)?;
            },
            "RefBytes" => quote! {
                write_var(w, #fname.0 as i32)?;
            },
            "ValBool" => quote! {
                write_var(w, if #fname.0 { 1 } else { 0 })?;
            },
            "RefEnumConstruct" => quote! {
                write_var(w, #fname.0 as i32)?;
            },
            _ => TokenStream::default(),
        }
    });
    quote! {
        #enum_name::#vname { #( #fname, )* } => {
            w.write_u8(#i)?;
            #( #fwrite )*
        }
    }
}
