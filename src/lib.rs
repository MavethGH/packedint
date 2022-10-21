use std::num::NonZeroU32;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, ToTokens};
use syn::{
    braced,
    parse::{Parse, ParseBuffer, ParseStream},
    parse_macro_input, Ident, LitInt, Token, Visibility,
};

#[derive(Clone, Copy)]
enum IntType {
    U8,
    U16,
    U32,
    U64,
    U128,
}

impl Parse for IntType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lit: LitInt = input.parse()?;
        let number = lit.base10_parse::<u32>()?;
        match number {
            8 => Ok(Self::U8),
            16 => Ok(Self::U16),
            32 => Ok(Self::U32),
            64 => Ok(Self::U64),
            128 => Ok(Self::U128),
            _ => Err(syn::parse::Error::new(
                lit.span(),
                "size of packed int must be 8, 16, 32, 64, or 128",
            )),
        }
    }
}

impl IntType {
    pub fn as_u32(&self) -> u32 {
        match self {
            IntType::U8 => 8,
            IntType::U16 => 16,
            IntType::U32 => 32,
            IntType::U64 => 64,
            IntType::U128 => 128,
        }
    }
}

impl ToTokens for IntType {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let token = format!("u{}", self.as_u32());
        token.to_tokens(tokens);
    }
}

#[derive(Clone)]
struct StructField {
    visibility: Visibility,
    name: Ident,
    bit_size: NonZeroU32,
}

impl Parse for StructField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let visibility = input.parse()?;
        let name = input.parse()?;
        let lit: LitInt = input.parse()?;
        let bit_size = NonZeroU32::new(lit.base10_parse()?).ok_or_else(|| {
            syn::parse::Error::new(
                lit.span(),
                "fields of a packed int struct cannot be 0 bits in size",
            )
        })?;
        Ok(Self {
            visibility,
            name,
            bit_size,
        })
    }
}

/// Syntax
///
///     packed_int! {
///         $VISIBILITY struct $NAME: $STRUCT_SIZE {
///             // Any number of fields as long as the total bits does not exceed $STRUCT_SIZE
///             $FIELD_VISIBILITY $FIELD_NAME: $BIT_SIZE
///         }
///     };
///
struct PackedInt {
    visibility: Visibility,
    name: Ident,
    int_type: IntType,
    fields: Vec<StructField>,
}

impl Parse for PackedInt {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let visibility = input.parse()?;
        input.parse::<Token![struct]>()?;
        let name = input.parse()?;
        input.parse::<Token![:]>()?;
        let struct_size: IntType = input.parse()?;

        let content: ParseBuffer;
        let _ = braced!(content in input);

        let mut total_bits = 0;
        let fields = content
            .parse_terminated::<_, Token![,]>(StructField::parse)?
            .into_iter()
            .map(|f| {
                total_bits += f.bit_size.get();
                f
            })
            .collect();

        if total_bits > struct_size.as_u32() {
            return Err(content.error("the size of the packed int is too small to hold its fields"));
        }

        Ok(Self {
            visibility,
            name,
            int_type: struct_size,
            fields,
        })
    }
}

#[proc_macro]
pub fn packed_int(input: TokenStream) -> TokenStream {
    let PackedInt {
        visibility,
        name,
        int_type,
        fields,
    } = parse_macro_input!(input as PackedInt);

    let mut total_bits = 0;
    let mut field_fns = Vec::with_capacity(fields.len());

    for field in &fields {
        let fns = generate_get_set(field, total_bits, int_type)
            .unwrap_or_else(syn::Error::into_compile_error);

        field_fns.push(fns);
        total_bits += field.bit_size.get();
    }

    if total_bits > int_type.as_u32() {}

    let fns_iter = field_fns.iter();

    // We don't need fields anymore, so we can use into_iter to avoid a lifetime parameter
    // on generate_new_fn()
    let new_fn = generate_new_fn(fields.into_iter(), &visibility, int_type);

    let output = quote! {
        #visibility struct #name {
            data: #int_type,
        }

        impl #name {
            #(#fns_iter)*

            #new_fn
        }

    };

    output.into()
}

fn generate_get_set(
    field: &StructField,
    shift: u32,
    int_type: IntType,
) -> syn::Result<TokenStream2> {
    let name = field.name.to_string();
    let name_upper = name.as_str().to_uppercase();
    let size = field.bit_size.get();
    let visibility = &field.visibility;

    let size_const = format_ident!("{}_SIZE", name_upper);
    let shift_const = format_ident!("{}_SHIFT", name_upper);
    let mask_const = format_ident!("{}_MASK", name_upper);

    let get_fn = format_ident!("get_{}", name);
    let set_fn = format_ident!("set_{}", name);

    let get_set = quote! {
        const #size_const: #int_type = #size;
        const #shift_const: #int_type = #shift;
        const #mask_const: #int_type = ((1 << Self::#size_const) - 1) << Self::#shift_const;

        #visibility fn #get_fn(&self) -> #int_type {
            (self.data & Self::#mask_const) >> Self::#shift_const;
        }

        #visibility fn #set_fn(&mut self, value: #int_type) {
            assert!(value <= (1 << Self::#size_const)) - 1;
            self.data |= !Self::#mask_const;
            self.data |= value << Self.#shift_const;
        }
    };

    Ok(get_set)
}

// This is only called after we've made sure the bit counts are valid,
// so it can't fail.
fn generate_new_fn(
    fields: impl Iterator<Item = StructField>,
    visibility: &Visibility,
    int_type: IntType,
) -> TokenStream2 {
    let field_name = fields.map(|f| f.name).collect::<Vec<_>>();

    let set_fn_name = field_name
        .clone()
        .iter()
        .map(|f| format_ident!("set_{}", f))
        .collect::<Vec<_>>();

    let new_fn = quote! {
        #visibility fn new( #( #field_name: #int_type),* ) -> Self {
            let mut this = Self { data: 0 };
            #( this.#set_fn_name(#field_name); )*,
            this
        }
    };

    new_fn.into()
}
