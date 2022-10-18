use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse::Parse, parse_macro_input, Data, DeriveInput, Field, LitInt, Token};

#[proc_macro_derive(PackedU64, attributes(bits))]
pub fn derive_packed_u64(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let generics = input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let impl_block = quote! {
        impl #impl_generics #name #ty_generics #where_clause {

        }
    };

    impl_block.into()
}

fn generate_impl_block(data: &Data) -> syn::Result<TokenStream> {
    match data {
        Data::Struct(ref data) => match data.fields {
            syn::Fields::Named(ref fields) => {
                let mut total_bits = 0;
                for field in fields.named.iter() {
                    for attr in &field.attrs {
                        if attr.path.is_ident("bits") {
                            let bits: BitsCount = syn::parse2(attr.tokens.clone())?;
                            let bits_count = bits.bits_count.base10_parse::<u64>()?;
                            let set_get_fns = generate_get_set(field, bits_count, total_bits);
                            total_bits += bits_count;
                            // We don't care about [bits=x] attributes after the first
                            break;
                        }
                    }
                }
            }
            // This macro doesn't work with tuple structs
            syn::Fields::Unnamed(_) => unimplemented!(),
            // If this is a unit struct, we don't need to do anything
            // TODO: might be worth emitting a warning in this case
            syn::Fields::Unit => return Ok(TokenStream::new()),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }

    return Ok(TokenStream::new());
}

fn generate_get_set(field: &Field, size: u64, shift: u64) -> syn::Result<TokenStream> {
    let name = field.ident.as_ref().unwrap().to_string();
    let name_upper = name.as_str().to_uppercase();

    let size_const = format_ident!("{}_SIZE", name_upper);
    let shift_const = format_ident!("{}_SHIFT", name_upper);
    let mask_const = format_ident!("{}_MASK", name_upper);

    let get_fn = format_ident!("get_{}", name);
    let set_fn = format_ident!("set_{}", name);

    let get_set = quote! {
        const #size_const: u64 = #size;
        const #shift_const: u64 = #shift;
        const #mask_const: u64 = ((1 << Self::#size_const) - 1) << Self::#shift_const;

        pub fn #get_fn(&self) -> u64 {
            (self.data & Self::#mask_const) >> Self::#shift_const;
        }

        pub fn #set_fn(&mut self, value: u64) {
            assert!(value <= (1 << Self::#size_const)) - 1;
            self.data |= !Self::#mask_const;
            self.data |= value << Self.#shift_const;
        }
    };

    Ok(get_set.into())
}

struct BitsCount {
    _eq_token: Token![=],
    pub bits_count: LitInt,
}

impl Parse for BitsCount {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            _eq_token: input.parse()?,
            bits_count: input.parse()?,
        })
    }
}
