use core::panic;
use std::collections::HashSet;

use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    token::{Colon2, Comma},
    Attribute, DataStruct, DeriveInput, Field, GenericArgument, GenericParam, Generics, Lit, Meta,
    MetaList, MetaNameValue, NestedMeta, Path, PathSegment, Type, TypePath, WherePredicate,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        attrs,
        generics,
        data,
        ..
    } = parse_macro_input!(input as DeriveInput);

    if let syn::Data::Struct(data) = data {
        let attr_bounds = collect_attr_bounds(&attrs);

        let debug_generics = collect_debug_generics(generics.clone(), &data);
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
        let mut where_clause = where_clause
            .cloned()
            .unwrap_or_else(|| parse_quote! { where });

        let convert = |g: &Type| -> WherePredicate {
            parse_quote! {#g: ::std::fmt::Debug }
        };
        let preds = &mut where_clause.predicates;
        match attr_bounds {
            Some(a) => preds.extend(a),
            None => preds.extend(debug_generics.iter().map(convert)),
        }

        let mut build_statement = quote! { f };
        generate_struct(ident.clone(), data, &mut build_statement);
        let output = quote! {
            impl #impl_generics ::std::fmt::Debug for #ident #ty_generics #where_clause {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    #build_statement
                }
            }
        };
        output.into()
    } else {
        panic!("only support named struct")
    }
}

type WherePredicateList = Punctuated<WherePredicate, Comma>;

fn collect_attr_bounds(attrs: &[Attribute]) -> Option<WherePredicateList> {
    let try_parse_bound = |m: &Meta| -> Option<WherePredicateList> {
        let bound_token: Path = parse_quote!(bound);
        if let Meta::NameValue(MetaNameValue {
            path,
            lit: Lit::Str(s),
            ..
        }) = m
        {
            if path == &bound_token {
                return s.parse_with(Punctuated::parse_terminated).ok();
            }
        }
        None
    };

    for attr in attrs {
        if let Ok(Meta::List(MetaList { path, nested, .. })) = attr.parse_meta() {
            if let Some(ident) = path.get_ident() {
                if ident == "debug" {
                    for meta in nested {
                        if let NestedMeta::Meta(ref m) = meta {
                            let ls = try_parse_bound(m);
                            if ls.is_some() {
                                return ls;
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

fn collect_debug_generics(generics: Generics, data: &DataStruct) -> HashSet<Type> {
    let generics: HashSet<Ident> = generics
        .params
        .into_iter()
        .filter_map(get_generic_param_type)
        .collect();
    let mut debug_generics: HashSet<Type> = HashSet::new();

    let mut try_add_debug_generics = |ty: &Type| {
        if let Some(segs) = get_type_segments(ty) {
            if let Some(head) = segs.first() {
                if generics.contains(&head.ident) {
                    debug_generics.insert(ty.clone());
                }
            }
        }
    };

    for field in &data.fields {
        if is_phantomdata(&field.ty) {
            continue;
        }
        map_type_args(&field.ty, &mut try_add_debug_generics);
    }

    debug_generics
}

fn get_generic_param_type(x: GenericParam) -> Option<Ident> {
    if let GenericParam::Type(ty_param) = x {
        Some(ty_param.ident)
    } else {
        None
    }
}

fn get_type_segments(ty: &Type) -> Option<&Punctuated<PathSegment, Colon2>> {
    if let Type::Path(TypePath { path, .. }) = ty {
        Some(&path.segments)
    } else {
        None
    }
}

fn is_phantomdata(ty: &Type) -> bool {
    if let Some(segs) = get_type_segments(ty) {
        if segs.len() == 1 && segs.first().unwrap().ident == "PhantomData" {
            return true;
        }
    }
    false
}

fn map_type_args(ty: &Type, f: &mut impl FnMut(&Type)) {
    f(ty);
    if let Some(segs) = get_type_segments(ty) {
        for seg in segs {
            if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                for arg in &args.args {
                    if let GenericArgument::Type(ty2) = arg {
                        map_type_args(ty2, f);
                    }
                }
            }
        }
    }
}

fn generate_struct(name: Ident, s: DataStruct, buffer: &mut TokenStream2) {
    make_struct(name, buffer);
    for field in s.fields {
        generate_field(field, buffer);
    }
    make_finish(buffer);
}

fn generate_field(field: Field, buffer: &mut TokenStream2) {
    let fmt = field.attrs.into_iter().find_map(get_field_debug_attr);
    make_field(field.ident.unwrap(), fmt, buffer);
}

fn get_field_debug_attr(attr: Attribute) -> Option<String> {
    if let Ok(Meta::NameValue(meta)) = attr.parse_meta() {
        if let Some(ident) = meta.path.get_ident() {
            if ident == "debug" {
                if let Lit::Str(str) = meta.lit {
                    return Some(str.value());
                }
            }
        }
    }
    None
}

fn make_finish(output: &mut TokenStream2) {
    output.extend(quote! {
        .finish()
    });
}

fn make_struct(name: Ident, output: &mut TokenStream2) {
    let name_str = format!("{name}");
    output.extend(quote! {
        .debug_struct(#name_str)
    });
}

fn make_field(name: Ident, fmt: Option<String>, output: &mut TokenStream2) {
    let name_str = format!("{name}");
    let t = match fmt {
        Some(f) => quote! {
            .field(#name_str, &format_args!(#f, &self.#name))
        },
        None => quote! {
            .field(#name_str, &self.#name)
        },
    };
    output.extend(t);
}
