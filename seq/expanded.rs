#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
use proc_macro::{TokenStream, TokenTree, Literal, Ident, Group, Delimiter};
#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let mut input = input.into_iter();
    let ident = input.next().unwrap();
    let ident = if let TokenTree::Ident(ident) = ident {
        ident
    } else {
        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["1"], &[]))
    };
    let str_in = input.next().unwrap();
    if let TokenTree::Ident(str_in) = str_in {
        match (&str_in.to_string(), &"in") {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    let kind = ::core::panicking::AssertKind::Eq;
                    ::core::panicking::assert_failed(
                        kind,
                        &*left_val,
                        &*right_val,
                        ::core::option::Option::None,
                    );
                }
            }
        };
    } else {
        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["2"], &[]))
    };
    let start = input.next().unwrap();
    let start: i32 = start.to_string().parse().unwrap();
    let dot1 = input.next().unwrap();
    if let TokenTree::Punct(dot1) = dot1 {
        match (&dot1.to_string(), &".") {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    let kind = ::core::panicking::AssertKind::Eq;
                    ::core::panicking::assert_failed(
                        kind,
                        &*left_val,
                        &*right_val,
                        ::core::option::Option::None,
                    );
                }
            }
        };
    } else {
        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["3"], &[]))
    };
    let dot2 = input.next().unwrap();
    if let TokenTree::Punct(dot2) = dot2 {
        match (&dot2.to_string(), &".") {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    let kind = ::core::panicking::AssertKind::Eq;
                    ::core::panicking::assert_failed(
                        kind,
                        &*left_val,
                        &*right_val,
                        ::core::option::Option::None,
                    );
                }
            }
        };
    } else {
        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["4"], &[]))
    };
    let end = input.next().unwrap();
    let end: i32 = end.to_string().parse().unwrap();
    let body = input.next().unwrap();
    let body = if let TokenTree::Group(body) = body {
        body
    } else {
        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["5"], &[]))
    };
    if !match body.delimiter() {
        Delimiter::Brace => true,
        _ => false,
    } {
        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["6"], &[]));
    }
    let mut stream = TokenStream::new();
    for i in start..end {
        stream.extend(gen_body(body.stream(), &ident.to_string(), i));
    }
    {
        ::std::io::_print(
            ::core::fmt::Arguments::new_v1(
                &["", "\n"],
                &[::core::fmt::ArgumentV1::new_display(&stream.to_string())],
            ),
        );
    };
    stream
}
fn gen_body(body: TokenStream, ident: &str, n: i32) -> TokenStream {
    body.into_iter()
        .map(|token_tree| match token_tree {
            TokenTree::Group(g) => {
                let stream = gen_body(g.stream(), ident, n);
                TokenTree::Group(Group::new(Delimiter::Bracket, stream))
            }
            TokenTree::Ident(i) => {
                let name = i.to_string();
                if name == ident {
                    TokenTree::Literal(Literal::i32_unsuffixed(n))
                } else {
                    TokenTree::Ident(Ident::new(&name, i.span()))
                }
            }
            c => c,
        })
        .collect()
}
const _: () = {
    extern crate proc_macro;
    #[rustc_proc_macro_decls]
    #[used]
    #[allow(deprecated)]
    static _DECLS: &[proc_macro::bridge::client::ProcMacro] = &[
        proc_macro::bridge::client::ProcMacro::bang("seq", seq),
    ];
};
