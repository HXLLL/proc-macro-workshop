use proc_macro::{TokenStream, TokenTree, Literal, Ident, Group, Delimiter};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let mut input = input.into_iter();
    let ident = input.next().unwrap();
    let ident = if let TokenTree::Ident(ident) = ident { ident } else { panic!("1") };
    
    let str_in = input.next().unwrap();
    if let TokenTree::Ident(str_in) = str_in { assert_eq!(str_in.to_string(), "in"); } else { panic!("2") };

    let start = input.next().unwrap();
    let start: i32 = start.to_string().parse().unwrap();

    let dot1 = input.next().unwrap();
    if let TokenTree::Punct(dot1) = dot1 { assert_eq!(dot1.to_string(), "."); } else { panic!("3") };

    let dot2 = input.next().unwrap();
    if let TokenTree::Punct(dot2) = dot2 { assert_eq!(dot2.to_string(), "."); } else { panic!("4") };

    let end = input.next().unwrap();
    let end: i32 = end.to_string().parse().unwrap();

    let body = input.next().unwrap();
    let body = if let TokenTree::Group(body) = body { body } else { panic!("5") };
    if !matches!(body.delimiter(), Delimiter::Brace) { panic!("6"); }

    let mut stream = TokenStream::new();
    for i in start..end {
        stream.extend(gen_body(body.stream(), &ident.to_string(), i));
    }
    println!("{}", stream.to_string());
    stream
}

fn gen_body(body: TokenStream, ident: &str, n: i32) -> TokenStream {
    body.into_iter().map(|token_tree| match token_tree {
        TokenTree::Group(g) => {
            let stream = gen_body(g.stream(), ident, n);
            TokenTree::Group(Group::new(Delimiter::Bracket, stream))
        },
        TokenTree::Ident(i) => {
            let name = i.to_string();
            if name == ident {
                TokenTree::Literal(Literal::i32_unsuffixed(n))
            } else {
                TokenTree::Ident(Ident::new(&name, i.span()))
            }
        }
        c => c,
    }).collect()
}