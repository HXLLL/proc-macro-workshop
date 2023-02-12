use proc_macro::TokenStream;
use proc_macro2::{
    Delimiter, Group, Ident, Literal, Span, TokenStream as TokenStream2,
    TokenTree as TT,
};
use syn::{
    braced,
    buffer::{Cursor, TokenBuffer},
    parse::Parse,
    token::Brace,
    Error, LitInt, Token,
};

struct Seq {
    ident: Ident,
    _in_token: Token![in],
    start: LitInt,
    _two_dot_token: Token![..],
    equal_token: Option<Token![=]>,
    end: LitInt,
    _brace_token: Brace,
    body: TokenStream2,
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq: Seq = syn::parse_macro_input!(input);
    seq.expand().into()
}

impl Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Seq {
            ident: input.parse()?,
            _in_token: input.parse()?,
            start: input.parse()?,
            _two_dot_token: input.parse()?,
            equal_token: input.parse().ok(),
            end: input.parse()?,
            _brace_token: braced!(content in input),
            body: content.parse()?,
        })
    }
}

impl Seq {
    fn expand(self) -> TokenStream2 {
        let Seq {
            ident,
            start,
            end,
            equal_token,
            body,
            ..
        } = self;
        match (start.base10_parse(), end.base10_parse()) {
            (Ok(start), Ok(end)) => {
                if equal_token.is_some() {
                    process(body, ident, start..=end)
                } else {
                    process(body, ident, start..end)
                }
            }
            (Err(e), _) => lit_error(e),
            (_, Err(e)) => lit_error(e),
        }
    }
}

fn lit_error(lit: Error) -> TokenStream2 {
    let error_msg = format!("Literal error: {lit}");
    Error::new(lit.span(), error_msg).to_compile_error()
}

fn process(
    body: TokenStream2,
    ident: Ident,
    range: impl Iterator<Item = i32> + Clone,
) -> TokenStream2 {
    let mut output = TokenStream2::new();
    let buffer = TokenBuffer::new2(body);
    if has_qualified_section(buffer.begin()) {
        eprintln!("has qualified section");
        repeat_qualified(buffer.begin(), &ident, range, &mut output);
    } else {
        eprintln!("do not has qualified section");
        repeat(buffer.begin(), &ident, range, &mut output);
    }
    eprintln!("macro output: {}", output.to_string());
    output
}

fn has_qualified_section(mut cur: Cursor) -> bool {
    while let Some((token, next)) = cur.token_tree() {
        cur = next;
        match token {
            TT::Group(g) => {
                let cursor = TokenBuffer::new2(g.stream());
                let cursor = cursor.begin();
                if has_qualified_section(cursor) {
                    return true;
                }
            }
            TT::Punct(p) if p.as_char() == '#' => {
                if get_qualified_group(&mut cur).is_some() {
                    return true;
                }
            }
            _ => {}
        }
    }
    false
}

fn repeat_qualified(
    mut cur: Cursor,
    ident: &Ident,
    range: impl Iterator<Item = i32> + Clone,
    output: &mut TokenStream2,
) {
    while let Some((token, next)) = cur.token_tree() {
        cur = next;
        eprintln!("repeat qualified processing {token:?}");
        match token {
            TT::Group(g) => {
                let mut group_tokens = TokenStream2::new();
                let buffer = TokenBuffer::new2(g.stream());
                repeat_qualified(
                    buffer.begin(),
                    ident,
                    range.clone(),
                    &mut group_tokens,
                );
                let mut group = Group::new(g.delimiter(), group_tokens);
                group.set_span(g.span());
                output.extend([TT::Group(group)]);
            }
            TT::Punct(p) if p.as_char() == '#' => {
                if let Some(g) = get_qualified_group(&mut cur) {
                    let buffer = TokenBuffer::new2(g.stream());
                    repeat(buffer.begin(), ident, range.clone(), output);
                } else {
                    output.extend([TT::Punct(p)]);
                }
            }
            t => output.extend([t]),
        }
    }
}

fn repeat(
    cur: Cursor,
    ident: &Ident,
    range: impl Iterator<Item = i32>,
    output: &mut TokenStream2,
) {
    for i in range {
        replace(cur, ident, i, output);
    }
}

fn get_qualified_group(cur: &mut Cursor) -> Option<Group> {
    if let Some((TT::Group(g), tmp_cur)) = cur.token_tree() {
        if let Some((TT::Punct(p), new_cur)) = tmp_cur.token_tree() {
            if g.delimiter() == Delimiter::Parenthesis && p.as_char() == '*' {
                *cur = new_cur;
                return Some(g);
            }
        }
    }
    None
}

fn replace(mut cur: Cursor, ident: &Ident, n: i32, output: &mut TokenStream2) {
    while let Some((token, next)) = cur.token_tree() {
        cur = next;
        eprintln!("replace processing {token:?}");
        match token {
            TT::Group(g) => {
                let mut group_tokens = TokenStream2::new();
                let buffer = TokenBuffer::new2(g.stream());
                replace(buffer.begin(), ident, n, &mut group_tokens);
                let mut group = Group::new(g.delimiter(), group_tokens);
                group.set_span(g.span());
                output.extend([TT::Group(group)]);
            }
            TT::Ident(id) => {
                let mut span = id.span();
                let mut tokens = vec![id];
                get_tilde_group(&mut cur, &mut tokens, &mut span);
                eprintln!("tilde group: {tokens:?}");
                let mut new_token = String::new();
                let mut is_lit = true;

                for token in tokens {
                    let t = if ident == &token {
                        n.to_string()
                    } else {
                        is_lit = false;
                        token.to_string()
                    };
                    new_token.push_str(&t);
                }

                let new_token = if is_lit {
                    let lit: i32 = new_token.to_string().parse().unwrap();
                    TT::Literal(Literal::i32_unsuffixed(lit))
                } else {
                    TT::Ident(Ident::new(&new_token, span))
                };
                output.extend([new_token]);
            }
            t => output.extend([t]),
        }
    }
}

fn get_tilde_group(cur: &mut Cursor, output: &mut Vec<Ident>, span: &mut Span) {
    const JOIN_SPAN: bool = false;
    while let Some((tilde_token, tmp_cur1)) = cur.punct() {
        if let Some((ident, tmp_cur2)) = tmp_cur1.ident() {
            if tilde_token.as_char() == '~' {
                *cur = tmp_cur2;
                if JOIN_SPAN {
                    *span = span.join(tilde_token.span()).unwrap();
                    *span = span.join(ident.span()).unwrap();
                }
                output.push(ident);
            } else {
                break;
            }
        } else {
            break;
        }
    }
}
