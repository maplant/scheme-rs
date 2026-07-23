use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    Ident, ItemFn, LitStr, ReturnType, Token,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

struct BridgeAttrs {
    name: String,
    lib: String,
    blocking: bool,
}

impl Parse for BridgeAttrs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut name = None;
        let mut lib = None;
        let mut blocking = false;

        while !input.is_empty() {
            let key: Ident = input.parse()?;
            input.parse::<Token![=]>()?;

            match key.to_string().as_str() {
                "name" => {
                    let val: LitStr = input.parse()?;
                    name = Some(val.value());
                }
                "lib" => {
                    let val: LitStr = input.parse()?;
                    lib = Some(val.value());
                }
                "blocking" => {
                    let val: syn::LitBool = input.parse()?;
                    blocking = val.value();
                }
                other => {
                    return Err(syn::Error::new(
                        key.span(),
                        format!("unknown attribute `{other}`"),
                    ));
                }
            }

            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }

        let name = name.ok_or_else(|| input.error("missing required attribute `name`"))?;
        let lib = lib.ok_or_else(|| input.error("missing required attribute `lib`"))?;
        Ok(BridgeAttrs { name, lib, blocking })
    }
}

fn returns_result(ret: &ReturnType) -> bool {
    if let ReturnType::Type(_, ty) = ret {
        if let syn::Type::Path(tp) = ty.as_ref() {
            if let Some(seg) = tp.path.segments.last() {
                return seg.ident == "Result";
            }
        }
    }
    false
}

#[proc_macro_attribute]
pub fn plugin_bridge(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attrs = parse_macro_input!(attr as BridgeAttrs);
    let func = parse_macro_input!(item as ItemFn);

    let fn_name = &func.sig.ident;
    let wrapper_name = format_ident!("__plugin_bridge_{}", fn_name);
    let spec_name = format_ident!("__plugin_bridge_spec_{}", fn_name);

    let params: Vec<_> = func.sig.inputs.iter().collect();
    let nargs = params.len();
    let bridge_name = &attrs.name;
    let bridge_lib = &attrs.lib;
    let blocking = attrs.blocking;

    let arg_bindings: Vec<_> = (0..nargs).map(|i| format_ident!("__arg_{}", i)).collect();

    let extractions: Vec<_> = params
        .iter()
        .enumerate()
        .map(|(i, param)| {
            let ty = match param {
                syn::FnArg::Typed(pat) => &pat.ty,
                _ => unreachable!("plugin_bridge does not support self parameters"),
            };
            let binding = &arg_bindings[i];
            quote! {
                let #binding: #ty = match <#ty as ::scheme_rs_plugin_api::FromScheme>::from_scheme(
                    unsafe { &*args.add(#i) }
                ) {
                    Ok(v) => v,
                    Err(e) => return ::scheme_rs_plugin_api::BridgeReturn::err(e.message()),
                };
            }
        })
        .collect();

    let call_args: Vec<_> = arg_bindings.iter().map(|b| quote! { #b }).collect();

    let result_handling = if returns_result(&func.sig.output) {
        quote! {
            match #fn_name(#(#call_args),*) {
                Ok(val) => ::scheme_rs_plugin_api::BridgeReturn::ok(
                    ::scheme_rs_plugin_api::IntoScheme::into_scheme(val)
                ),
                Err(e) => {
                    let e: ::scheme_rs_plugin_api::PluginError = ::std::convert::Into::into(e);
                    ::scheme_rs_plugin_api::BridgeReturn::err(e.message())
                }
            }
        }
    } else {
        quote! {
            let val = #fn_name(#(#call_args),*);
            ::scheme_rs_plugin_api::BridgeReturn::ok(
                ::scheme_rs_plugin_api::IntoScheme::into_scheme(val)
            )
        }
    };

    let name_len = bridge_name.len();
    let lib_len = bridge_lib.len();

    let output = quote! {
        #func

        unsafe extern "C" fn #wrapper_name(
            args: *const ::scheme_rs_plugin_api::Value,
            nargs: usize,
        ) -> ::scheme_rs_plugin_api::BridgeReturn {
            if nargs != #nargs {
                return ::scheme_rs_plugin_api::BridgeReturn::err(
                    concat!(#bridge_name, ": expected ", stringify!(#nargs), " arguments")
                );
            }
            #(#extractions)*
            #result_handling
        }

        fn #spec_name() -> ::scheme_rs_plugin_api::BridgeSpec {
            static NAME: &[u8] = #bridge_name.as_bytes();
            static LIB: &[u8] = #bridge_lib.as_bytes();
            ::scheme_rs_plugin_api::BridgeSpec {
                name_ptr: NAME.as_ptr(),
                name_len: #name_len,
                lib_ptr: LIB.as_ptr(),
                lib_len: #lib_len,
                num_args: #nargs,
                variadic: false,
                func: Some(#wrapper_name as ::scheme_rs_plugin_api::SimpleBridgeFn),
                cps_func: None,
                blocking: #blocking,
            }
        }
    };

    output.into()
}
