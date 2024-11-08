use proc_macro::{self, TokenStream};
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, DataEnum, DataStruct, DeriveInput, Fields, FnArg, Ident,
    ItemFn, PatType, Type,
};

#[proc_macro_attribute]
pub fn builtin(name: TokenStream, item: TokenStream) -> TokenStream {
    let name = proc_macro2::TokenStream::from(name);
    let builtin = parse_macro_input!(item as ItemFn);

    let impl_name = builtin.sig.ident.clone();
    let wrapper_name = impl_name.to_string() + "_wrapper";
    let wrapper_name = Ident::new(&wrapper_name, Span::call_site());

    let is_variadic = if let Some(last_arg) = builtin.sig.inputs.last() {
        is_vec(last_arg)
    } else {
        false
    };

    let num_args = if is_variadic {
        builtin.sig.inputs.len().saturating_sub(2)
    } else {
        builtin.sig.inputs.len() - 1
    };

    let wrapper: ItemFn = if !is_variadic {
        let arg_indices: Vec<_> = (0..num_args).into_iter().collect();
        parse_quote! {
            fn #wrapper_name(
                cont: Option<std::sync::Arc<::scheme_rs::continuation::Continuation>>,
                args: Vec<::scheme_rs::gc::Gc<::scheme_rs::value::Value>>
            ) -> futures::future::BoxFuture<'static, Result<Vec<::scheme_rs::gc::Gc<::scheme_rs::value::Value>>, ::scheme_rs::error::RuntimeError>> {
                Box::pin(
                    async move {
                        #impl_name(
                            &cont,
                            #( &args[#arg_indices], )*
                        ).await
                    }
                )
            }
        }
    } else {
        let arg_indices: Vec<_> = (0..num_args).into_iter().collect();
        parse_quote! {
            fn #wrapper_name(
                cont: Option<std::sync::Arc<::scheme_rs::continuation::Continuation>>,
                mut required_args: Vec<::scheme_rs::gc::Gc<::scheme_rs::value::Value>>
            ) -> futures::future::BoxFuture<'static, Result<Vec<::scheme_rs::gc::Gc<::scheme_rs::value::Value>>, ::scheme_rs::error::RuntimeError>> {
                let var_args = required_args.split_off(#num_args);
                Box::pin(
                    async move {
                        #impl_name(
                            &cont,
                            #( &required_args[#arg_indices], )*
                            var_args
                        ).await
                    }
                )
            }
        }
    };
    quote! {
        #builtin

        #wrapper

        inventory::submit! {
            ::scheme_rs::builtin::Builtin::new(#name, #num_args, #is_variadic, #wrapper_name)
        }
    }
    .into()
}

fn is_vec(arg: &FnArg) -> bool {
    if let FnArg::Typed(PatType { ty, .. }) = arg {
        if let Type::Path(ref path) = ty.as_ref() {
            return path
                .path
                .segments
                .last()
                .map(|p| p.ident.to_string())
                .as_deref()
                == Some("Vec");
        }
    }
    false
}

#[proc_macro_derive(Trace)]
pub fn derive_trace(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, data, .. } = parse_macro_input!(input);

    match data {
        syn::Data::Struct(data_struct) => derive_trace_struct(ident, data_struct).into(),
        syn::Data::Enum(data_enum) => derive_trace_enum(ident, data_enum).into(),
        _ => panic!("Union types are not supported."),
    }
}

fn derive_trace_struct(name: Ident, record: DataStruct) -> proc_macro2::TokenStream {
    let fields = match record.fields {
        Fields::Named(fields) => fields.named,
        Fields::Unnamed(fields) => fields.unnamed,
        _ => {
            return quote! {
                impl ::scheme_rs::gc::Trace for #name {
                    fn unroot(&self) {}
                }
            }
        }
    };

    let field_names = fields
        .iter()
        .enumerate()
        .map(|(i, f)| {
            if is_gc(&f.ty) {
                panic!("Gc types not allowed in Traceable struct, must be GcInner");
            }
            f.ident
                .clone()
                .unwrap_or_else(|| Ident::new(&i.to_string(), Span::call_site()))
        })
        .collect::<Vec<_>>();

    quote! {
        impl ::scheme_rs::gc::Trace for #name {
            fn unroot(&self) {
                #( self.#field_names.unroot(); )*
            }
        }
    }
}

fn derive_trace_enum(name: Ident, data_enum: DataEnum) -> proc_macro2::TokenStream {
    let match_clauses = data_enum.variants.iter().flat_map(|variant| {
        let variant_name = variant.ident.clone();
        let field_name: Vec<_> = match variant.fields {
            Fields::Named(ref named) => named
                .named
                .iter()
                .map(|x| {
                    let ident = x.ident.clone().unwrap();
                    quote! { #ident }
                })
                .collect(),
            Fields::Unnamed(ref unnamed) => unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, _)| {
                    let ident = Ident::new(&format!("t{i}"), Span::call_site());
                    quote! { #ident }
                })
                .collect(),
            _ => return None,
        };
        let fields_destructured = match variant.fields {
            Fields::Named(..) => quote! { { #( ref #field_name ),* } },
            _ => quote! { ( #( ref #field_name ),* ) },
        };
        Some(quote! {
            #name::#variant_name #fields_destructured => {
                #( #field_name.unroot(); )*
            }
        })
    });
    quote! {
        impl ::scheme_rs::gc::Trace for #name {
            fn unroot(&self) {
                match self {
                    #( #match_clauses )*,
                    _ => (),
                }
            }
        }
    }
}

fn is_gc(arg: &Type) -> bool {
    if let Type::Path(ref path) = arg {
        return path
            .path
            .segments
            .last()
            .map(|p| p.ident.to_string())
            .as_deref()
            == Some("Gc");
    }
    false
}
