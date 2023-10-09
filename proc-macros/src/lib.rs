use proc_macro::{self, TokenStream};
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, parse_quote, FnArg, Ident, ItemFn, PatType, Type};

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
                cont: Option<std::sync::Arc<crate::continuation::Continuation>>,
                args: Vec<crate::gc::Gc<crate::value::Value>>
            ) -> futures::future::BoxFuture<'static, Result<crate::gc::Gc<crate::value::Value>, crate::error::RuntimeError>> {
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
                cont: Option<std::sync::Arc<crate::continuation::Continuation>>,
                mut required_args: Vec<crate::gc::Gc<crate::value::Value>>
            ) -> futures::future::BoxFuture<'static, Result<crate::gc::Gc<crate::value::Value>, crate::error::RuntimeError>> {
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
            crate::builtin::Builtin::new(#name, #num_args, #is_variadic, #wrapper_name)
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
