use proc_macro::{self, TokenStream};
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, parse_quote, FnArg, Ident, ItemFn, PatType, Type};

#[proc_macro_attribute]
pub fn builtin(attr: TokenStream, item: TokenStream) -> TokenStream {
    let name = attr.to_string();

    let builtin = parse_macro_input!(item as ItemFn);

    let impl_name = builtin.sig.ident.clone();
    let wrapper_name = impl_name.to_string() + "_wrapper";
    let wrapper_name = Ident::new(&wrapper_name, Span::call_site());
    let num_args = builtin.sig.inputs.len() - 1;
    
    let is_variadic = if let Some(last_arg) = builtin.sig.inputs.last() {
        is_vec(last_arg)
    } else {
        false
    };

    let wrapper: ItemFn = if !is_variadic {
        let arg_indices: Vec<_> = (0..num_args).into_iter().collect();
        parse_quote! {
            fn #wrapper_name(
                env: crate::gc::Gc<crate::eval::Env>,
                args: Vec<crate::gc::Gc<crate::eval::Value>>
            ) -> futures::future::BoxFuture<'static, Result<crate::gc::Gc<crate::eval::Value>, crate::eval::RuntimeError>> {
                Box::pin(
                    async move {
                        #impl_name(
                            &env,
                            #( &args[#arg_indices], )*
                        ).await
                    }
                )
            }
        }
    } else {
        let num_required = num_args.saturating_sub(1);
        let arg_indices: Vec<_> = (0..num_required).into_iter().collect();
        parse_quote! {
            fn #wrapper_name(
                env: crate::gc::Gc<crate::eval::Env>,
                mut required_args: Vec<crate::gc::Gc<crate::eval::Value>>
            ) -> futures::future::BoxFuture<'static, Result<crate::gc::Gc<crate::eval::Value>, crate::eval::RuntimeError>> {
                let var_args = required_args.split_off(#num_required);
                Box::pin(
                    async move {
                        #impl_name(
                            &env,
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
