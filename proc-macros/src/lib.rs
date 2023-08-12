use proc_macro::{self, TokenStream};
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, Ident, ItemFn};

#[proc_macro_attribute]
pub fn builtin(attr: TokenStream, item: TokenStream) -> TokenStream {
    let name = attr.to_string();

    let builtin = parse_macro_input!(item as ItemFn);

    let impl_name = builtin.sig.ident.clone();
    let wrapper_name = impl_name.to_string() + "_wrapper";
    let wrapper_name = Ident::new(&wrapper_name, Span::call_site());
    let num_args = builtin.sig.inputs.len() - 1;
    let arg_indices: Vec<_> = (0..num_args).into_iter().collect();

    quote! {
        #builtin

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

        inventory::submit! {
            crate::builtin::Builtin::new(#name, #num_args, false, #wrapper_name)
        }
    }
    .into()
}
