use proc_macro::{self, TokenStream};
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, DataEnum, DataStruct, DeriveInput,
    Fields, FnArg, GenericParam, Generics, Ident, ItemFn, ItemTrait, Member, PatType, Token, Type,
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
    let DeriveInput {
        ident,
        data,
        generics,
        ..
    } = parse_macro_input!(input);

    match data {
        syn::Data::Struct(data_struct) => derive_trace_struct(ident, data_struct, generics).into(),
        syn::Data::Enum(data_enum) => derive_trace_enum(ident, data_enum).into(),
        _ => panic!("Union types are not supported."),
    }
}

fn derive_trace_struct(
    name: Ident,
    record: DataStruct,
    generics: Generics,
) -> proc_macro2::TokenStream {
    let fields = match record.fields {
        Fields::Named(fields) => fields.named,
        Fields::Unnamed(fields) => fields.unnamed,
        _ => {
            return quote! {
                impl ::scheme_rs::gc::Trace for #name {
                    fn visit_children(&self, visitor: fn(::scheme_rs::gc::OpaqueGcPtr)) {}
                }
            }
        }
    };

    let Generics {
        mut params,
        where_clause,
        ..
    } = generics;

    let mut unbound_params = Punctuated::<GenericParam, Token![,]>::new();

    for param in params.iter_mut() {
        match param {
            GenericParam::Type(ref mut ty) => {
                ty.bounds.push(syn::TypeParamBound::Verbatim(
                    quote! { ::scheme_rs::gc::Trace },
                ));
                unbound_params.push(GenericParam::Type(syn::TypeParam::from(ty.ident.clone())));
            }
            param => unbound_params.push(param.clone()),
        }
    }

    let field_actions = fields
        .iter()
        .enumerate()
        .flat_map(|(i, f)| {
            let ident = f.ident.clone().map_or_else(
                || {
                    Member::Unnamed(syn::Index {
                        index: i as u32,
                        span: Span::call_site(),
                    })
                },
                Member::Named,
            );
            if is_gc(&f.ty) {
                quote! {
                    visitor(self.#ident.as_opaque());
                }
            } else {
                quote! {
                    self. #ident .visit_children(visitor);
                }
            }
        })
        .collect::<Vec<_>>();

    quote! {
    #[automatically_derived]
        unsafe impl<#params> ::scheme_rs::gc::Trace for #name <#unbound_params>
    #where_clause
    {
            unsafe fn visit_children(&self, visitor: fn(::scheme_rs::gc::OpaqueGcPtr)) {
                #(
                    #field_actions
                )*
            }
        }
    }
}

// TODO: Add generics here
fn derive_trace_enum(name: Ident, data_enum: DataEnum) -> proc_macro2::TokenStream {
    let match_clauses = data_enum.variants.into_iter().flat_map(|variant| {
        let fields: Vec<_> = match variant.fields {
            Fields::Named(ref named) => named
                .named
                .iter()
                .map(|field| (field.ty.clone(), field.ident.as_ref().unwrap().clone()))
                .collect(),
            Fields::Unnamed(ref unnamed) => unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, field)| {
                    let ident = Ident::new(&format!("t{i}"), Span::call_site());
                    (field.ty.clone(), ident)
                })
                .collect(),
            _ => return None,
        };
        let actions: Vec<_> = fields
            .iter()
            .map(|(ty, accessor)| {
                if is_gc(&ty) {
                    quote! {
                        visitor(#accessor.as_opaque())
                    }
                } else {
                    quote! {
                        #accessor.visit_children(visitor)
                    }
                }
            })
            .collect();
        let field_name = fields.into_iter().map(|(_, field)| field);
        let fields_destructured = match variant.fields {
            Fields::Named(..) => quote! { { #( ref #field_name, )* .. } },
            _ => quote! { ( #( ref #field_name ),* ) },
        };
        let variant_name = variant.ident;
        Some(quote! {
            #name::#variant_name #fields_destructured => {
                #(
                    #actions;
                )*
            }
        })
    });
    quote! {
        unsafe impl ::scheme_rs::gc::Trace for #name {
            unsafe fn visit_children(&self, visitor: fn(::scheme_rs::gc::OpaqueGcPtr)) {
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

#[proc_macro_attribute]
pub fn trace(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ItemTrait {
        attrs,
        vis,
        unsafety,
        auto_token,
        ident,
        generics: Generics {
            params,
            where_clause,
            ..
        },
        mut supertraits,
        items,
        ..
    } = parse_macro_input!(item as ItemTrait);

    supertraits.push(syn::TypeParamBound::Verbatim(
        quote! { ::scheme_rs::gc::Trace },
    ));

    quote! {
    #( #attrs )* #vis #unsafety #auto_token trait #ident < #params >: #supertraits
    #where_clause
    {
        #( #items )*

        /// # Safety
        ///
        /// Function automatically implemented by [::scheme_rs::gc::Trace] macro. DO NOT CALL!
        /// DO NOT IMPLEMENT!
        unsafe fn scheme_rs_visit_children(&self, visitor: fn(::scheme_rs::gc::OpaqueGcPtr)) {
        <Self as ::scheme_rs::gc::Trace>::visit_children(self, visitor);
        }
    }

    unsafe impl < #params > ::scheme_rs::gc::Trace for dyn #ident < #params >
    #where_clause
    {
        unsafe fn visit_children(&self, visitor: fn(::scheme_rs::gc::OpaqueGcPtr)) {
        self.scheme_rs_visit_children(visitor);
        }
    }
    }
    .into()
}
