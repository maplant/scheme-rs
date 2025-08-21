use proc_macro::{self, TokenStream};
use proc_macro2::{Literal, Span};
use quote::{format_ident, quote};
use syn::{
    DataEnum, DataStruct, DeriveInput, Fields, FnArg, GenericParam, Generics, Ident, ItemFn,
    LitStr, Member, Pat, PatIdent, PatType, Token, Type, TypePath, TypeReference,
    parse_macro_input, parse_quote, punctuated::Punctuated,
};

#[proc_macro_attribute]
pub fn bridge(args: TokenStream, item: TokenStream) -> TokenStream {
    let mut name: Option<LitStr> = None;
    let mut lib: Option<LitStr> = None;
    let bridge_attr_parser = syn::meta::parser(|meta| {
        if meta.path.is_ident("name") {
            name = Some(meta.value()?.parse()?);
            Ok(())
        } else if meta.path.is_ident("lib") {
            lib = Some(meta.value()?.parse()?);
            Ok(())
        } else {
            Err(meta.error("unsupported bridge property"))
        }
    });

    parse_macro_input!(args with bridge_attr_parser);

    let name = name.unwrap().value();
    let lib = lib.unwrap().value();
    let bridge = parse_macro_input!(item as ItemFn);

    let impl_name = bridge.sig.ident.clone();
    let wrapper_name = impl_name.to_string();
    let wrapper_name = Ident::new(&wrapper_name, Span::call_site());

    let is_variadic = if let Some(last_arg) = bridge.sig.inputs.last() {
        is_slice(last_arg)
    } else {
        false
    };

    let num_args = if is_variadic {
        bridge.sig.inputs.len().saturating_sub(1)
    } else {
        bridge.sig.inputs.len()
    };

    let arg_names: Vec<_> = bridge
        .sig
        .inputs
        .iter()
        .enumerate()
        .map(|(i, arg)| {
            if let FnArg::Typed(PatType { pat, .. }) = arg {
                if let Pat::Ident(PatIdent { ref ident, .. }) = pat.as_ref() {
                    return ident.to_string();
                }
            }
            format!("arg{i}")
        })
        .collect();

    let wrapper: ItemFn = if !is_variadic {
        let arg_indices: Vec<_> = (0..num_args).collect();
        parse_quote! {
            pub(crate) fn #wrapper_name<'a>(
                args: &'a [::scheme_rs::value::Value],
                rest_args: &'a [::scheme_rs::value::Value],
                cont: &'a ::scheme_rs::value::Value,
                _env: &'a [::scheme_rs::gc::Gc<::scheme_rs::value::Value>],
                exception_handler: &'a Option<::scheme_rs::gc::Gc<::scheme_rs::exception::ExceptionHandler>>,
                dynamic_wind: &'a ::scheme_rs::proc::DynamicWind,
            ) -> futures::future::BoxFuture<'a, Result<scheme_rs::proc::Application, ::scheme_rs::value::Value>> {
                #bridge

                Box::pin(
                    async move {
                        let cont = cont.clone().try_into()?;
                        Ok(::scheme_rs::proc::Application::new(
                            cont,
                            #impl_name(
                                #( &args[#arg_indices], )*
                            ).await?,
                            exception_handler.clone(),
                            dynamic_wind.clone(),
                            None // TODO
                        ))
                    }
                )
            }
        }
    } else {
        let arg_indices: Vec<_> = (0..num_args).collect();
        parse_quote! {
            pub(crate) fn #wrapper_name<'a>(
                args: &'a [::scheme_rs::value::Value],
                rest_args: &'a [::scheme_rs::value::Value],
                cont: &'a ::scheme_rs::value::Value,
                _env: &'a [::scheme_rs::gc::Gc<::scheme_rs::value::Value>],
                exception_handler: &'a Option<::scheme_rs::gc::Gc<::scheme_rs::exception::ExceptionHandler>>,
                dynamic_wind: &'a ::scheme_rs::proc::DynamicWind,
            ) -> futures::future::BoxFuture<'a, Result<scheme_rs::proc::Application, ::scheme_rs::value::Value>> {
                #bridge

                Box::pin(
                    async move {
                        let cont = cont.clone().try_into()?;
                        Ok(::scheme_rs::proc::Application::new(
                            cont,
                            #impl_name(
                                #( &args[#arg_indices], )*
                                rest_args
                            ).await?,
                            exception_handler.clone(),
                            dynamic_wind.clone(),
                            None // TODO
                        ))
                    }
                )
            }
        }
    };
    quote! {
        #wrapper

        inventory::submit! {
            ::scheme_rs::registry::BridgeFn::new(
                #name,
                #lib,
                #num_args,
                #is_variadic,
                #wrapper_name,
                ::scheme_rs::registry::BridgeFnDebugInfo::new(
                    ::std::file!(),
                    ::std::line!(),
                    ::std::column!(),
                    0,
                    &[ #( #arg_names, )* ],
                )
            )
        }
    }
    .into()
}

fn is_slice(arg: &FnArg) -> bool {
    matches!(arg, FnArg::Typed(PatType { ty, ..}) if matches!(ty.as_ref(), Type::Reference(TypeReference { elem, .. }) if matches!(elem.as_ref(), Type::Slice(_))))
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
        syn::Data::Enum(data_enum) => derive_trace_enum(ident, data_enum, generics).into(),
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
                unsafe impl ::scheme_rs::gc::Trace for #name {
                    unsafe fn visit_children(&self, visitor: unsafe fn(::scheme_rs::gc::OpaqueGcPtr)) {}
                }
            };
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

    let field_visits = fields
        .iter()
        .enumerate()
        .map(|(i, f)| {
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

    let field_drops = fields
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
            if !is_gc(&f.ty) {
                Some(quote! {
                        self.#ident.finalize();
                })
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    quote! {
        #[automatically_derived]
        unsafe impl<#params> ::scheme_rs::gc::Trace for #name <#unbound_params>
        #where_clause
        {
            unsafe fn visit_children(&self, visitor: unsafe fn(::scheme_rs::gc::OpaqueGcPtr)) {
                #(
                    #field_visits
                )*
            }

            unsafe fn finalize(&mut self) {
                #(
                    #field_drops
                )*
            }
        }
    }
}

// TODO: Add generics here
fn derive_trace_enum(
    name: Ident,
    data_enum: DataEnum,
    generics: Generics,
) -> proc_macro2::TokenStream {
    let (visit_match_clauses, finalize_match_clauses): (Vec<_>, Vec<_>) = data_enum
        .variants
        .into_iter()
        .flat_map(|variant| {
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
            let visits: Vec<_> = fields
                .iter()
                .map(|(ty, accessor)| {
                    if is_gc(ty) {
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
            let drops: Vec<_> = fields
                .iter()
                .filter(|(ty, _)| !is_gc(ty))
                .map(|(_, accessor)| {
                    quote! {
                        #accessor.finalize();
                    }
                })
                .collect();
            let field_name = fields.iter().map(|(_, field)| field);
            let fields_destructured = match variant.fields {
                Fields::Named(..) => quote! { { #( ref #field_name, )* .. } },
                _ => quote! { ( #( ref #field_name ),* ) },
            };
            let field_name = fields.iter().map(|(_, field)| field);
            let fields_destructured_mut = match variant.fields {
                Fields::Named(..) => quote! { { #( ref mut #field_name, )* .. } },
                _ => quote! { ( #( ref mut #field_name ),* ) },
            };
            let variant_name = variant.ident;
            Some((
                quote! {
                    Self::#variant_name #fields_destructured => {
                        #(
                            #visits;
                        )*
                    }
                },
                quote! {
                    Self::#variant_name #fields_destructured_mut => {
                        #(
                            #drops
                        )*
                    }
                },
            ))
        })
        .unzip();

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

    quote! {
        unsafe impl<#params> ::scheme_rs::gc::Trace for #name <#unbound_params>
        #where_clause
        {
            unsafe fn visit_children(&self, visitor: unsafe fn(::scheme_rs::gc::OpaqueGcPtr)) {
                match self {
                    #( #visit_match_clauses, )*
                    _ => (),
                }
            }

            unsafe fn finalize(&mut self) {
                match self {
                    #( #finalize_match_clauses, )*
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

fn is_primitive(path: &TypePath, ty: &'static str) -> bool {
    path.path
        .segments
        .last()
        .map(|p| p.ident.to_string())
        .as_deref()
        == Some(ty)
}

fn rust_type_to_cranelift_type(ty: &Type) -> Option<Ident> {
    match ty {
        Type::Path(path) if is_primitive(path, "bool") => Some(format_ident!("I8")),
        Type::Path(path) if is_primitive(path, "i32") => Some(format_ident!("I32")),
        Type::Path(path) if is_primitive(path, "u32") => Some(format_ident!("I32")),
        Type::Path(_) => Some(format_ident!("I64")),
        Type::Ptr(_) => Some(format_ident!("I64")),
        Type::Tuple(_) => None,
        _ => unreachable!(),
    }
}

#[proc_macro_attribute]
pub fn runtime_fn(_args: TokenStream, item: TokenStream) -> TokenStream {
    let runtime_fn = parse_macro_input!(item as ItemFn);

    let name_ident = runtime_fn.sig.ident.clone();
    let name_lit = Literal::string(&runtime_fn.sig.ident.to_string());
    let ret = if let Some(ret_type) = match runtime_fn.sig.output {
        syn::ReturnType::Default => None,
        syn::ReturnType::Type(_, ref ty) => Some(rust_type_to_cranelift_type(&ty)),
    }.flatten() {
        quote! { sig.returns.push(AbiParam::new(types::#ret_type)); }
    } else {
        quote! {}
    };
    let args: Vec<_> = runtime_fn
        .sig
        .inputs
        .iter()
        .filter_map(|arg| {
            let FnArg::Typed(pat) = arg else {
                unreachable!();
            };
            rust_type_to_cranelift_type(&pat.ty)
        })
        .collect();


    quote! {
        #[allow(unused)]
        inventory::submit!(crate::runtime::RuntimeFn::new(
            |runtime_fns, module| {
                use cranelift::prelude::*;
                use cranelift_module::{Module, Linkage};
                let mut sig = module.make_signature();
                #(
                    sig.params.push(AbiParam::new(types::#args));
                )*
                #ret
                let func = module.declare_function(#name_lit, Linkage::Import, &sig).unwrap();
                runtime_fns.#name_ident(func);
            },
            |jit_builder| {
                jit_builder.symbol(#name_lit, #name_ident as *const u8);
            }
        ));
        

        #runtime_fn
    }
    .into()
}
