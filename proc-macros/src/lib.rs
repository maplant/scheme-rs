use proc_macro::{self, TokenStream};
use proc_macro2::{Literal, Span};
use quote::{format_ident, quote};
use syn::{
    DataEnum, DataStruct, DeriveInput, Error, Fields, FnArg, GenericParam, Generics, Ident, ItemFn,
    LitStr, Member, Pat, PatIdent, PatType, Token, Type, TypePath, TypeReference, Visibility,
    parse_macro_input, punctuated::Punctuated,
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

    let (rest_args, is_variadic) = if let Some(last_arg) = bridge.sig.inputs.last()
        && is_slice(&last_arg)
    {
        (quote!(rest_args), true)
    } else {
        (quote!(), false)
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
                if let Pat::Ident(PatIdent { ident, .. }) = pat.as_ref() {
                    return ident.to_string();
                }
            }
            format!("arg{i}")
        })
        .collect();

    let arg_indices: Vec<_> = (0..num_args).collect();

    quote! {
        pub(crate) fn #wrapper_name<'a>(
            runtime: &'a ::scheme_rs::runtime::Runtime,
            _env: &'a [::scheme_rs::gc::Gc<::scheme_rs::value::Value>],
            args: &'a [::scheme_rs::value::Value],
            rest_args: &'a [::scheme_rs::value::Value],
            cont: &'a ::scheme_rs::value::Value,
            exception_handler: &'a Option<::scheme_rs::gc::Gc<::scheme_rs::exception::ExceptionHandler>>,
            dynamic_wind: &'a ::scheme_rs::proc::DynamicWind,
        ) -> futures::future::BoxFuture<'a, scheme_rs::proc::Application> {
            #bridge

            Box::pin(
                async move {
                    let result = #impl_name(
                        #( &args[#arg_indices], )*
                        #rest_args
                    ).await;
                    // If the function returned an error, we want to raise
                    // it.
                    let result = match result {
                        Err(err) => return ::scheme_rs::exception::raise(
                            runtime.clone(),
                            err.into(),
                            exception_handler.clone(),
                            dynamic_wind,
                        ),
                        Ok(result) => result,
                    };
                    let cont = cont.clone().try_into().unwrap();
                    ::scheme_rs::proc::Application::new(
                        cont,
                        result,
                        exception_handler.clone(),
                        dynamic_wind.clone(),
                        None // TODO
                    )
                }
            )
        }

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

#[proc_macro_attribute]
pub fn cps_bridge(args: TokenStream, item: TokenStream) -> TokenStream {
    let mut name: Option<LitStr> = None;
    let mut lib: Option<LitStr> = None;
    let mut arg_names: Option<LitStr> = None;
    let bridge_attr_parser = syn::meta::parser(|meta| {
        if meta.path.is_ident("name") {
            name = Some(meta.value()?.parse()?);
            Ok(())
        } else if meta.path.is_ident("lib") {
            lib = Some(meta.value()?.parse()?);
            Ok(())
        } else if meta.path.is_ident("args") {
            arg_names = Some(meta.value()?.parse()?);
            Ok(())
        } else {
            Err(meta.error("unsupported bridge property"))
        }
    });

    parse_macro_input!(args with bridge_attr_parser);

    let mut bridge = parse_macro_input!(item as ItemFn);
    let wrapper_name = Ident::new(&bridge.sig.ident.to_string(), Span::call_site());
    bridge.sig.ident = Ident::new("inner", Span::call_site());
    let impl_name = bridge.sig.ident.clone();

    let (vis, inventory) = if matches!(bridge.vis, Visibility::Public(_)) {
        let name = name.unwrap().value();
        let vis = std::mem::replace(&mut bridge.vis, Visibility::Inherited);
        let lib = lib.unwrap().value();
        let args = arg_names.unwrap().value();
        let mut is_variadic = false;
        let arg_names = args
            .split(" ")
            .filter_map(|x| {
                if x == "." {
                    is_variadic = true;
                    None
                } else {
                    Some(x)
                }
            })
            .collect::<Vec<_>>();
        let num_args = arg_names.len() - is_variadic as usize;
        let inventory = quote! {
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
        };
        (vis, inventory)
    } else {
        if let Some(name) = name {
            return Error::new(
                name.span(),
                "name attribute is not supported for private functions",
            )
            .into_compile_error()
            .into();
        }
        if let Some(lib) = lib {
            return Error::new(
                lib.span(),
                "lib attribute is not supported for private functions",
            )
            .into_compile_error()
            .into();
        }
        if let Some(args) = arg_names {
            return Error::new(
                args.span(),
                "args attribute is not supported for private functions",
            )
            .into_compile_error()
            .into();
        }
        let vis = std::mem::replace(&mut bridge.vis, Visibility::Inherited);
        (vis, quote!())
    };

    quote! {
        #vis fn #wrapper_name<'a>(
            runtime: &'a ::scheme_rs::runtime::Runtime,
            env: &'a [::scheme_rs::gc::Gc<::scheme_rs::value::Value>],
            args: &'a [::scheme_rs::value::Value],
            rest_args: &'a [::scheme_rs::value::Value],
            cont: &'a ::scheme_rs::value::Value,
            exception_handler: &'a Option<::scheme_rs::gc::Gc<::scheme_rs::exception::ExceptionHandler>>,
            dynamic_wind: &'a ::scheme_rs::proc::DynamicWind,
        ) -> futures::future::BoxFuture<'a, scheme_rs::proc::Application> {
            #bridge

            Box::pin(async move {
                let result = #impl_name(
                    runtime,
                    env,
                    args,
                    rest_args,
                    cont,
                    exception_handler,
                    dynamic_wind,
                ).await;
                match result {
                    Err(err) => ::scheme_rs::exception::raise(
                        runtime.clone(),
                        err.into(),
                        exception_handler.clone(),
                        dynamic_wind,
                    ),
                    Ok(result) => result,
                }
            })
        }

        #inventory
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
            GenericParam::Type(ty) => {
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
                Fields::Named(..) => quote! { { #( #field_name, )* .. } },
                _ => quote! { ( #( #field_name ),* ) },
            };
            let field_name = fields.iter().map(|(_, field)| field);
            let fields_destructured_mut = match variant.fields {
                Fields::Named(..) => quote! { { #( #field_name, )* .. } },
                _ => quote! { ( #( #field_name ),* ) },
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
            GenericParam::Type(ty) => {
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
    if let Type::Path(path) = arg {
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
    }
    .flatten()
    {
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
