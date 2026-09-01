use proc_macro::{self, TokenStream};
use proc_macro2::{Literal, Span};
use quote::{format_ident, quote};
use syn::{
    Attribute, DataEnum, DataStruct, DeriveInput, Error, Expr, ExprClosure, Fields, FnArg,
    GenericParam, Generics, Ident, ItemFn, Lit, LitBool, LitStr, Member, Meta, Pat, PatIdent,
    PatType, Result, ReturnType, Token, Type, TypePath, TypeReference, Visibility, braced,
    bracketed, parenthesized,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
};

/// The `bridge` proc macro allows one to register Scheme procedures written in
/// Rust.
///
/// Rust functions registered with `bridge` must have the following syntax:
/// `async? fn(#[rest_arg]? arg: T, ...) -> R`
///
/// The types of the arguments can be any `T` for which `T: TryFrom<Value>`, or
/// they can be a `Value`. Scheme-rs will throw an excpetion if the value to the
/// function fails this conversion.
///
/// A parameter of type `Option<T>` is optional and receives `None` when the
/// caller omits it. Optional arguments must come at the end of the argument
/// list.
///
/// A parameter marked `#[rest_args]` receives any remaining arguments as a
/// scheme list. Only one argument may be marked this way.
///
/// The return type `R` may be any type convertible into a [`Value`], a tuple
/// of such types to return multiple values, or a `Result` of either.
///
/// Bridge functions can be async if the `async` feature flag is enabled.
///
/// The `bridge` proc macro takes two arguments: `def` which specifies the
/// scheme procedure name and `lib` which specifies the library to register the
/// procedure to. At time of variable resolution, if the library has no scheme
/// code associated with it, all bridge functions registered to that library
/// will be assumed to be public. More control can be given by associating
/// scheme code with the library, in that case bridge functions will need to be
/// made public by putting them in the `export` spec.
///
/// # Capturing the environment
///
/// Procedures that capture their environment (known as closures) can be
/// specified by attaching the `#[env]` attribute to arguments. Procedures that
/// have env arguments will not be registered and therefor must not take a `lib`
/// attribute.
///
/// # Known functions
///
/// If there is no `rest_args` (variadic) parameter and the function takes three
/// or fewer arguments, the function may choose to return a single value or `()`
/// instead of a `Vec` and become "known". Known functions are faster than
/// regular bridge functions and have the added benefit of not having to return
/// a `Vec<Value>` but any type that can be converted into one (although `()` is
/// treated as no return value rather than the empty list in this context).
///
/// ```rust
/// #[bridge(name = "length", lib = "(list-length (1))")]
/// pub fn length(arg: List) -> usize {
///     arg.len()
/// }
/// ```

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

    let mut bridge = parse_macro_input!(item as ItemFn);
    let docs = doc_string(&bridge.attrs);

    let wrapper_name = std::mem::replace(&mut bridge.sig.ident, Ident::new("__inner__", Span::call_site()));
    const MAX_DIRECT_ARGS: usize = 4;

    let mut arg_names = Vec::new();
    let mut bindings = Vec::new();
    let mut args = Vec::new();
    let mut has_cont_barrier = false;
    let mut env_var_idx = 0usize;
    let mut is_variadic = false;
    let mut num_args = 0usize;
    let mut num_required = 0usize;
    let mut num_optional = 0usize;

    for arg in bridge.sig.inputs.iter_mut() {
        let FnArg::Typed(arg) = arg else {
            return Error::new(
                arg.span(),
                "methods cannot be bridge functions"
            )
                .into_compile_error()
                .into();
        };

        let mut is_env_var = false;
        let mut is_rest_args = false;
        let is_optional = is_option(&arg.ty);

        arg.attrs.retain(|attr| {
            if attr.path().is_ident("env") && !is_env_var && !is_rest_args {
                is_env_var = true;
                false
            } else if attr.path().is_ident("rest_args") && !is_env_var && !is_rest_args && !is_variadic {
                is_rest_args = true;
                is_variadic = true;
                false
            } else {
                true
            }
        }
        );

        if is_env_var {
            let env_idx = env_var_idx;
            env_var_idx += 1;
            args.push(convert_arg(quote!((&env[#env_idx]))));
            continue;
        }

        if is_cont_barrier(arg) {
            if has_cont_barrier {
                return Error::new(
                    arg.span(),
                    "cannot have multiple continuation barrier arguments",
                )
                .into_compile_error()
                .into();
            }

            has_cont_barrier = true;
            args.push(convert_arg(quote!((&mut *barrier))));
            continue;
        }

        if is_variadic && !is_rest_args {
            return Error::new(
                arg.span(),
                "regular arguments must come before the #[rest_args] argument",
            )
            .into_compile_error()
            .into();
        }

        if is_optional && (is_env_var || is_rest_args) {
            return Error::new(
                arg.span(),
                "env and #[rest_args] arguments cannot have type Option<T>",
            )
            .into_compile_error()
            .into();
        }

        if num_optional > 0 && !is_optional && !is_rest_args {
            return Error::new(
                arg.span(),
                "required arguments must come before optional (Option<T>) arguments",
            )
            .into_compile_error()
            .into();
        }

        arg_names.push(
            if let PatType { pat, .. } = arg
                && let Pat::Ident(PatIdent { ident, .. }) = pat.as_ref()
            {
                ident.to_string()
            } else {
                format!("arg{num_args}")
            },
        );

        if is_rest_args {
            let conses = (num_args..MAX_DIRECT_ARGS).rev().map(|i| {
                let slot = format_ident!("arg{}", i + 1);
                quote! {
                    if !#slot.is_undefined() {
                        rest_args = ::scheme_rs::value::Value::from(
                            ::scheme_rs::lists::Pair::immutable(#slot, rest_args),
                        );
                    }
                }
            });
            
            bindings.push(quote! {
                #[allow(unused_mut)]
                let mut rest_args = rest;
                #(#conses)*
            });
            args.push(convert_arg(quote!(rest_args)));
            continue;
        }

        let idx = num_args;
        num_args += 1;
        let slot = format_ident!("arg{}", idx + 1);

        if is_optional {
            num_optional += 1;
            if idx >= MAX_DIRECT_ARGS {
                bindings.push(quote! {
                    let (#slot, rest): (::scheme_rs::value::Value, ::scheme_rs::value::Value) =
                        if let Some(pair) = rest.cast::<::scheme_rs::lists::Pair>() {
                            pair.into()
                        } else {
                            (::scheme_rs::value::Value::undefined(), rest)
                        };
                });
            }
            let converted = convert_arg(quote!(#slot));
            args.push(quote! {
                if #slot.is_undefined() {
                    None
                } else {
                    Some(#converted)
                }
            });
            continue;
        }

        num_required += 1;

        if idx < MAX_DIRECT_ARGS {
            bindings.push(quote! {
                if #slot.is_undefined() {
                    return ::scheme_rs::exceptions::raise(
                        ::scheme_rs::exceptions::Exception::wrong_num_of_args(
                            NUM_REQUIRED, #idx,
                        )
                        .into(),
                        barrier,
                    );
                }
            });
        } else {
            bindings.push(quote! {
                let (#slot, rest): (::scheme_rs::value::Value, ::scheme_rs::value::Value) =
                    if let Some(pair) = rest.cast::<::scheme_rs::lists::Pair>() {
                        pair.into()
                    } else {
                        return ::scheme_rs::exceptions::raise(
                            ::scheme_rs::exceptions::Exception::wrong_num_of_args(
                                NUM_REQUIRED, #idx,
                            )
                            .into(),
                            barrier,
                        );
                    };
            });
        }
        args.push(convert_arg(quote!(#slot)));
    }

    let visibility = bridge.vis.clone();

    if name.is_none() && env_var_idx == 0 {
        return Error::new(Span::call_site(), "name attribute is required")
            .into_compile_error()
            .into();
    };

    if name.is_some() && env_var_idx > 0 {
        return Error::new(
            Span::call_site(),
            "name attribute is not allowed with env arguments",
        )
        .into_compile_error()
        .into();
    }

    if lib.is_none() && env_var_idx == 0 {
        return Error::new(Span::call_site(), "lib attribute is required")
            .into_compile_error()
            .into();
    }

    if lib.is_some() && env_var_idx > 0 {
        return Error::new(
            Span::call_site(),
            "lib attribute is not allowed with env arguments",
        )
        .into_compile_error()
        .into();
    }

    if bridge.sig.asyncness.is_none()
        && !is_variadic
        && num_optional == 0
        && (1..=3).contains(&num_args)
        // TODO: we will eventually allow known functions to take the cont
        // barrier
        && !has_cont_barrier
        && env_var_idx == 0
        && let Some(known_ret_type) = is_return_type_known(&bridge.sig.output)
    {
        return codegen_known_bridge(
            &bridge,
            known_ret_type,
            &name.unwrap().value(),
            &lib.unwrap().value(),
            num_args,
            visibility,
            wrapper_name,
            bridge
                .sig
                .inputs
                .iter()
                .enumerate()
                .map(|(i, arg)| {
                    if let FnArg::Typed(PatType { pat, .. }) = arg
                        && let Pat::Ident(PatIdent { ident, .. }) = pat.as_ref()
                    {
                        ident.clone()
                    } else {
                        format_ident!("arg{i}")
                    }
                })
                .collect(),
            arg_names,
            docs,
        );
    }

    if !is_variadic {
        let no_extra = if num_args >= MAX_DIRECT_ARGS {
            let err = too_many_args_error(
                num_optional > 0,
                quote!(NUM_ARGS + ::scheme_rs::lists::list_len(&rest)),
            );
            quote! {
                if !rest.is_null() {
                    return ::scheme_rs::exceptions::raise(#err.into(), barrier);
                }
            }
        } else {
            let next = format_ident!("arg{}", num_args + 1);
            let defined = (num_args..MAX_DIRECT_ARGS).map(|i| {
                let slot = format_ident!("arg{}", i + 1);
                quote!((!#slot.is_undefined()) as usize)
            });
            let err = too_many_args_error(
                num_optional > 0,
                quote!(NUM_ARGS #( + #defined )* + ::scheme_rs::lists::list_len(&rest)),
            );
            quote! {
                if !#next.is_undefined() {
                    return ::scheme_rs::exceptions::raise(#err.into(), barrier);
                }
            }
        };
        bindings.push(no_extra);
    }

    // Bridges with optional arguments are registered as variadic with only
    // their required count, since the registry has no notion of optional
    // arguments.
    let reg_variadic = is_variadic || num_optional > 0;

    let func = if bridge.sig.asyncness.is_some() {
        quote! {
            #visibility fn #wrapper_name<'a>(
                proc: ::scheme_rs::proc::Procedure,
                arg1: ::scheme_rs::value::Value,
                arg2: ::scheme_rs::value::Value,
                arg3: ::scheme_rs::value::Value,
                arg4: ::scheme_rs::value::Value,
                rest: ::scheme_rs::value::Value,
                barrier: &'a mut ::scheme_rs::proc::ContBarrier,
            ) -> futures::future::BoxFuture<'a, scheme_rs::proc::Application> {
                use ::scheme_rs::proc::IntoApplication;

                #bridge

                Box::pin(
                    async move {
                        #[allow(unused)]
                        const NUM_ARGS: usize = #num_args;
                        #[allow(unused)]
                        const NUM_REQUIRED: usize = #num_required;
                        #[allow(unused)]
                        let env: &[::scheme_rs::value::Value] = proc.env();
                        #(#bindings)*

                        __inner__(#(#args,)*).await.into_application(barrier)
                    }
                )
            }
        }
    } else {
        quote! {
            #visibility extern "C" fn #wrapper_name(
                proc: ::scheme_rs::proc::Procedure,
                arg1: ::scheme_rs::value::Value,
                arg2: ::scheme_rs::value::Value,
                arg3: ::scheme_rs::value::Value,
                arg4: ::scheme_rs::value::Value,
                rest: ::scheme_rs::value::Value,
                barrier: &mut ::scheme_rs::proc::ContBarrier<'_>,
                out: &mut ::std::mem::MaybeUninit<::scheme_rs::proc::Application>,
            ) {
                use ::scheme_rs::proc::IntoApplication;

                #bridge

                #[allow(unused)]
                let env: &[::scheme_rs::value::Value] = proc.env();

                let app = (move || -> ::scheme_rs::proc::Application {
                    #[allow(unused)]
                    const NUM_ARGS: usize = #num_args;
                    #[allow(unused)]
                    const NUM_REQUIRED: usize = #num_required;
                    #[allow(unused)]
                    #(#bindings)*

                    __inner__(#(#args,)*).into_application(barrier)
                })();

                out.write(app);
            }
        }
    };

    let registration = if let Some(name) = name
        && let Some(lib) = lib
    {
        let bridge_ty = if bridge.sig.asyncness.is_some() {
            quote!(Async)
        } else {
            quote!(Sync)
        };
        quote! {
            ::scheme_rs::registry::inventory::submit! {
                ::scheme_rs::registry::BridgeFn::new(
                    #name,
                    #lib,
                    #num_required,
                    #reg_variadic,
                    ::scheme_rs::registry::Bridge::#bridge_ty(#wrapper_name),
                    ::scheme_rs::registry::BridgeFnDebugInfo::new(
                        ::std::file!(),
                        ::std::line!(),
                        ::std::column!(),
                        0,
                        &[ #( #arg_names, )* ],
                        #docs,
                    )
                )
            }
        }
    } else {
        quote!()
    };

    quote!{
        #func
        #registration
    }.into()
}

fn convert_arg(expr: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    quote! {
        match #expr.try_into() {
            Ok(ok) => ok,
            Err(err) => {
                return ::scheme_rs::exceptions::raise(err.into(), barrier);
            }
        }
    }
}

fn too_many_args_error(
    has_optional: bool,
    provided: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    if has_optional {
        quote! {
            ::scheme_rs::exceptions::Exception::wrong_num_of_var_args(
                NUM_REQUIRED..NUM_ARGS,
                #provided,
            )
        }
    } else {
        quote! {
            ::scheme_rs::exceptions::Exception::wrong_num_of_args(
                NUM_ARGS,
                #provided,
            )
        }
    }
}

fn is_option(ty: &Type) -> bool {
    if let Type::Path(TypePath { qself: None, path }) = ty
        && let Some(last) = path.segments.last()
    {
        last.ident == "Option"
    } else {
        false
    }
}

#[derive(Clone)]
struct KnownReturnType {
    is_unit: bool,
    is_result: bool,
}

fn is_return_type_known(ret_type: &ReturnType) -> Option<KnownReturnType> {
    let ty = match ret_type {
        ReturnType::Default => {
            return Some(KnownReturnType {
                is_unit: true,
                is_result: false,
            });
        }
        ReturnType::Type(_, ty) => ty.as_ref(),
    };

    // Result<T, _> is known if T is not multiple return values
    if let Type::Path(TypePath { path, .. }) = ty
        && let Some(last) = path.segments.last()
        && last.ident == "Result"
    {
        if let syn::PathArguments::AngleBracketed(args) = &last.arguments
            && args.args.len() == 2
            && let Some(syn::GenericArgument::Type(ok_ty)) = args.args.first()
            && !is_multiple_return_values(ok_ty)
        {
            Some(KnownReturnType {
                is_unit: is_unit(ok_ty),
                is_result: true,
            })
        } else {
            None
        }
    } else if is_multiple_return_values(ty) {
        // Non-result is known if it is not multiple return values
        None
    } else {
        Some(KnownReturnType {
            is_unit: is_unit(ty),
            is_result: false,
        })
    }
}

fn is_multiple_return_values(ty: &Type) -> bool {
    matches!(ty, Type::Tuple(t) if !t.elems.is_empty())
}

fn is_unit(ty: &Type) -> bool {
    matches!(ty, Type::Tuple(t) if t.elems.is_empty())
}

fn is_cont_barrier(PatType { ty, .. }: &PatType) -> bool {
        if let Type::Reference(TypeReference {
            mutability: Some(_),
            elem,
            ..
        }) = ty.as_ref()
        {
            if let Type::Path(TypePath { path, .. }) = elem.as_ref() {
                return path
                    .segments
                    .last()
                    .map(|s| s.ident == "ContBarrier")
                    .unwrap_or(false);
            }
        }
    false
}

fn codegen_known_bridge(
    bridge: &ItemFn,
    ret_type: KnownReturnType,
    name: &str,
    lib: &str,
    num_args: usize,
    visibility: Visibility,
    wrapper_name: Ident,
    args: Vec<Ident>,
    arg_names: Vec<String>,
    docs: String,
) -> TokenStream {
    let call_inner = quote!(
        __inner__(
            #(
                match #args.try_into() {
                    Ok(val) => val,
                    Err(err) => {
                        *error = err.into();
                        return ::scheme_rs::value::Value::undefined();
                    }
                },
            )*
        )
    );
    let call_inner = if ret_type.is_result {
        quote!(
            let res = match #call_inner {
                Ok(res) => res,
                Err(err) => {
                    *error = err.into();
                    return ::scheme_rs::value::Value::undefined();
                }
            };
        )
    } else {
        quote!(
            let res = #call_inner;
        )
    };

    let known_type = format_ident!("Known{}x{}", args.len(), !ret_type.is_unit as usize);

    let return_value = if ret_type.is_unit {
        quote!({
            let () = res;
            ::scheme_rs::value::Value::null()
        })
    } else {
        quote!(res.into())
    };

    quote! {
        #[allow(clippy::needless_question_mark)]
        #visibility extern "C" fn #wrapper_name(
            #( #args: ::scheme_rs::value::Value, )*
            error: &mut Value
        ) -> ::scheme_rs::value::Value {
            #bridge
            #call_inner
            #return_value
        }

        ::scheme_rs::registry::inventory::submit! {
            ::scheme_rs::registry::BridgeFn::new(
                #name,
                #lib,
                #num_args,
                false,
                ::scheme_rs::registry::Bridge::Known(::scheme_rs::proc::KnownFunc::#known_type(
                    #wrapper_name
                )),
                ::scheme_rs::registry::BridgeFnDebugInfo::new(
                    ::std::file!(),
                    ::std::line!(),
                    ::std::column!(),
                    0,
                    &[ #( #arg_names, )* ],
                    #docs,
                )
            )
        }
    }
    .into()
}

fn doc_string(attrs: &[Attribute]) -> String {
    attrs
        .iter()
        .filter_map(|attr| {
            if !attr.path().is_ident("doc") {
                return None;
            }
            let Meta::NameValue(name_value) = &attr.meta else {
                return None;
            };
            let Expr::Lit(expr_lit) = &name_value.value else {
                return None;
            };
            let Lit::Str(lit) = &expr_lit.lit else {
                return None;
            };
            Some(lit.value().trim_start().to_string())
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Derive the `Trace` trait for a type.
///
/// `Trace` assumes that all fields of the type implement `Trace` or are a `Gc`
/// type. Occasionally you want to skip the tracing of a field, perhaps because
/// the type cannot implement `Trace`. The `#[trace(skip)]` attribute specifies
/// that the collector should ignore that field when tracing the type.
///
/// Skipping a field is always safe, but can cause memory leaks if the field
/// being skipped contains a `Gc`.
///
/// `Trace` will also automatically add `Trace` bounds to generic parameters.
/// To avoid this behavior, use the `#[trace(skip_bounds)]` attribute.
///
/// ```rust,ignore
/// #[derive(Trace)]
/// #[trace(skip_bounds)]
/// struct CustomType<T> {
///     #[trace(skip)]
///     inner: TypeYouDoNotOwn<T>
/// }
/// ```
#[proc_macro_derive(Trace, attributes(trace))]
pub fn derive_trace(input: TokenStream) -> TokenStream {
    let DeriveInput {
        attrs,
        ident,
        data,
        generics,
        ..
    } = parse_macro_input!(input);

    let tokens = match data {
        syn::Data::Struct(data_struct) => derive_trace_struct(&attrs, ident, data_struct, generics),
        syn::Data::Enum(data_enum) => derive_trace_enum(&attrs, ident, data_enum, generics),
        syn::Data::Union(union) => Err(Error::new(
            union.union_token.span(),
            "unions are not supported by Trace",
        )),
    };

    tokens.unwrap_or_else(syn::Error::into_compile_error).into()
}

fn derive_trace_struct(
    attrs: &[Attribute],
    name: Ident,
    record: DataStruct,
    generics: Generics,
) -> syn::Result<proc_macro2::TokenStream> {
    let fields = match record.fields {
        Fields::Named(fields) => fields.named,
        Fields::Unnamed(fields) => fields.unnamed,
        _ => {
            return Ok(quote! {
                unsafe impl ::scheme_rs::gc::Trace for #name {
                    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(::scheme_rs::gc::OpaqueGcPtr)) {}

                    unsafe fn finalize(&mut self) {
                        unsafe {
                            ::std::ptr::drop_in_place(self as *mut Self)
                        }
                    }
                }
            });
        }
    };

    let Generics {
        mut params,
        where_clause,
        ..
    } = generics;

    let mut unbound_params = Punctuated::<GenericParam, Token![,]>::new();

    // TODO: Factor this out
    if !skip_bounds(attrs)? {
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
    }

    let (field_visits, field_drops): (Vec<_>, Vec<_>) = fields
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
            let ty = &f.ty;
            let is_gc = is_gc(ty);
            let skip_field = skip_field(&f.attrs)?;
            let visit = if skip_field {
                quote! {}
            } else if is_gc {
                quote! {
                    visitor(self.#ident.as_opaque());
                }
            } else {
                quote! {
                    <#ty as ::scheme_rs::gc::Trace>::visit_children(&self. #ident, visitor);
                }
            };
            let finalize = if skip_field {
                quote! {
                    core::ptr::drop_in_place(&mut self. #ident as *mut #ty);
                }
            } else if is_gc {
                quote! {}
            } else {
                quote! { <#ty as ::scheme_rs::gc::Trace>::finalize(&mut self. #ident); }
            };
            Ok((visit, finalize))
        })
        .collect::<syn::Result<Vec<_>>>()?
        .into_iter()
        .unzip();

    Ok(quote! {
        #[automatically_derived]
        unsafe impl<#params> ::scheme_rs::gc::Trace for #name <#unbound_params>
        #where_clause
        {
            unsafe fn visit_children(&self, visitor: &mut dyn FnMut(::scheme_rs::gc::OpaqueGcPtr)) {
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
    })
}

fn derive_trace_enum(
    attrs: &[Attribute],
    name: Ident,
    data_enum: DataEnum,
    generics: Generics,
) -> syn::Result<proc_macro2::TokenStream> {
    let (visit_match_clauses, finalize_match_clauses): (Vec<_>, Vec<_>) = data_enum
        .variants
        .into_iter()
        .flat_map(|variant| {
            let fields: Vec<_> = match variant.fields {
                Fields::Named(ref named) => named
                    .named
                    .iter()
                    .map(|field| {
                        (
                            field.attrs.clone(),
                            field.ty.clone(),
                            field.ident.as_ref().unwrap().clone(),
                        )
                    })
                    .collect(),
                Fields::Unnamed(ref unnamed) => unnamed
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, field)| {
                        let ident = Ident::new(&format!("t{i}"), Span::call_site());
                        (field.attrs.clone(), field.ty.clone(), ident)
                    })
                    .collect(),
                _ => return None,
            };
            Some((variant, fields))
        })
        .map(|(variant, fields)| {
            let visits = fields
                .iter()
                .map(|(attrs, ty, accessor)| {
                    let skip_field = skip_field(&attrs)?;

                    let visit = if skip_field {
                        quote! {
                            let _ = #accessor;
                        }
                    } else if is_gc(ty) {
                        quote! {
                            visitor(#accessor.as_opaque());
                        }
                    } else {
                        quote! {
                            <#ty as ::scheme_rs::gc::Trace>::visit_children(#accessor, visitor);
                        }
                    };
                    Ok(visit)
                })
                .collect::<syn::Result<Vec<_>>>()?;
            let drops: Vec<_> = fields
                .iter()
                .map(|(attrs, ty, accessor)| {
                    let skip_field = skip_field(&attrs).unwrap();

                    if skip_field {
                        quote! {
                            core::ptr::drop_in_place(#accessor as *mut #ty);
                        }
                    } else if is_gc(ty) {
                        quote! {}
                    } else {
                        quote! {
                            <#ty as ::scheme_rs::gc::Trace>::finalize(#accessor);
                        }
                    }
                })
                .collect();
            let field_name = fields.iter().map(|(_, _, field)| field);
            let fields_destructured = match variant.fields {
                Fields::Named(..) => quote! { { #( #field_name, )* .. } },
                _ => quote! { ( #( #field_name ),* ) },
            };
            let field_name = fields.iter().map(|(_, _, field)| field);
            let fields_destructured_mut = match variant.fields {
                Fields::Named(..) => quote! { { #( #field_name, )* .. } },
                _ => quote! { ( #( #field_name ),* ) },
            };
            let variant_name = variant.ident;
            Ok((
                quote! {
                    Self::#variant_name #fields_destructured => {
                        #(
                            #visits
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
        .collect::<syn::Result<Vec<_>>>()?
        .into_iter()
        .unzip();

    let Generics {
        mut params,
        where_clause,
        ..
    } = generics;

    let mut unbound_params = Punctuated::<GenericParam, Token![,]>::new();

    if !skip_bounds(attrs)? {
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
    }

    Ok(quote! {
        #[automatically_derived]
        unsafe impl<#params> ::scheme_rs::gc::Trace for #name <#unbound_params>
        #where_clause
        {
            unsafe fn visit_children(&self, visitor: &mut dyn FnMut(::scheme_rs::gc::OpaqueGcPtr)) {
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
    })
}

fn skip_field(attrs: &[Attribute]) -> syn::Result<bool> {
    let mut skip_field = false;

    for attr in attrs.iter() {
        if attr.path().is_ident("trace") {
            let nested = attr.parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)?;
            for meta in nested.into_iter() {
                if meta.path().is_ident("skip") {
                    skip_field = true;
                } else if meta.path().is_ident("skip_bounds") {
                    return Err(Error::new(
                        meta.path().span(),
                        "skip_bounds attribute is unsupported in this position",
                    ));
                } else {
                    return Err(Error::new(meta.path().span(), "unrecognized attribute"));
                }
            }
        }
    }

    Ok(skip_field)
}

fn skip_bounds(attrs: &[Attribute]) -> syn::Result<bool> {
    let mut skip_bounds = false;

    for attr in attrs.iter() {
        if attr.path().is_ident("trace") {
            let nested = attr.parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)?;
            for meta in nested.into_iter() {
                if meta.path().is_ident("skip_bounds") {
                    skip_bounds = true;
                } else if meta.path().is_ident("skip") {
                    return Err(Error::new(
                        meta.path().span(),
                        "skip attribute is unsupported in this position",
                    ));
                } else {
                    return Err(Error::new(meta.path().span(), "unrecognized attribute"));
                }
            }
        }
    }

    Ok(skip_bounds)
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
        ::scheme_rs::registry::inventory::submit!(crate::runtime::RuntimeFn::new(
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

enum RtdField {
    Immutable(LitStr),
    Mutable(LitStr),
}

impl Parse for RtdField {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(LitStr) {
            Ok(Self::Immutable(input.parse()?))
        } else {
            let mutability: Ident = input.parse()?;
            let constructor = if mutability == "immutable" {
                RtdField::Immutable
            } else if mutability == "mutable" {
                RtdField::Mutable
            } else {
                return Err(Error::new(
                    mutability.span(),
                    format!("invalid mutability '{mutability}'"),
                ));
            };
            let content;
            parenthesized!(content in input);
            let name: LitStr = content.parse()?;
            Ok((constructor)(name))
        }
    }
}

impl RtdField {
    fn into_token_stream(self) -> proc_macro2::TokenStream {
        match self {
            Self::Immutable(name) => quote! {
                ::scheme_rs::records::Field::Immutable(::scheme_rs::symbols::Symbol::intern(#name))
            },
            Self::Mutable(name) => quote! {
                ::scheme_rs::records::Field::Mutable(::scheme_rs::symbols::Symbol::intern(#name))
            },
        }
    }
}

struct Rtd {
    name: LitStr,
    ty: Type,
    parent: Option<Expr>,
    opaque: Option<Expr>,
    sealed: Option<LitBool>,
    uid: Option<LitStr>,
    constructor: Option<ExprClosure>,
    fields: Option<Vec<RtdField>>,
    lib: Option<LitStr>,
}

impl Parse for Rtd {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut name = None;
        let mut ty = None;
        let mut parent = None;
        let mut opaque = None;
        let mut sealed = None;
        let mut fields = None;
        let mut uid = None;
        let mut lib = None;
        let mut constructor: Option<ExprClosure> = None;
        while !input.is_empty() {
            let keyword: Ident = input.parse()?;
            if keyword == "name" {
                if name.is_some() {
                    return Err(Error::new(keyword.span(), "duplicate definition of name"));
                }
                let _: Token![:] = input.parse()?;
                name = Some(input.parse()?);
            } else if keyword == "ty" {
                if ty.is_some() {
                    return Err(Error::new(keyword.span(), "duplicate definition of ty"));
                }
                let _: Token![:] = input.parse()?;
                ty = Some(input.parse()?);
            } else if keyword == "parent" {
                if parent.is_some() {
                    return Err(Error::new(keyword.span(), "duplicate definition of parent"));
                }
                let _: Token![:] = input.parse()?;
                parent = Some(input.parse()?);
            } else if keyword == "constructor" {
                if constructor.is_some() {
                    return Err(Error::new(
                        keyword.span(),
                        "duplicate definition of constructor",
                    ));
                }
                let _: Token![:] = input.parse()?;
                constructor = Some(input.parse()?);
            } else if keyword == "opaque" {
                if opaque.is_some() {
                    return Err(Error::new(keyword.span(), "duplicate definition of opaque"));
                }
                let _: Token![:] = input.parse()?;
                opaque = Some(input.parse()?);
            } else if keyword == "sealed" {
                if sealed.is_some() {
                    return Err(Error::new(keyword.span(), "duplicate definition of sealed"));
                }
                let _: Token![:] = input.parse()?;
                sealed = Some(input.parse()?);
            } else if keyword == "uid" {
                if uid.is_some() {
                    return Err(Error::new(keyword.span(), "duplicate definition of uid"));
                }
                let _: Token![:] = input.parse()?;
                uid = Some(input.parse()?);
            } else if keyword == "lib" {
                if lib.is_some() {
                    return Err(Error::new(keyword.span(), "duplicate definition of lib"));
                }
                let _: Token![:] = input.parse()?;
                lib = Some(input.parse()?);
            } else if keyword == "fields" {
                if fields.is_some() {
                    return Err(Error::new(keyword.span(), "duplicate definition of fields"));
                }
                let _: Token![:] = input.parse()?;
                let content;
                bracketed!(content in input);
                let punctuated_fields = content.parse_terminated(RtdField::parse, Token![,])?;
                fields = Some(punctuated_fields.into_iter().collect());
            } else {
                return Err(Error::new(keyword.span(), "unknown field name"));
            }

            if !input.is_empty() {
                let _: Token![,] = input.parse()?;
            }
        }

        let Some(name) = name else {
            return Err(Error::new(input.span(), "name field is required"));
        };

        let Some(ty) = ty else {
            return Err(Error::new(input.span(), "ty field is required"));
        };

        if !sealed.as_ref().map_or(false, LitBool::value) && constructor.is_none() {
            return Err(Error::new(
                input.span(),
                "unsealed records must have a constructor defined",
            ));
        }

        Ok(Rtd {
            name,
            ty,
            parent,
            opaque,
            sealed,
            uid,
            constructor,
            fields,
            lib,
        })
    }
}

/// Convenience macro for declaring RecordTypeDescriptors
#[proc_macro]
pub fn rtd(tokens: TokenStream) -> TokenStream {
    let Rtd {
        name,
        ty,
        parent,
        opaque,
        sealed,
        uid,
        constructor,
        fields,
        lib,
    } = parse_macro_input!(tokens as Rtd);

    let num_fields = fields.as_ref().map_or(0, Vec::len);
    let num_embedded_fields = match &parent {
        Some(parent) => quote!({
            let parent = <#parent as ::scheme_rs::records::Embeddable>::rtd();
            parent.embedded_vtable.map_or(0, |vt| vt.embedded_fields) + #num_fields
        }),
        None => {
            quote!(#num_fields)
        }
    };

    let num_inherited_fields = match &parent {
        Some(parent) => quote!({
            let parent = <#parent as ::scheme_rs::records::Embeddable>::rtd();
            parent.num_fields()
        }),
        None => quote!(0),
    };

    let fields = fields
        .into_iter()
        .flatten()
        .map(RtdField::into_token_stream)
        .collect::<Vec<_>>();
    let inherits = match parent {
        Some(parent) => quote!({
            let parent = <#parent as ::scheme_rs::records::Embeddable>::rtd();
            let mut inherits = parent.inherits.clone();
            inherits.insert(::by_address::ByAddress(parent));
            inherits
        }),
        None => quote!(Default::default()),
    };
    let embedded_constructor = match constructor {
        Some(constructor) => {
            let num_inputs = constructor.inputs.len();
            let inputs = 0..num_inputs;
            let types = inputs.clone().map(|_| quote!(::scheme_rs::value::Value));
            quote!(Some({
                ::scheme_rs::records::RustParentConstructor::new(|vals| {
                    if vals.len() != #num_inputs {
                        return Err(::scheme_rs::exceptions::Exception::wrong_num_of_args(#num_inputs, vals.len()));
                    }
                    let constructor: fn(#(#types,)*) -> Result<_, ::scheme_rs::exceptions::Exception> = #constructor;
                    let value = (constructor)(#(vals[#inputs].clone(),)*)?;
                    Ok(Box::new(move |dst: *mut ()| {
                        unsafe {
                            dst.cast::<#ty>().write(value);
                        }
                    }))
                })
            }))
        }
        None => quote!(None),
    };
    let opaque = opaque.unwrap_or_else(|| parse_quote!(false));
    let sealed = sealed.unwrap_or_else(|| parse_quote!(false));
    let uid = match uid {
        Some(uid) => quote!(Some(::scheme_rs::symbols::Symbol::intern(#uid))),
        None => quote!(None),
    };

    let bridge = lib.map(|lib| {
        let name = format!("{}-rtd", name.value());
        quote! {
            #[::scheme_rs_macros::bridge(name = #name, lib = #lib)]
            pub fn rtd() -> ::scheme_rs::value::Value {
                ::scheme_rs::value::Value::from(RTD.clone())
            }
        }
    });

    quote! {
        {
            static RTD: std::sync::LazyLock<std::sync::Arc<::scheme_rs::records::RecordTypeDescriptor>> =
                std::sync::LazyLock::new(|| {
                    std::sync::Arc::new(::scheme_rs::records::RecordTypeDescriptor {
                        name: ::scheme_rs::symbols::Symbol::intern(#name),
                        inherits: #inherits,
                        opaque: #opaque,
                        sealed: #sealed,
                        uid: #uid,
                        num_inherited_fields: #num_inherited_fields,
                        fields: vec![ #( #fields, )* ],
                        embedded_constructor: #embedded_constructor,
                        embedded_vtable: Some(::scheme_rs::records::EmbeddableVTable::new::<#ty>(#num_embedded_fields)),
                    })
                });
            #bridge
            RTD.clone()
        }
    }.into()
}

struct DctField {
    name: Ident,
    ty: Type,
}

impl Parse for DctField {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        let _: Token![:] = input.parse()?;
        let ty: Type = input.parse()?;
        Ok(Self { name, ty })
    }
}

struct DefineConditionType {
    scheme_name: LitStr,
    rust_name: Ident,
    lib: Option<LitStr>,
    parent: Type,
    constructor: Option<ExprClosure>,
    fields: Option<Vec<DctField>>,
    dbg: Option<ExprClosure>,
}

impl Parse for DefineConditionType {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut scheme_name = None;
        let mut rust_name = None;
        let mut parent = None;
        let mut constructor = None;
        let mut fields = None;
        let mut dbg = None;
        let mut lib = None;

        while !input.is_empty() {
            let keyword: Ident = input.parse()?;
            if keyword == "scheme_name" {
                if scheme_name.is_some() {
                    return Err(Error::new(
                        keyword.span(),
                        "duplicate definition of scheme_name",
                    ));
                }
                let _: Token![:] = input.parse()?;
                scheme_name = Some(input.parse()?);
            } else if keyword == "lib" {
                if lib.is_some() {
                    return Err(Error::new(keyword.span(), "duplicate definition of lib"));
                }
                let _: Token![:] = input.parse()?;
                lib = Some(input.parse()?);
            } else if keyword == "rust_name" {
                if rust_name.is_some() {
                    return Err(Error::new(
                        keyword.span(),
                        "duplicate definition of rust_name",
                    ));
                }
                let _: Token![:] = input.parse()?;
                rust_name = Some(input.parse()?);
            } else if keyword == "parent" {
                if parent.is_some() {
                    return Err(Error::new(keyword.span(), "duplicate definition of parent"));
                }
                let _: Token![:] = input.parse()?;
                parent = Some(input.parse()?);
            } else if keyword == "constructor" {
                if constructor.is_some() {
                    return Err(Error::new(
                        keyword.span(),
                        "duplicate definition of constructor",
                    ));
                }
                let _: Token![:] = input.parse()?;
                constructor = Some(input.parse()?);
            } else if keyword == "debug" {
                if dbg.is_some() {
                    return Err(Error::new(keyword.span(), "duplicate definition of debug"));
                }
                let _: Token![:] = input.parse()?;
                dbg = Some(input.parse()?);
            } else if keyword == "fields" {
                if fields.is_some() {
                    return Err(Error::new(keyword.span(), "duplicate definition of fields"));
                }
                let _: Token![:] = input.parse()?;
                let content;
                braced!(content in input);
                let punctuated_fields = content.parse_terminated(DctField::parse, Token![,])?;
                fields = Some(punctuated_fields.into_iter().collect());
            } else {
                return Err(Error::new(keyword.span(), "unknown field name"));
            }

            if !input.is_empty() {
                let _: Token![,] = input.parse()?;
            }
        }

        let Some(scheme_name) = scheme_name else {
            return Err(Error::new(input.span(), "scheme_name field is required"));
        };

        let Some(rust_name) = rust_name else {
            return Err(Error::new(input.span(), "rust_name field is required"));
        };

        let Some(parent) = parent else {
            return Err(Error::new(input.span(), "parent field is required"));
        };

        Ok(DefineConditionType {
            scheme_name,
            rust_name,
            parent,
            constructor,
            fields,
            dbg,
            lib,
        })
    }
}

#[proc_macro]
pub fn define_condition_type(tokens: TokenStream) -> TokenStream {
    let DefineConditionType {
        scheme_name,
        rust_name,
        parent,
        constructor,
        fields,
        dbg,
        lib,
    } = parse_macro_input!(tokens as DefineConditionType);

    let (field_names, field_tys): (Vec<_>, Vec<_>) = fields
        .into_iter()
        .flatten()
        .map(|field| (field.name, field.ty))
        .unzip();

    let field_name_strs = field_names
        .clone()
        .into_iter()
        .map(|field_name| LitStr::new(&field_name.to_string(), field_name.span()));

    let field_idxs = 0..field_names.len();

    let lib = lib.map(|lib| quote!(lib: #lib,));

    let constructor = constructor.map_or_else(
        || {
            quote! {
                constructor: || Ok(#rust_name::default()),
            }
        },
        |constructor| {
            quote!(
                constructor: #constructor,
            )
        },
    );

    let dbg = dbg.map(|dbg| {
        quote!(
            let dbg: fn(&Self, &mut std::fmt::Formatter<'_>) -> std::fmt::Result = #dbg;
            (dbg)(self, f)?;
        )
    });

    quote! {
        #[derive(Clone, ::scheme_rs::gc::Trace)]
        pub struct #rust_name {
            pub parent: #parent,
            #( pub #field_names: #field_tys, )*

        }

        unsafe impl ::scheme_rs::records::Embeddable for #rust_name {
            fn rtd() -> std::sync::Arc<::scheme_rs::records::RecordTypeDescriptor> {
                ::scheme_rs::records::rtd!(
                    ty: #rust_name,
                    name: #scheme_name,
                    parent: #parent,
                    #lib
                    fields: [#(#field_name_strs,)*],
                    #constructor
                )
            }

            fn parent_record(
                &self,
                rtd: &std::sync::Arc<::scheme_rs::records::RecordTypeDescriptor>
            ) -> Option<&dyn ::scheme_rs::records::Embeddable> {
                #parent::rtd()
                    .is_subtype_of(rtd)
                    .then(|| &self.parent as &dyn ::scheme_rs::records::Embeddable)
            }

            fn get_field(&self, k: usize) -> Result<::scheme_rs::value::Value, ::scheme_rs::exceptions::Exception> {
                match k {
                    #(#field_idxs => Ok(::scheme_rs::value::Value::from(self.#field_names.clone())),)*
                    _ => Err(Exception::error(format!("invalid record field: {k}"))),
                }
            }

            fn debug_fmt(
                &self,
                _circular_values: &mut indexmap::IndexMap<::scheme_rs::value::Value, bool>,
                f: &mut std::fmt::Formatter<'_>
            ) -> std::fmt::Result {
                use std::fmt::Debug;
                write!(f, "#<{}", #scheme_name)?;
                #dbg
                write!(f, ">")?;
                Ok(())
            }
        }

        impl std::fmt::Debug for #rust_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.debug_fmt(&mut indexmap::IndexMap::default(), f)
            }
        }

    }
    .into()
}

// Internal use only:

#[proc_macro_attribute]
pub fn maybe_async(_args: TokenStream, item: TokenStream) -> TokenStream {
    let func = parse_macro_input!(item as ItemFn);
    let mut async_func = func.clone();
    async_func.sig.asyncness = Some(Token![async](Span::call_site()));
    quote! {
        #[cfg(not(feature = "async"))]
        #func

        #[cfg(feature = "async")]
        #async_func
    }
    .into()
}

#[proc_macro]
pub fn maybe_await(tokens: TokenStream) -> TokenStream {
    let tokens = proc_macro2::TokenStream::from(tokens);
    quote! {
        {
            #[cfg(not(feature = "async"))]
            let result = #tokens ;

            #[cfg(feature = "async")]
            let result = #tokens .await;

            result
        }
    }
    .into()
}

#[proc_macro]
pub fn maybe_await_boxed(tokens: TokenStream) -> TokenStream {
    let tokens = proc_macro2::TokenStream::from(tokens);
    quote! {
        {
            #[cfg(not(feature = "async"))]
            let result = #tokens ;

            #[cfg(feature = "async")]
            let result = Box::pin(async move { #tokens .await }).await;

            result
        }
    }
    .into()
}
