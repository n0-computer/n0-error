use heck::ToSnakeCase;
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::Parser;
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, Attribute, DataEnum, Field, Fields,
    FieldsNamed, Ident,
};

/// Attribute macro that adds a `location: &'static ::std::panic::Location<'static>`
/// field to all named-field variants of an enum. Does nothing else.
#[proc_macro_attribute]
pub fn add_location(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as syn::Item);
    let out = match &mut input {
        syn::Item::Enum(item_enum) => {
            for variant in item_enum.variants.iter_mut() {
                match &mut variant.fields {
                    Fields::Named(fields) => {
                        let has_location = fields
                            .named
                            .iter()
                            .any(|f| f.ident.as_ref().map(|i| i == "location").unwrap_or(false));
                        if !has_location {
                            // Inject `location` at the end.
                            fields.named.push(
                                syn::Field::parse_named
                                    .parse2(quote! {
                                        location: &'static ::std::panic::Location<'static>
                                    })
                                    .expect("failed to parse injected location field"),
                            );
                        }
                    }
                    _ => {
                        return syn::Error::new_spanned(
                            &variant.ident,
                            "#[add_location] requires named-field variants",
                        )
                        .to_compile_error()
                        .into();
                    }
                }
            }
            quote! { #item_enum }
        }
        _ => {
            return syn::Error::new_spanned(&input, "#[add_location] only supports enums")
                .to_compile_error()
                .into();
        }
    };
    out.into()
}

fn err(ident: &Ident, err: impl ToString) -> proc_macro2::TokenStream {
    return syn::Error::new_spanned(ident, err.to_string())
        .to_compile_error()
        .to_token_stream();
}

/// Derive macro that implements StackError, Display, Debug, std::error::Error,
/// generates constructors, and `From<T>` impls for fields marked with `#[from]`.
///
/// Recognized attributes:
/// - on variants: `#[display("...")]`, `#[transparent]`
/// - on fields: `#[from]`, `#[std]` (mark std::error::Error source)
#[proc_macro_derive(Error, attributes(display, transparent, from, std))]
pub fn derive_error(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    let enum_ident = &input.ident;
    let generics = &input.generics;
    let data_enum = match &input.data {
        syn::Data::Enum(e) => DataEnum {
            enum_token: e.enum_token,
            brace_token: e.brace_token,
            variants: e.variants.clone(),
        },
        _ => {
            return syn::Error::new_spanned(&input, "#[derive(Error)] only supports enums")
                .to_compile_error()
                .into();
        }
    };
    let out = generate_impls(enum_ident, &data_enum, generics);
    out.into()
}

fn generate_impls(
    enum_ident: &Ident,
    data: &DataEnum,
    generics: &syn::Generics,
) -> proc_macro2::TokenStream {
    enum SourceKind {
        None,
        Stack,
        Std,
    }

    // For each variant, capture doc comment text or #[display] attr
    struct VariantInfo<'a> {
        ident: Ident,
        fields: &'a FieldsNamed,
        display: Option<proc_macro2::TokenStream>,
        source: SourceKind,
        transparent: bool,
        from: Option<&'a Field>,
    }

    impl<'a> VariantInfo<'a> {
        fn fields(&self) -> &Punctuated<Field, Comma> {
            &self.fields.named
        }

        fn field_idents(&self) -> impl Iterator<Item = &Ident> {
            self.fields().iter().map(|f| f.ident.as_ref().unwrap())
        }
    }

    let mut variants_info = vec![];
    for v in data.variants.iter() {
        let fields =
            match v.fields {
                Fields::Named(ref fields) => fields,
                _ => return err(
                    &v.ident,
                    "#[derive(Error)] is only supported on enums with only named-field variants",
                ),
            };

        // get source field (by name `source` if present)
        let source_field = fields
            .named
            .iter()
            .find(|f| f.ident.as_ref().unwrap() == "source");
        let has_source = source_field.is_some();

        // parse transparent attribute
        let mut transparent = false;
        for attr in &v.attrs {
            if attr.path().is_ident("transparent") {
                transparent = true;
                if !has_source || fields.named.len() > 1 {
                    return err(
                        &v.ident,
                        "Variants with a #[transparent] attribute need a single `source` field",
                    );
                }
            }
        }

        let source_kind = {
            match source_field.as_ref() {
                None => SourceKind::None,
                Some(field) => {
                    if field.attrs.iter().any(|attr| attr.path().is_ident("std")) {
                        SourceKind::Std
                    } else {
                        SourceKind::Stack
                    }
                }
            }
        };

        // parse #[from] attribute on fields
        let mut from = None;
        for field in fields.named.iter() {
            if field.attrs.iter().any(|attr| attr.path().is_ident("from")) {
                if from.is_some() {
                    return err(
                        &field.ident.clone().expect("named field"),
                        "Only one field per variant may have #[from]",
                    );
                }
                from = Some(field);
            }
        }

        variants_info.push(VariantInfo {
            ident: v.ident.clone(),
            fields,
            display: get_doc_or_display(&v.attrs),
            source: source_kind,
            transparent,
            from,
        })
    }

    // Constructors like `fn read(source: ...) -> Self`
    let constructors = variants_info.iter().map(|vi| {
        let v_ident = &vi.ident;
        let fn_ident = Ident::new(&v_ident.to_string().to_snake_case(), v_ident.span());
        let params = vi
            .fields
            .named
            .iter()
            .filter(|f| f.ident.as_ref().map(|i| i != "location").unwrap_or(true))
            .map(|f| {
                let ident = f.ident.as_ref().unwrap();
                let ty = &f.ty;
                quote! { #ident: #ty }
            });
        let names = vi.field_idents().filter(|i| i != &"location");
        let comma = (!vi.fields().is_empty()
            && vi
                .fields
                .named
                .iter()
                .any(|f| f.ident.as_ref().map(|i| i != "location").unwrap_or(true)))
        .then(|| quote!(,));
        quote! {
            #[track_caller]
            pub fn #fn_ident(#(#params),*) -> Self {
                Self::#v_ident { #(#names),* #comma location: ::std::panic::Location::caller() }
            }
        }
    });

    // StackError impl pieces
    let match_location_arms = variants_info.iter().map(|vi| {
        let v_ident = &vi.ident;
        let inner = if vi.fields.named.is_empty() {
            quote!(location)
        } else {
            quote!(location, ..)
        };
        quote! { Self::#v_ident { #inner } => location }
    });

    let match_source_arms = variants_info.iter().map(|vi| {
        let v_ident = &vi.ident;
        match vi.source {
            SourceKind::Stack => quote! { Self::#v_ident { source, .. } => Some(::n0_error::ErrorSource::Stack(source)), },
            SourceKind::Std => quote! { Self::#v_ident { source, .. } => Some(::n0_error::ErrorSource::Std(source)), },
            SourceKind::None => quote! { Self::#v_ident { .. } => None, }
        }
    });

    let match_std_source_arms = variants_info.iter().map(|vi| {
        let v_ident = &vi.ident;
        match vi.source {
            SourceKind::Stack | SourceKind::Std => {
                quote! { Self::#v_ident { source, .. } => Some(source as & dyn std::error::Error), }
            }
            SourceKind::None => quote! { Self::#v_ident { .. } => None, },
        }
    });

    let match_transparent_arms = variants_info.iter().map(|vi| {
        let v_ident = &vi.ident;
        if vi.transparent {
            quote! { Self::#v_ident { .. } => true }
        } else {
            quote! { Self::#v_ident { .. } => false }
        }
    });

    let match_display_plain_arms = variants_info.iter().map(|vi| {
        let v_ident = &vi.ident;
        if vi.transparent {
            quote! { Self::#v_ident { source, .. } => { write!(f, "{source}") } }
        } else {
            match &vi.display {
                Some(expr) => {
                    let mc = (!vi.fields().is_empty()).then(|| quote!(,));
                    let names = vi.field_idents();
                    quote! {
                        #[allow(unused)]
                        Self::#v_ident { #(#names),* #mc .. } => { #expr }
                    }
                }
                None => {
                    // Fallback to variant name
                    let text = v_ident.to_string();
                    quote! { Self::#v_ident { .. } => write!(f, #text) }
                }
            }
        }
    });

    // From impls for variants marked with #[from]
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let from_impls = variants_info.iter().filter_map(|vi| vi.from.map(|field| (vi, field))).map(|(vi, field)| {
        let v_ident = &vi.ident;
        let src_ty = &field.ty;
        let src_field = &field.ident;
        quote! {
            impl #impl_generics ::core::convert::From<#src_ty> for #enum_ident #ty_generics #where_clause {
                #[track_caller]
                fn from(source: #src_ty) -> Self {
                    Self::#v_ident { #src_field: source, location: ::std::panic::Location::caller() }
                }
            }
        }
    });

    quote! {
        impl #enum_ident #generics {
            #( #constructors )*
        }

        impl ::n0_error::StackError for #enum_ident #generics {
            fn location(&self) -> &'static ::std::panic::Location<'static> {
                match self {
                    #( #match_location_arms, )*
                }
            }
            fn source(&self) -> Option<::n0_error::ErrorSource<'_>> {
                match self {
                    #( #match_source_arms )*
                }
            }
            fn display_plain(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                match self {
                    #( #match_display_plain_arms, )*
                }
            }

            fn is_transparent(&self) -> bool {
                match self {
                    #( #match_transparent_arms, )*
                }
            }
        }

        impl ::std::fmt::Display for #enum_ident #generics {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                use ::n0_error::{ErrorSource, SourceFormat, StackError};
                self.display_plain(f)?;
                if f.alternate() {
                    self.fmt_sources(f, SourceFormat::OneLine)?;
                }
                Ok(())
            }
        }

        impl ::std::fmt::Debug for #enum_ident #generics {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                use ::n0_error::{ErrorSource, SourceFormat, StackError, DisplayOpts};
                let opts = DisplayOpts::default()
                    .with_location()
                    .with_sources(SourceFormat::MultiLine);
                self.fmt_with_opts(f, opts)
            }
        }
        impl ::std::error::Error for #enum_ident #generics {
            fn source(&self) -> Option<&(dyn ::std::error::Error + 'static)> {
                match self {
                    #( #match_std_source_arms )*
                }
            }
        }
        #( #from_impls )*
    }
}

fn get_doc_or_display(attrs: &[Attribute]) -> Option<proc_macro2::TokenStream> {
    // Prefer #[display("...")]
    for attr in attrs {
        if attr.path().is_ident("display") {
            if let Ok(lit) = attr.parse_args::<syn::LitStr>() {
                return Some(quote! { write!(f, #lit) });
            } else {
                return Some(
                    syn::Error::new_spanned(
                        attr,
                        "#[display] expects a string literal: #[display(\"...\")] ",
                    )
                    .to_compile_error(),
                );
            }
        }
    }
    // Otherwise collect doc lines: #[doc = "..."]
    let mut docs: Vec<String> = Vec::new();
    for attr in attrs {
        if attr.path().is_ident("doc") {
            if let syn::Meta::NameValue(nv) = &attr.meta {
                if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(s),
                    ..
                }) = &nv.value
                {
                    docs.push(s.value().trim().to_string());
                }
            }
        }
    }
    if docs.is_empty() {
        None
    } else {
        let doc = docs.join("\n");
        Some(quote! { write!(f, #doc) })
    }
}
