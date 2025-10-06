use heck::ToSnakeCase;
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::Parser;
use syn::Variant;
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, Attribute, Expr, Field, Fields,
    FieldsNamed, Ident,
};

/// Attribute macro that adds a `location: Option<::n0_error::Location>`
/// field to all named-field variants of an enum. Does nothing else.
#[proc_macro_attribute]
pub fn add_location(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as syn::Item);
    match &mut input {
        syn::Item::Enum(item_enum) => {
            for variant in item_enum.variants.iter_mut() {
                match &mut variant.fields {
                    Fields::Named(fields) => {
                        let field = quote! { location: Option<::n0_error::Location> };
                        fields
                            .named
                            .push(syn::Field::parse_named.parse2(field).unwrap());
                    }
                    _ => {
                        return err(&variant, "#[add_location] requires named-field variants")
                            .into();
                    }
                }
            }
            quote! { #item_enum }
        }
        _ => {
            return err(&input, "#[add_location] only supports enums").into();
        }
    }
    .into()
}

fn err(ident: impl ToTokens, err: impl ToString) -> proc_macro2::TokenStream {
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
    let r#enum = match &input.data {
        syn::Data::Enum(e) => e,
        _ => return err(&input, "#[derive(Error)] only supports enums").into(),
    };
    let variants_info: Result<Vec<_>, _> = r#enum
        .variants
        .iter()
        .map(|v| VariantInfo::parse(v))
        .collect();
    let variants_info = match variants_info {
        Ok(x) => x,
        Err(err) => return err.into(),
    };
    generate_impls(enum_ident, generics, variants_info).into()
}

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

    fn location(&self) -> Option<&Field> {
        self.fields()
            .iter()
            .find(|f| f.ident.as_ref().unwrap() == "location")
    }

    fn fields_without_location(&self) -> impl Iterator<Item = &Field> {
        self.fields()
            .iter()
            .filter(|f| f.ident.as_ref().unwrap() != "location")
    }

    fn field_idents_without_location(&self) -> impl Iterator<Item = &Ident> {
        self.fields_without_location()
            .map(|f| f.ident.as_ref().unwrap())
    }

    fn parse(v: &'a Variant) -> Result<VariantInfo<'a>, proc_macro2::TokenStream> {
        let fields =
            match v.fields {
                Fields::Named(ref fields) => fields,
                _ => return Err(err(
                    &v.ident,
                    "#[derive(Error)] is only supported on enums with only named-field variants",
                )),
            };

        // get source field (by name `source` if present)
        let source_field = fields
            .named
            .iter()
            .find(|f| f.ident.as_ref().unwrap() == "source");

        // parse transparent attribute
        let transparent = v.attrs.iter().any(|a| a.path().is_ident("transparent"));
        if transparent && source_field.is_none() {
            return Err(err(
                &v.ident,
                "Variants with a #[transparent] attribute need a single `source` field",
            ));
        }

        let source_kind = match source_field.as_ref() {
            None => SourceKind::None,
            Some(field) => {
                if field.attrs.iter().any(|attr| attr.path().is_ident("std")) {
                    SourceKind::Std
                } else {
                    SourceKind::Stack
                }
            }
        };

        // parse #[from] attribute on fields
        let mut from = None;
        for field in fields.named.iter() {
            if field.attrs.iter().any(|attr| attr.path().is_ident("from")) {
                if from.is_some() {
                    return Err(err(
                        &field.ident.clone().expect("named field"),
                        "Only one field per variant may have #[from]",
                    ));
                }
                from = Some(field);
            }
        }

        Ok(VariantInfo {
            ident: v.ident.clone(),
            fields,
            display: get_doc_or_display(&v.attrs),
            source: source_kind,
            transparent,
            from,
        })
    }
}

fn generate_impls(
    enum_ident: &Ident,
    generics: &syn::Generics,
    variants_info: Vec<VariantInfo>,
) -> proc_macro2::TokenStream {
    // Constructors like `fn read(source: ...) -> Self`
    let constructors = variants_info.iter().map(|vi| {
        let v_ident = &vi.ident;
        let fn_ident = Ident::new(&v_ident.to_string().to_snake_case(), v_ident.span());
        let params = vi.fields_without_location().map(|f| {
            let ident = f.ident.as_ref().unwrap();
            let ty = &f.ty;
            quote! { #ident: #ty }
        });
        let names = vi.field_idents_without_location();
        let location = vi
            .location()
            .map(|_| quote!(location: ::n0_error::location()));
        let comma = (location.is_some() && vi.fields().len() > 1).then(|| quote!(,));
        quote! {
            #[track_caller]
            pub fn #fn_ident(#(#params),*) -> Self {
                Self::#v_ident { #(#names),* #comma #location }
            }
        }
    });

    // StackError impl pieces
    let match_location_arms = variants_info.iter().map(|vi| {
        let v_ident = &vi.ident;
        if vi.location().is_some() {
            let suffix = (vi.fields().len() > 1).then(|| quote!(, ..));
            quote! { Self::#v_ident { location #suffix } => *location }
        } else {
            let inner = (!vi.fields().is_empty()).then(|| quote!(..));
            quote! { Self::#v_ident { #inner } => None }
        }
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
            SourceKind::Std => {
                quote! { Self::#v_ident { source, .. } => Some(source as & dyn std::error::Error), }
            }
            SourceKind::Stack => {
                quote! { Self::#v_ident { source, .. } => Some(::n0_error::StackError::as_std(source)), }
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

    let match_fmt_message_arms = variants_info.iter().map(|vi| {
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

    let match_debug_arms = variants_info.iter().map(|vi| {
        let v_ident = &vi.ident;
        let field_idents = vi.field_idents();
        let print = field_idents.map(|ident| {
            let ident_s = ident.to_string();
            quote! { .field(#ident_s, #ident) }
        });
        let names = vi.field_idents();
        let v_name = v_ident.to_string();
        quote! {
            Self::#v_ident { #(#names),* } => {
                f.debug_struct(#v_name)#(#print)*.finish()?;
            }
        }
    });

    // From impls for variants marked with #[from]
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let from_impls = variants_info.iter().filter_map(|vi| vi.from.map(|field| (vi, field))).map(|(vi, field)| {
        let v_ident = &vi.ident;
        let src_ty = &field.ty;
        let src_field = &field.ident;
        let location = vi
            .location()
            .map(|_| quote!(location: ::n0_error::location()));
        let comma = (location.is_some() && vi.fields().len() > 1).then(|| quote!(,));
        quote! {
            impl #impl_generics ::core::convert::From<#src_ty> for #enum_ident #ty_generics #where_clause {
                #[track_caller]
                fn from(source: #src_ty) -> Self {
                    Self::#v_ident { #src_field: source #comma #location }
                }
            }
        }
    });

    quote! {
        impl #enum_ident #generics {
            #( #constructors )*
        }

        impl ::n0_error::StackError for #enum_ident #generics {
            fn as_std(&self) -> &(dyn ::std::error::Error + 'static) {
                self
            }

            fn location(&self) -> Option<::n0_error::Location> {
                match self {
                    #( #match_location_arms, )*
                }
            }
            fn source(&self) -> Option<::n0_error::ErrorSource<'_>> {
                match self {
                    #( #match_source_arms )*
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
                use ::n0_error::{SourceFormat, StackError};
                match self {
                    #( #match_fmt_message_arms, )*
                }?;
                if f.alternate() {
                    self.fmt_sources(f, SourceFormat::OneLine)?;
                }
                Ok(())
            }
        }

        impl ::std::fmt::Debug for #enum_ident #generics {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                if f.alternate() {
                    match self {
                        #(#match_debug_arms)*
                    }
                } else {
                    use ::n0_error::{StackError};
                    self.fmt_full(f)?;
                }
                Ok(())
            }
        }

        impl ::std::error::Error for #enum_ident #generics {
            fn source(&self) -> Option<&(dyn ::std::error::Error + 'static)> {
                match self {
                    #( #match_std_source_arms )*
                }
            }
        }

        impl #impl_generics ::core::convert::From<#enum_ident> for ::n0_error::AnyError #ty_generics #where_clause {
            #[track_caller]
            fn from(source: #enum_ident) -> Self {
                ::n0_error::AnyError::Stack(::std::boxed::Box::new(source))
            }
        }

        #( #from_impls )*
    }
}

fn get_doc_or_display(attrs: &[Attribute]) -> Option<proc_macro2::TokenStream> {
    // Prefer #[display("...")]
    for attr in attrs {
        if attr.path().is_ident("display") {
            // Accept format!-style args: #[display("text {}", arg1, arg2, ...)]
            match attr.parse_args_with(Punctuated::<Expr, syn::Token![,]>::parse_terminated) {
                Ok(args) => {
                    if args.is_empty() {
                        return Some(
                            syn::Error::new_spanned(
                                attr,
                                "#[display(..)] requires at least a format string",
                            )
                            .to_compile_error(),
                        );
                    }
                    let mut it = args.into_iter();
                    let fmt = it.next().unwrap();
                    let rest: Vec<_> = it.collect();
                    return Some(quote! { write!(f, #fmt #(, #rest)* ) });
                }
                Err(e) => {
                    return Some(e.to_compile_error());
                }
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
