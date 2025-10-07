use heck::ToSnakeCase;
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, Attribute, Expr, Field, Fields,
    FieldsNamed, Ident,
};

/// Attribute macro that adds a `location: Option<::n0_error::Location>`
/// field to a struct or to all named-field variants of an enum. Does nothing else.
///
/// If the struct or an enum variant is currently a unit kind, it will be converted
/// to a named-field kind.
///
/// Tuple structs or variants are not supported.
#[proc_macro_attribute]
pub fn add_location(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as syn::Item);
    match &mut input {
        syn::Item::Enum(item_enum) => {
            for variant in item_enum.variants.iter_mut() {
                if let Err(err) = add_location_field(&mut variant.fields) {
                    return err.into();
                }
            }
            quote! { #item_enum }
        }
        syn::Item::Struct(item_struct) => {
            if let Err(err) = add_location_field(&mut item_struct.fields) {
                return err.into();
            }
            quote! { #item_struct }
        }
        _ => {
            return err(&input, "#[add_location] only supports enums and structs").into();
        }
    }
    .into()
}

fn add_location_field(fields: &mut Fields) -> Result<(), TokenStream> {
    let field = parse_quote! { location: Option<::n0_error::Location> };
    match fields {
        Fields::Named(fields) => {
            fields.named.push(field);
            Ok(())
        }
        Fields::Unit => {
            let mut named = FieldsNamed {
                brace_token: Default::default(),
                named: Default::default(),
            };
            named.named.push(field);
            *fields = Fields::Named(named);
            Ok(())
        }
        _ => Err(err(
            &fields,
            "#[add_location] does not support tuple variants or structs",
        )
        .into()),
    }
}

/// Derive macro that implements StackError, Display, Debug, std::error::Error,
/// generates constructors, and `From<T>` impls for fields marked with `#[from]`.
///
/// Recognized attributes:
/// - on variants / the struct item: `#[display("...")]`, `#[transparent]`
/// - on fields: `#[from]`, `#[std]` (mark std::error::Error source)
#[proc_macro_derive(Error, attributes(display, transparent, from, std))]
pub fn derive_error(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    let item_ident = &input.ident;
    let generics = &input.generics;
    match &input.data {
        syn::Data::Enum(item_enum) => {
            let infos: Result<Vec<_>, _> = item_enum
                .variants
                .iter()
                .map(|v| VariantInfo::parse(&v.ident, &v.fields, &v.attrs))
                .collect();
            match infos {
                Ok(infos) => generate_enum_impls(item_ident, generics, infos),
                Err(err) => err,
            }
        }
        syn::Data::Struct(item) => {
            match VariantInfo::parse(&item_ident, &item.fields, &input.attrs) {
                Ok(info) => generate_struct_impl(item_ident, generics, info),
                Err(err) => err,
            }
        }
        _ => err(&input, "#[derive(Error)] only supports enums or structs"),
    }
    .into()
}

enum SourceKind {
    None,
    Stack,
    Std,
}

// For each variant, capture doc comment text or #[display] attr
struct VariantInfo<'a> {
    ident: Ident,
    fields: Vec<&'a Field>,
    display: Option<proc_macro2::TokenStream>,
    source: SourceKind,
    transparent: bool,
    from: Option<&'a Field>,
}

impl<'a> VariantInfo<'a> {
    fn fields(&self) -> &Vec<&Field> {
        &self.fields
    }

    fn field_idents(&self) -> impl Iterator<Item = &Ident> {
        self.fields().iter().map(|f| f.ident.as_ref().unwrap())
    }

    fn location(&self) -> Option<&Field> {
        self.fields()
            .iter()
            .copied()
            .find(|f| f.ident.as_ref().unwrap() == "location")
    }

    fn fields_without_location(&self) -> impl Iterator<Item = &Field> {
        self.fields()
            .iter()
            .copied()
            .filter(|f| f.ident.as_ref().unwrap() != "location")
    }

    fn field_idents_without_location(&self) -> impl Iterator<Item = &Ident> {
        self.fields_without_location()
            .map(|f| f.ident.as_ref().unwrap())
    }

    fn parse(
        ident: &Ident,
        fields: &'a Fields,
        attrs: &[Attribute],
    ) -> Result<VariantInfo<'a>, proc_macro2::TokenStream> {
        let fields: Vec<&Field> = match fields {
            Fields::Named(ref fields) => fields.named.iter().collect(),
            Fields::Unit => vec![],
            Fields::Unnamed(ref fields) => fields.unnamed.iter().collect(),
        };

        // get source field (by name `source` if present)
        let source_field = fields
            .iter()
            .find(|f| f.ident.as_ref().map(|i| i == "source").unwrap_or(false));

        // parse transparent attribute
        let transparent = attrs.iter().any(|a| a.path().is_ident("transparent"));
        if transparent && source_field.is_none() {
            return Err(err(
                ident,
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
        for field in fields.iter() {
            if field.attrs.iter().any(|attr| attr.path().is_ident("from")) {
                if from.is_some() {
                    return Err(err(
                        &field.ident.clone().expect("named field"),
                        "Only one field per variant may have #[from]",
                    ));
                }
                from = Some(*field);
            }
        }

        Ok(VariantInfo {
            ident: ident.clone(),
            fields,
            display: get_doc_or_display(&attrs),
            source: source_kind,
            transparent,
            from,
        })
    }
}

fn generate_enum_impls(
    enum_ident: &Ident,
    generics: &syn::Generics,
    variants: Vec<VariantInfo>,
) -> proc_macro2::TokenStream {
    // Constructors like `fn read(source: ...) -> Self`
    let constructors = variants.iter().map(|vi| {
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
        let doc = format!("Creates a new [`Self::{}`] error.", v_ident);
        quote! {
            #[track_caller]
            #[doc = #doc]
            pub fn #fn_ident(#(#params),*) -> Self {
                Self::#v_ident { #(#names),* #comma #location }
            }
        }
    });

    // StackError impl pieces
    let match_location_arms = variants.iter().map(|vi| {
        let v_ident = &vi.ident;
        if vi.location().is_some() {
            let suffix = (vi.fields().len() > 1).then(|| quote!(, ..));
            quote! { Self::#v_ident { location #suffix } => *location }
        } else {
            let inner = (!vi.fields().is_empty()).then(|| quote!(..));
            quote! { Self::#v_ident { #inner } => None }
        }
    });

    let match_source_arms = variants.iter().map(|vi| {
        let v_ident = &vi.ident;
        match vi.source {
            SourceKind::Stack => quote! { Self::#v_ident { source, .. } => Some(::n0_error::ErrorSource::Stack(source)), },
            SourceKind::Std => quote! { Self::#v_ident { source, .. } => Some(::n0_error::ErrorSource::Std(source)), },
            SourceKind::None => quote! { Self::#v_ident { .. } => None, }
        }
    });

    let match_std_source_arms = variants.iter().map(|vi| {
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

    let match_transparent_arms = variants.iter().map(|vi| {
        let v_ident = &vi.ident;
        if vi.transparent {
            quote! { Self::#v_ident { .. } => true }
        } else {
            quote! { Self::#v_ident { .. } => false }
        }
    });

    let match_fmt_message_arms = variants.iter().map(|vi| {
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

    let match_debug_arms = variants.iter().map(|vi| {
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
    let from_impls = variants.iter().filter_map(|vi| vi.from.map(|field| (vi, field))).map(|(vi, field)| {
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

fn generate_struct_impl(
    item_ident: &Ident,
    generics: &syn::Generics,
    info: VariantInfo,
) -> proc_macro2::TokenStream {
    let constructor = {
        let params = info.fields_without_location().map(|f| {
            let ident = f.ident.as_ref().unwrap();
            let ty = &f.ty;
            quote! { #ident: #ty }
        });
        let names = info.field_idents_without_location();
        let location = info
            .location()
            .map(|_| quote!(location: ::n0_error::location()));
        let comma = (location.is_some() && info.fields().len() > 1).then(|| quote!(,));
        let doc = format!("Creates a new [`{}`] error.", item_ident);
        quote! {
            #[doc = #doc]
            #[track_caller]
            pub fn new(#(#params),*) -> Self {
                Self { #(#names),* #comma #location }
            }
        }
    };
    let get_location = if info.location().is_some() {
        quote!(self.location)
    } else {
        quote! { None }
    };

    let get_error_source = match info.source {
        SourceKind::Stack => quote! { Some(::n0_error::ErrorSource::Stack(&self.source)) },
        SourceKind::Std => quote! { Some(::n0_error::ErrorSource::Std(&self.source)) },
        SourceKind::None => quote! { None },
    };

    let get_std_source = match info.source {
        SourceKind::Std => quote! { Some(source as & dyn std::error::Error) },
        SourceKind::Stack => quote! { Some(::n0_error::StackError::as_std(source)) },
        SourceKind::None => quote! { None },
    };

    let get_transparent = if info.transparent {
        quote! { true }
    } else {
        quote! { false }
    };

    let get_display = {
        if info.transparent {
            quote! { write!(f, "{}", self.source) }
        } else {
            match &info.display {
                Some(expr) => {
                    let mc = (!info.fields().is_empty()).then(|| quote!(,));
                    let names = info.field_idents();
                    quote! {
                        #[allow(unused)]
                        let Self { #(#names),* #mc .. } = self;
                        #expr
                    }
                }
                None => {
                    // Fallback to struct name
                    let text = info.ident.to_string();
                    quote! { write!(f, #text) }
                }
            }
        }
    };

    let get_debug = {
        let print = info.field_idents().map(|ident| {
            let ident_s = ident.to_string();
            quote! { .field(#ident_s, &self.#ident) }
        });
        let item_name = info.ident.to_string();
        quote! {
            f.debug_struct(#item_name)#(#print)*.finish()?;
        }
    };

    // From impls for variants marked with #[from]
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let get_from = if let Some(field) = info.from {
        let src_ty = &field.ty;
        let src_field = &field.ident;
        let location = info
            .location()
            .map(|_| quote!(location: ::n0_error::location()));
        let comma = (location.is_some() && info.fields().len() > 1).then(|| quote!(,));
        Some(quote! {
            impl #impl_generics ::core::convert::From<#src_ty> for #item_ident #ty_generics #where_clause {
                #[track_caller]
                fn from(source: #src_ty) -> Self {
                    Self { #src_field: source #comma #location }
                }
            }
        })
    } else {
        None
    };

    quote! {
        impl #impl_generics #item_ident #ty_generics #where_clause {
            #constructor
        }

        impl ::n0_error::StackError for #item_ident #generics {
            fn as_std(&self) -> &(dyn ::std::error::Error + 'static) {
                self
            }

            fn location(&self) -> Option<::n0_error::Location> {
                #get_location
            }
            fn source(&self) -> Option<::n0_error::ErrorSource<'_>> {
                #get_error_source
            }
            fn is_transparent(&self) -> bool {
                #get_transparent
            }
        }

        impl ::std::fmt::Display for #item_ident #generics {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                use ::n0_error::{SourceFormat, StackError};
                #get_display?;
                if f.alternate() {
                    self.fmt_sources(f, SourceFormat::OneLine)?;
                }
                Ok(())
            }
        }

        impl ::std::fmt::Debug for #item_ident #generics {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                if f.alternate() {
                    #get_debug
                } else {
                    use ::n0_error::{StackError};
                    self.fmt_full(f)?;
                }
                Ok(())
            }
        }

        impl ::std::error::Error for #item_ident #generics {
            fn source(&self) -> Option<&(dyn ::std::error::Error + 'static)> {
                #get_std_source
            }
        }

        impl #impl_generics ::core::convert::From<#item_ident> for ::n0_error::AnyError #ty_generics #where_clause {
            #[track_caller]
            fn from(source: #item_ident) -> Self {
                ::n0_error::AnyError::Stack(::std::boxed::Box::new(source))
            }
        }

        #get_from
    }
}

fn get_doc_or_display(attrs: &[Attribute]) -> Option<proc_macro2::TokenStream> {
    // Prefer #[display("...")]
    if let Some(attr) = attrs.iter().find(|a| a.path().is_ident("display")) {
        // Accept format!-style args: #[display("text {}", arg1, arg2, ...)]
        match attr.parse_args_with(Punctuated::<Expr, syn::Token![,]>::parse_terminated) {
            Ok(args) if args.is_empty() => Some(err(
                attr,
                "#[display(..)] requires at least a format string",
            )),
            Ok(args) => {
                let mut it = args.into_iter();
                let fmt = it.next().unwrap();
                let rest: Vec<_> = it.collect();
                Some(quote! { write!(f, #fmt #(, #rest)* ) })
            }
            Err(e) => Some(e.to_compile_error()),
        }
    } else {
        // Otherwise collect doc lines: #[doc = "..."]
        let docs: Vec<String> = attrs
            .iter()
            .filter(|a| a.path().is_ident("doc"))
            .filter_map(|attr| {
                let s = attr.meta.require_name_value().ok()?;
                match &s.value {
                    syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Str(s),
                        ..
                    }) => Some(s.value().trim().to_string()),
                    _ => None,
                }
            })
            .collect();
        if docs.is_empty() {
            None
        } else {
            let doc = docs.join("\n");
            Some(quote! { write!(f, #doc) })
        }
    }
}

fn err(ident: impl ToTokens, err: impl ToString) -> proc_macro2::TokenStream {
    syn::Error::new_spanned(ident, err.to_string())
        .to_compile_error()
        .to_token_stream()
}
