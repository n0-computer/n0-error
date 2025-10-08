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
/// generates constructors, and `From<T>` impls for fields/variants configured via `#[error(..)]`.
///
/// Recognized attributes:
/// - on items/variants: `#[display("...")]`, `#[error(transparent)]`, `#[error(from_sources)]`, `#[error(std_sources)]`
/// - on fields: `#[error(from)]`, `#[error(std_err)]`, `#[error(stack_err)]`, `#[error(source)]`
#[proc_macro_derive(Error, attributes(display, error))]
pub fn derive_error(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    let item_ident = &input.ident;
    let generics = &input.generics;
    let top = TopOptions::from_attrs(&input.attrs);
    match &input.data {
        syn::Data::Enum(item_enum) => {
            let infos: Result<Vec<_>, _> = item_enum
                .variants
                .iter()
                .map(|v| VariantInfo::parse(&v.ident, &v.fields, &v.attrs, &top))
                .collect();
            match infos {
                Ok(infos) => generate_enum_impls(item_ident, generics, infos),
                Err(err) => err,
            }
        }
        syn::Data::Struct(item) => {
            match VariantInfo::parse(&item_ident, &item.fields, &input.attrs, &top) {
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

#[derive(Default, Clone, Copy)]
struct TopOptions {
    from_sources: bool,
    std_sources: bool,
}

impl TopOptions {
    fn from_attrs(attrs: &[Attribute]) -> Self {
        let mut out = TopOptions::default();
        for attr in attrs.iter().filter(|a| a.path().is_ident("error")) {
            // Parse like #[error(flag, flag2)]
            let flags: syn::Result<Punctuated<Ident, syn::Token![,]>> =
                attr.parse_args_with(Punctuated::<Ident, syn::Token![,]>::parse_terminated);
            if let Ok(flags) = flags {
                for f in flags {
                    let s = f.to_string();
                    match s.as_str() {
                        "from_sources" => out.from_sources = true,
                        "std_sources" => out.std_sources = true,
                        _ => {}
                    }
                }
            }
        }
        out
    }
}

#[derive(Default, Clone, Copy)]
struct FieldOptions {
    is_source: bool,
    from: bool,
    std_err: bool,
    stack_err: bool,
}

impl FieldOptions {
    fn from_field(f: &Field) -> Self {
        let mut fo = FieldOptions::default();
        for attr in f.attrs.iter().filter(|a| a.path().is_ident("error")) {
            let flags: syn::Result<Punctuated<Ident, syn::Token![,]>> =
                attr.parse_args_with(Punctuated::<Ident, syn::Token![,]>::parse_terminated);
            if let Ok(flags) = flags {
                for ident in flags {
                    match ident.to_string().as_str() {
                        "from" => fo.from = true,
                        "std_err" => fo.std_err = true,
                        "stack_err" => fo.stack_err = true,
                        "source" => fo.is_source = true,
                        _ => {}
                    }
                }
            }
        }
        fo
    }
}

// For each variant, capture doc comment text or #[display] attr
struct VariantInfo<'a> {
    ident: Ident,
    fields: Vec<&'a Field>,
    display: Option<proc_macro2::TokenStream>,
    source_kind: SourceKind,
    source_ident: Option<Ident>,
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
        top: &TopOptions,
    ) -> Result<VariantInfo<'a>, proc_macro2::TokenStream> {
        let fields: Vec<&Field> = match fields {
            Fields::Named(ref fields) => fields.named.iter().collect(),
            Fields::Unit => vec![],
            Fields::Unnamed(ref fields) => fields.unnamed.iter().collect(),
        };

        // Figure out source field: explicit #[error(source)] takes precedence, else a field literally named `source`
        let explicit_sources: Vec<&Field> = fields
            .iter()
            .copied()
            .filter(|f| FieldOptions::from_field(f).is_source)
            .collect();
        if explicit_sources.len() > 1 {
            return Err(err(
                ident,
                "Only one field per variant may have #[error(source)]",
            ));
        }

        let source_field: Option<&Field> = if let Some(f) = explicit_sources.first() {
            Some(*f)
        } else {
            fields
                .iter()
                .copied()
                .find(|f| f.ident.as_ref().map(|i| i == "source").unwrap_or(false))
        };

        // parse transparent attribute via #[error(transparent)]
        let transparent = attrs
            .iter()
            .filter(|a| a.path().is_ident("error"))
            .filter_map(|attr| {
                attr.parse_args_with(Punctuated::<Ident, syn::Token![,]>::parse_terminated)
                    .ok()
            })
            .flat_map(|p| p.into_iter())
            .any(|ident| ident == "transparent");
        if transparent && source_field.is_none() {
            return Err(err(
                ident,
                "Variants with #[error(transparent)] require a source field",
            ));
        }

        // Determine source kind and optional From based on field options and top-level switches
        let source_kind = match source_field.as_ref() {
            None => SourceKind::None,
            Some(field) => {
                let fo = FieldOptions::from_field(field);
                if fo.std_err || (top.std_sources && !fo.stack_err) {
                    SourceKind::Std
                } else {
                    SourceKind::Stack
                }
            }
        };

        // parse #[error(from)] on fields, or infer from_sources if configured
        let mut from: Option<&Field> = None;
        for field in fields.iter() {
            let fo = FieldOptions::from_field(field);
            if fo.from {
                if from.is_some() {
                    return Err(err(
                        &field.ident.clone().expect("named field"),
                        "Only one field per variant may have #[error(from)]",
                    ));
                }
                from = Some(*field);
            }
        }
        if from.is_none() && top.from_sources {
            if let Some(sf) = source_field.as_ref() {
                from = Some(*sf);
            }
        }

        Ok(VariantInfo {
            ident: ident.clone(),
            fields,
            display: get_doc_or_display(&attrs),
            source_kind,
            source_ident: source_field.and_then(|f| f.ident.clone()),
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
            #[doc = #doc]
            #[track_caller]
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
            quote! { Self::#v_ident { location #suffix } => location.as_ref() }
        } else {
            let inner = (!vi.fields().is_empty()).then(|| quote!(..));
            quote! { Self::#v_ident { #inner } => None }
        }
    });

    let match_set_location_arms = variants.iter().map(|vi| {
        let v_ident = &vi.ident;
        if vi.location().is_some() {
            let suffix = (vi.fields().len() > 1).then(|| quote!(, ..));
            quote! { Self::#v_ident { location #suffix } => { *location = Some(new_location) }}
        } else {
            let inner = (!vi.fields().is_empty()).then(|| quote!(..));
            quote! { Self::#v_ident { #inner } => {} }
        }
    });

    let match_source_arms = variants.iter().map(|vi| {
        let v_ident = &vi.ident;
        match (vi.source_ident.as_ref(), &vi.source_kind) {
            (Some(src_ident), SourceKind::Stack) => quote! { Self::#v_ident { #src_ident: source, .. } => Some(::n0_error::ErrorRef::Stack(source)), },
            (Some(src_ident), SourceKind::Std) => quote! { Self::#v_ident { #src_ident: source, .. } => Some(::n0_error::ErrorRef::Std(n0_error::StdWrapperRef::new(source))), },
            _ => quote! { Self::#v_ident { .. } => None, }
        }
    });

    let match_std_source_arms = variants.iter().map(|vi| {
        let v_ident = &vi.ident;
        match (vi.source_ident.as_ref(), &vi.source_kind) {
            (Some(src_ident), SourceKind::Std) => {
                quote! { Self::#v_ident { #src_ident: source, .. } => Some(source as & dyn std::error::Error), }
            }
            (Some(src_ident), SourceKind::Stack) => {
                quote! { Self::#v_ident { #src_ident: source, .. } => Some(::n0_error::StackError::as_std(source)), }
            }
            _ => quote! { Self::#v_ident { .. } => None, },
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
            if let Some(src_ident) = &vi.source_ident {
                quote! { Self::#v_ident { #src_ident: source, .. } => { write!(f, "{source}") } }
            } else {
                quote! { Self::#v_ident { .. } => { write!(f, "") } }
            }
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
            fn as_std(&self) -> &(dyn ::std::error::Error + ::std::marker::Send + ::std::marker::Sync + 'static) {
                self
            }

            fn location(&self) -> Option<&::n0_error::Location> {
                match self {
                    #( #match_location_arms, )*
                }
            }

            fn set_location(&mut self, new_location: ::n0_error::Location) {
                match self {
                    #( #match_set_location_arms, )*
                }
            }
            fn source(&self) -> Option<::n0_error::ErrorRef<'_>> {
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
                use ::n0_error::{SourceFormat, StackError, StackErrorExt};
                match self {
                    #( #match_fmt_message_arms, )*
                }?;
                if f.alternate() {
                    self.report().fmt_sources(f, SourceFormat::OneLine)?;
                }
                Ok(())
            }
        }

        impl ::std::fmt::Debug for #enum_ident #generics {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                use ::n0_error::StackErrorExt;
                if f.alternate() {
                    match self {
                        #(#match_debug_arms)*
                    }
                } else {
                    use ::n0_error::{StackError};
                    self.report().full().format(f)?;
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

        // impl #impl_generics ::core::convert::From<#enum_ident> for ::n0_error::AnyError #ty_generics #where_clause {
        //     #[track_caller]
        //     fn from(source: #enum_ident) -> Self {
        //         ::n0_error::AnyError::Stack(::std::boxed::Box::new(source))
        //     }
        // }

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
        quote!(self.location.as_ref())
    } else {
        quote! { None }
    };

    let set_location = if info.location().is_some() {
        quote!(self.location = Some(new_location);)
    } else {
        quote! {}
    };

    let get_error_source = match (info.source_ident.as_ref(), &info.source_kind) {
        (Some(src_ident), SourceKind::Stack) => {
            quote! { Some(::n0_error::ErrorRef::Stack(&self.#src_ident)) }
        }
        (Some(src_ident), SourceKind::Std) => {
            quote! { Some(::n0_error::ErrorRef::Std(&self.#src_ident)) }
        }
        _ => quote! { None },
    };

    let get_std_source = match (info.source_ident.as_ref(), &info.source_kind) {
        (Some(src_ident), SourceKind::Std) => {
            quote! { Some(&self.#src_ident as & dyn std::error::Error) }
        }
        (Some(src_ident), SourceKind::Stack) => {
            quote! { Some(::n0_error::StackError::as_std(&self.#src_ident)) }
        }
        _ => quote! { None },
    };

    let get_transparent = if info.transparent {
        quote! { true }
    } else {
        quote! { false }
    };

    let get_display = {
        if info.transparent {
            if let Some(src_ident) = &info.source_ident {
                quote! { write!(f, "{}", self.#src_ident) }
            } else {
                quote! { write!(f, "") }
            }
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

    // From impls for fields marked with #[error(from)] (or inferred via from_sources)
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
            fn as_std(&self) -> &(dyn ::std::error::Error + ::std::marker::Send + ::std::marker::Sync + 'static) {
                self
            }

            fn location(&self) -> Option<&::n0_error::Location> {
                #get_location
            }
            fn set_location(&mut self, new_location: ::n0_error::Location) {
                #set_location
            }
            fn source(&self) -> Option<::n0_error::ErrorRef<'_>> {
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
                    self.report().fmt_sources(f, SourceFormat::OneLine)?;
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
                    self.report().full().format(f)?;
                }
                Ok(())
            }
        }

        impl ::std::error::Error for #item_ident #generics {
            fn source(&self) -> Option<&(dyn ::std::error::Error + 'static)> {
                #get_std_source
            }
        }

        // impl #impl_generics ::core::convert::From<#item_ident> for ::n0_error::AnyError #ty_generics #where_clause {
        //     #[track_caller]
        //     fn from(source: #item_ident) -> Self {
        //         ::n0_error::AnyError::Stack(::std::boxed::Box::new(source))
        //     }
        // }

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
