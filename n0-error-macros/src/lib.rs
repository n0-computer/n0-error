use darling::FromAttributes;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, Attribute, DeriveInput, Expr, Field,
    Fields, FieldsNamed, Ident,
};

#[proc_macro_attribute]
pub fn stack_error(args: TokenStream, item: TokenStream) -> TokenStream {
    match stack_error_inner(args, parse_macro_input!(item as syn::Item)) {
        Err(err) => err.to_compile_error().into(),
        Ok(tokens) => tokens.into(),
    }
}

fn stack_error_inner(
    args: TokenStream,
    mut input: syn::Item,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    let args: StackErrAttrArgs = syn::parse(args.clone())?;
    match &mut input {
        syn::Item::Enum(item) => {
            if args.add_meta {
                for variant in item.variants.iter_mut() {
                    add_meta_field(&mut variant.fields)?;
                }
            }
            modify_attrs(&args, &mut item.attrs)?;
            Ok(quote! { #item })
        }
        syn::Item::Struct(item) => {
            if args.add_meta {
                add_meta_field(&mut item.fields)?;
            }
            modify_attrs(&args, &mut item.attrs)?;
            Ok(quote! { #item })
        }
        _ => Err(err(
            &input,
            "#[stack_error] only supports enums and structs",
        )),
    }
}

fn modify_attrs(args: &StackErrAttrArgs, attrs: &mut Vec<Attribute>) -> Result<(), syn::Error> {
    attrs.retain(|attr| !attr.path().is_ident("stackerr"));
    if args.derive {
        attrs.insert(0, parse_quote!(#[derive(::n0_error::StackError)]));
    }
    let error_args: Vec<_> = [
        args.from_sources.then(|| quote!(from_sources)),
        args.std_sources.then(|| quote!(then_sources)),
    ]
    .into_iter()
    .flatten()
    .collect();
    if !error_args.is_empty() {
        attrs.push(parse_quote!(#[error(#(#error_args),*)]))
    }

    Ok(())
}

/// Attribute macro that adds a `meta: ::n0_error::Meta` field to a struct or
/// to all named-field variants of an enum. Does nothing else.
///
/// If the struct or an enum variant is currently a unit kind, it will be converted
/// to a named-field kind.
///
/// Tuple structs or variants are supported: for tuple items, the `meta`
/// field is appended as the last unnamed field.
#[proc_macro_attribute]
pub fn add_meta(_attr: TokenStream, item: TokenStream) -> TokenStream {
    match add_meta_inner(parse_macro_input!(item as syn::Item)) {
        Err(err) => err.to_compile_error().into(),
        Ok(tokens) => tokens.into(),
    }
}

fn add_meta_inner(mut input: syn::Item) -> Result<proc_macro2::TokenStream, syn::Error> {
    match &mut input {
        syn::Item::Enum(item_enum) => {
            for variant in item_enum.variants.iter_mut() {
                add_meta_field(&mut variant.fields)?;
            }
            Ok(quote! { #item_enum })
        }
        syn::Item::Struct(item_struct) => {
            add_meta_field(&mut item_struct.fields)?;
            Ok(quote! { #item_struct })
        }
        _ => Err(err(&input, "#[add_meta] only supports enums and structs")),
    }
}

fn add_meta_field(fields: &mut Fields) -> Result<(), syn::Error> {
    match fields {
        Fields::Named(fields) => {
            let field: syn::Field =
                parse_quote! { #[doc = "Captured call-site metadata"] meta: ::n0_error::Meta };
            fields.named.push(field);
            Ok(())
        }
        Fields::Unit => {
            let mut named = FieldsNamed {
                brace_token: Default::default(),
                named: Default::default(),
            };
            let field: syn::Field =
                parse_quote! { #[doc = "Captured call-site metadata"] meta: ::n0_error::Meta };
            named.named.push(field);
            *fields = Fields::Named(named);
            Ok(())
        }
        Fields::Unnamed(fields) => {
            // Append meta as last unnamed field for tuple items and mark it explicitly
            let field: syn::Field = parse_quote! { #[doc = "Captured call-site metadata"] #[error(meta)] ::n0_error::Meta };
            fields.unnamed.push(field);
            Ok(())
        }
    }
}

/// Derive macro that implements StackError, Display, Debug, std::error::Error,
/// and generates `From<T>` impls for fields/variants configured via `#[error(..)]`.
///
/// Recognized attributes:
/// - on enums:
///   - `#[error(from_sources)]`: Creates `From` impls for the `source` types of all variants. Will fail to compile if multiple sources have the same type.
///   - `#[error(std_sources)]`: Defaults all sources to be std errors instead of stack errors.
/// - on enum variants and structs:
///   - `#[error("format {field}: {}", a + b)]`: Sets the display formatting. You can refer to named fields by their names, and to tuple fields by `_0`, `_1` etc.
///   - `#[error(transparent)]`: Directly forwards the display implementation to the error source, and omits the outer error in the source chain when reporting errors.
/// - on fields:
///   - `#[error(source)]`: Sets a field as the source of this error. If a field is named `source` this is applied implicitly and not needed.
///   - `#[error(from)]`: Creates a `From` impl for the field's type to the error type.
///   - `#[error(std_err)]`: Marks the error as a `std` error. Without this attribute, errors are expected to implement `StackError`. Only applicable to source fields.
#[proc_macro_derive(StackError, attributes(error))]
pub fn derive_stack_error(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    match derive_error_inner(input) {
        Err(tokens) => tokens.write_errors().into(),
        Ok(tokens) => tokens.into(),
    }
}

fn derive_error_inner(input: DeriveInput) -> Result<proc_macro2::TokenStream, darling::Error> {
    match &input.data {
        syn::Data::Enum(item_enum) => {
            let top = EnumAttrArgs::from_attributes(&input.attrs)?;
            let infos = item_enum
                .variants
                .iter()
                .map(|v| VariantInfo::parse(&v.ident, &v.fields, &v.attrs, &top))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(generate_enum_impls(&input.ident, &input.generics, infos))
        }
        syn::Data::Struct(item) => {
            let top = EnumAttrArgs::default();
            let info = VariantInfo::parse(&input.ident, &item.fields, &input.attrs, &top)?;
            Ok(generate_struct_impl(&input.ident, &input.generics, info))
        }
        _ => Err(err(
            &input,
            "#[derive(StackError)] only supports enums or structs",
        )
        .into()),
    }
}

struct SourceField<'a> {
    kind: SourceKind,
    field: FieldInfo<'a>,
}

impl<'a> SourceField<'a> {
    fn expr_error_ref(&self, ident: &Ident) -> proc_macro2::TokenStream {
        match self.kind {
            SourceKind::Std => quote! { Some(::n0_error::ErrorRef::std(#ident)) },
            SourceKind::Stack => quote! { Some(::n0_error::ErrorRef::stack(#ident)) },
        }
    }

    fn expr_error_std(&self, ident: &Ident) -> proc_macro2::TokenStream {
        match self.kind {
            SourceKind::Std => quote! { Some(#ident as &dyn ::std::error::Error) },
            SourceKind::Stack => quote! { Some(::n0_error::StackError::as_std(#ident)) },
        }
    }
}

enum SourceKind {
    Stack,
    Std,
}

#[derive(Default, Clone, Copy, darling::FromMeta)]
#[darling(default, derive_syn_parse)]
struct StackErrAttrArgs {
    add_meta: bool,
    derive: bool,
    from_sources: bool,
    std_sources: bool,
}

#[derive(Default, Clone, Copy, FromAttributes)]
#[darling(default, attributes(error, stackerr))]
struct EnumAttrArgs {
    from_sources: bool,
    std_sources: bool,
}

#[derive(Default, Clone, Copy, FromAttributes)]
#[darling(default, attributes(error))]
struct FieldAttrArgs {
    source: bool,
    from: bool,
    std_err: bool,
    stack_err: bool,
    meta: bool,
}

struct VariantInfo<'a> {
    ident: Ident,
    fields: Vec<FieldInfo<'a>>,
    kind: Kind,
    display: DisplayArgs,
    source: Option<SourceField<'a>>,
    /// The field that is used for From<..> impls
    from: Option<FieldInfo<'a>>,
    /// The meta field if present.
    meta: Option<FieldInfo<'a>>,
}

#[derive(Clone)]
struct FieldInfo<'a> {
    field: &'a Field,
    args: FieldAttrArgs,
    ident: FieldIdent<'a>,
}

impl<'a> FieldInfo<'a> {
    fn from_named(field: &'a Field) -> Result<Self, syn::Error> {
        Ok(Self {
            args: FieldAttrArgs::from_attributes(&field.attrs)?,
            ident: FieldIdent::Named(field.ident.as_ref().unwrap()),
            field,
        })
    }
    fn from_unnamed(index: usize, field: &'a Field) -> Result<Self, syn::Error> {
        Ok(Self {
            args: FieldAttrArgs::from_attributes(&field.attrs)?,
            ident: FieldIdent::Unnamed(index),
            field,
        })
    }

    fn is_meta(&self) -> bool {
        self.args.meta || matches!(self.ident, FieldIdent::Named(ident) if ident == "meta")
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Kind {
    Named,
    Unit,
    Tuple,
}

#[derive(Clone, Copy)]
enum FieldIdent<'a> {
    Named(&'a Ident),
    Unnamed(usize),
}

impl<'a> FieldIdent<'a> {
    fn named(&self) -> Option<&'a Ident> {
        match self {
            FieldIdent::Named(ident) => Some(ident),
            _ => None,
        }
    }
    fn self_expr(&self) -> proc_macro2::TokenStream {
        match self {
            FieldIdent::Named(ident) => {
                quote!(self.#ident)
            }
            FieldIdent::Unnamed(num) => {
                quote!(self.#num)
            }
        }
    }
}

impl<'a> VariantInfo<'a> {
    fn transparent(&self) -> Option<&FieldInfo<'_>> {
        match self.display {
            DisplayArgs::Transparent => self.source.as_ref().map(|s| &s.field),
            _ => None,
        }
    }

    fn field_binding_idents(&self) -> impl Iterator<Item = Ident> + '_ {
        self.fields.iter().map(|f| match f.ident {
            FieldIdent::Named(ident) => ident.clone(),
            FieldIdent::Unnamed(i) => syn::Ident::new(&format!("_{}", i), Span::call_site()),
        })
    }

    fn has_meta(&self) -> bool {
        self.meta.is_some()
    }

    fn fields_without_meta(&self) -> impl Iterator<Item = &FieldInfo<'a>> {
        self.fields.iter().filter(|f| !f.is_meta())
    }

    fn field_idents_without_meta(&self) -> impl Iterator<Item = Ident> + '_ {
        self.fields_without_meta().map(|f| match f.ident {
            FieldIdent::Named(ident) => ident.clone(),
            FieldIdent::Unnamed(i) => syn::Ident::new(&format!("_{}", i), Span::call_site()),
        })
    }

    fn parse(
        ident: &Ident,
        fields: &'a Fields,
        attrs: &[Attribute],
        top: &EnumAttrArgs,
    ) -> Result<VariantInfo<'a>, syn::Error> {
        let display = DisplayArgs::parse(&attrs)?;
        let (kind, fields): (Kind, Vec<FieldInfo>) = match fields {
            Fields::Named(ref fields) => (
                Kind::Named,
                fields
                    .named
                    .iter()
                    .map(FieldInfo::from_named)
                    .collect::<Result<_, _>>()?,
            ),
            Fields::Unit => (Kind::Unit, Vec::new()),
            Fields::Unnamed(ref fields) => (
                Kind::Tuple,
                fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, f)| FieldInfo::from_unnamed(i, f))
                    .collect::<Result<_, _>>()?,
            ),
        };

        if fields.iter().filter(|f| f.args.source).count() > 1 {
            return Err(err(
                ident,
                "Only one field per variant may have #[error(source)]",
            ));
        }
        let source_field = fields
            .iter()
            .find(|f| f.args.source)
            .or_else(|| match kind {
                Kind::Named => fields.iter().find(|f| match f.ident {
                    FieldIdent::Named(name) => name == "source",
                    _ => false,
                }),
                Kind::Tuple => {
                    if display.is_transparent() {
                        fields.first()
                    } else {
                        None
                    }
                }
                Kind::Unit => None,
            });

        if display.is_transparent() && source_field.is_none() {
            return Err(err(
                ident,
                "Variants with #[error(transparent)] require a source field",
            ));
        }

        // Determine source kind and optional From based on field options and top-level switches
        let source = match source_field.as_ref() {
            None => None,
            Some(field) => {
                let kind = if field.args.std_err || (top.std_sources && !field.args.stack_err) {
                    SourceKind::Std
                } else {
                    SourceKind::Stack
                };
                let field = (*field).clone();
                Some(SourceField { kind, field })
            }
        };

        let from_field = fields
            .iter()
            .find(|f| f.args.from)
            .or_else(|| top.from_sources.then(|| source_field).flatten());
        let from_field = from_field.cloned();
        let meta_field = fields.iter().find(|f| f.is_meta()).cloned();
        Ok(VariantInfo {
            ident: ident.clone(),
            fields,
            kind,
            display,
            source,
            from: from_field,
            meta: meta_field,
        })
    }

    // Pattern helpers
    fn spread_field(&self, field: &FieldInfo<'a>, token: &Ident) -> proc_macro2::TokenStream {
        let v_ident = &self.ident;
        match (self.kind, field.ident) {
            (Kind::Named, FieldIdent::Named(ident)) => {
                quote! { Self::#v_ident { #ident: #token, .. } }
            }
            (Kind::Tuple, FieldIdent::Unnamed(idx)) => {
                let pats = (0..self.fields.len()).map(|i| {
                    if i == idx {
                        quote! { #token }
                    } else {
                        quote! { _ }
                    }
                });
                quote! { Self::#v_ident ( #(#pats),* ) }
            }
            _ => unreachable!("Invalid call to spread_field"),
        }
    }

    fn spread_empty(&self) -> proc_macro2::TokenStream {
        let v_ident = &self.ident;
        match self.kind {
            Kind::Unit => quote! { Self::#v_ident },
            Kind::Named => quote! { Self::#v_ident { .. } },
            Kind::Tuple => {
                let pats = std::iter::repeat(quote! { _ }).take(self.fields.len());
                quote! { Self::#v_ident ( #(#pats),* ) }
            }
        }
    }

    fn spread_all(&self, binds: &[Ident]) -> proc_macro2::TokenStream {
        let v_ident = &self.ident;
        match self.kind {
            Kind::Named => {
                let pairs = self
                    .fields
                    .iter()
                    .flat_map(|f| f.ident.named())
                    .zip(binds.iter())
                    .map(|(ident, bind)| {
                        if *ident == *bind {
                            quote! { #ident }
                        } else {
                            quote! { #ident: #bind }
                        }
                    });
                quote! { #[allow(unused)] Self::#v_ident { #( #pairs ),* } }
            }
            Kind::Tuple => {
                quote! { #[allow(unused)] Self::#v_ident ( #( #binds ),* ) }
            }
            Kind::Unit => quote! { Self::#v_ident },
        }
    }
}

fn generate_enum_impls(
    enum_ident: &Ident,
    generics: &syn::Generics,
    variants: Vec<VariantInfo>,
) -> proc_macro2::TokenStream {
    // StackError impl pieces
    let match_meta_arms = variants.iter().map(|vi| {
        if let Some(meta_field) = &vi.meta {
            let bind = syn::Ident::new("__meta", Span::call_site());
            let pat = vi.spread_field(meta_field, &bind);
            quote! { #pat => Some(&#bind) }
        } else {
            let pat = vi.spread_empty();
            quote! { #pat => None }
        }
    });

    let match_source_arms = variants.iter().map(|vi| match &vi.source {
        Some(src) => {
            let bind = syn::Ident::new("__src", Span::call_site());
            let pat = vi.spread_field(&src.field, &bind);
            let expr = src.expr_error_ref(&bind);
            quote! { #pat => #expr, }
        }
        None => {
            let pat = vi.spread_empty();
            quote! { #pat => None, }
        }
    });

    let match_std_source_arms = variants.iter().map(|vi| match &vi.source {
        Some(src) => {
            let bind = syn::Ident::new("source", Span::call_site());
            let pat = vi.spread_field(&src.field, &bind);
            let expr = src.expr_error_std(&bind);
            quote! { #pat => #expr, }
        }
        None => {
            let pat = vi.spread_empty();
            quote! { #pat => None, }
        }
    });

    let match_transparent_arms = variants.iter().map(|vi| {
        let value = vi.transparent().is_some();
        let pat = vi.spread_empty();
        quote! { #pat => #value }
    });

    let match_fmt_message_arms = variants.iter().map(|vi| match &vi.display {
        DisplayArgs::Format(expr) => {
            let binds: Vec<Ident> = vi.field_binding_idents().collect();
            let pat = vi.spread_all(&binds);
            quote! { #pat => { #expr } }
        }
        DisplayArgs::Default | DisplayArgs::Transparent => {
            let text = format!("{}::{}", enum_ident, vi.ident);
            let pat = vi.spread_empty();
            quote! { #pat => write!(f, #text) }
        }
    });

    let match_debug_arms = variants.iter().map(|vi| {
        let v_name = vi.ident.to_string();
        let binds: Vec<Ident> = vi.field_binding_idents().collect();
        let pat = vi.spread_all(&binds);
        let labels: Vec<String> = vi
            .fields
            .iter()
            .map(|f| match f.ident {
                FieldIdent::Named(id) => id.to_string(),
                FieldIdent::Unnamed(i) => i.to_string(),
            })
            .collect();
        quote! {
            #pat => {
                let mut dbg = f.debug_struct(#v_name);
                #( dbg.field(#labels, &#binds); )*; dbg.finish()?;
            }
        }
    });

    // From impls for variant fields marked with #[from]
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let from_impls = variants.iter().filter_map(|vi| vi.from.as_ref().map(|field| (vi, field))).map(|(vi, field)| {
        let v_ident = &vi.ident;
        let ty = &field.field.ty;
        let construct = match (vi.kind, field.ident) {
            (Kind::Named, FieldIdent::Named(ident)) => {
                let meta = vi.meta.as_ref().map(|_| quote!( meta: ::n0_error::Meta::new() ));
                let comma = (meta.is_some() && vi.fields.len() > 1).then(|| quote!(,));
                quote! { Self::#v_ident { #ident: source #comma #meta } }
            }
            (Kind::Tuple, FieldIdent::Unnamed(src_idx)) => {
                let total = vi.fields.len();
                let mut exprs: Vec<proc_macro2::TokenStream> = Vec::with_capacity(total);
                for i in 0..total {
                    if i == src_idx { exprs.push(quote!{ source }); }
                    else if let Some(meta_field) = &vi.meta {
                        if let FieldIdent::Unnamed(mi) = meta_field.ident { if mi == i { exprs.push(quote!{ ::n0_error::Meta::new() }); continue; } }
                        exprs.push(quote!{ ::core::compile_error!("unsupported From impl: extra fields present") });
                    } else {
                        exprs.push(quote!{ ::core::compile_error!("unsupported From impl: extra fields present") });
                    }
                }
                quote! { Self::#v_ident( #(#exprs),* ) }
            }
            _ => unreachable!(),
        };
        quote! {
            impl #impl_generics ::core::convert::From<#ty> for #enum_ident #ty_generics #where_clause {
                #[track_caller]
                fn from(source: #ty) -> Self { #construct }
            }
        }
    });

    quote! {
        impl #impl_generics ::n0_error::StackError for #enum_ident #ty_generics #where_clause {
            fn as_std(&self) -> &(dyn ::std::error::Error + ::std::marker::Send + ::std::marker::Sync + 'static) {
                self
            }

            fn as_dyn(&self) -> &(dyn ::n0_error::StackError) {
                self
            }

            fn fmt_message(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                match self {
                    #( #match_fmt_message_arms, )*
                }
            }

            fn meta(&self) -> Option<&::n0_error::Meta> {
                match self {
                    #( #match_meta_arms, )*
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

        impl #impl_generics ::core::convert::From<#enum_ident #ty_generics> for ::n0_error::AnyError #where_clause {
            fn from(value: #enum_ident #ty_generics) -> ::n0_error::AnyError {
                ::n0_error::AnyError::from_stack(value)
            }
        }

        impl #impl_generics ::std::fmt::Display for #enum_ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                use ::n0_error::{SourceFormat, StackError};
                let sources = f.alternate().then_some(SourceFormat::OneLine);
                write!(f, "{}", self.report().sources(sources))
            }
        }

        impl #impl_generics ::std::fmt::Debug for #enum_ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                use ::n0_error::StackErrorExt;
                if f.alternate() {
                    match self {
                        #(#match_debug_arms)*
                    }
                } else {
                    use ::n0_error::StackError;
                    write!(f, "{}", self.report().full())?;
                }
                Ok(())
            }
        }

        impl #impl_generics ::std::error::Error for #enum_ident #ty_generics #where_clause {
            fn source(&self) -> Option<&(dyn ::std::error::Error + 'static)> {
                match self {
                    #( #match_std_source_arms )*
                }
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
        let params = info.fields_without_meta().map(|f| {
            let ty = &f.field.ty;
            match f.ident {
                FieldIdent::Named(ident) => quote! { #ident: #ty },
                FieldIdent::Unnamed(i) => {
                    let arg = syn::Ident::new(&format!("_{}", i), Span::call_site());
                    quote! { #arg: #ty }
                }
            }
        });
        let names = info.field_idents_without_meta();
        let meta_named = info
            .meta
            .as_ref()
            .map(|_| quote!(meta: ::n0_error::Meta::new()));
        let meta_tuple = info.meta.as_ref().map(|_| quote!(::n0_error::Meta::new()));
        let comma = (info.has_meta() && info.fields.len() > 1).then(|| quote!(,));
        let doc = format!("Creates a new [`{}`] error.", item_ident);
        let construct = match info.kind {
            Kind::Unit => quote! { Self },
            Kind::Named => quote! { Self { #(#names),* #comma #meta_named } },
            Kind::Tuple => quote! { Self( #(#names),* #comma #meta_tuple ) },
        };
        quote! {
            #[doc = #doc]
            #[track_caller]
            pub fn new(#(#params),*) -> Self { #construct }
        }
    };
    let get_meta = if let Some(meta_field) = &info.meta {
        match meta_field.ident {
            FieldIdent::Named(ident) => quote!(Some(&self.#ident)),
            FieldIdent::Unnamed(i) => {
                let idx = syn::Index::from(i);
                quote!(Some(&self.#idx))
            }
        }
    } else {
        quote! { None }
    };

    let get_error_source = match &info.source {
        Some(src) => {
            let bind = syn::Ident::new("source", Span::call_site());
            let getter = match src.field.ident {
                FieldIdent::Named(id) => quote! { let #bind = &self.#id; },
                FieldIdent::Unnamed(i) => {
                    let idx = syn::Index::from(i);
                    quote! { let #bind = &self.#idx; }
                }
            };
            let expr = src.expr_error_ref(&bind);
            quote! {{ #getter #expr }}
        }
        None => quote! { None },
    };

    let get_std_source = match &info.source {
        Some(src) => {
            let bind = syn::Ident::new("source", Span::call_site());
            let getter = match src.field.ident {
                FieldIdent::Named(id) => quote! { let #bind = &self.#id; },
                FieldIdent::Unnamed(i) => {
                    let idx = syn::Index::from(i);
                    quote! { let #bind = &self.#idx; }
                }
            };
            let expr = src.expr_error_std(&bind);
            quote! {{ #getter #expr }}
        }
        None => quote! { None },
    };

    let is_transparent = info.transparent().is_some();

    let get_display = {
        if let Some(field) = info.transparent() {
            let expr = field.ident.self_expr();
            quote! { write!(f, "{}", #expr) }
        } else {
            match &info.display {
                DisplayArgs::Format(expr) => {
                    let binds: Vec<Ident> = info.field_binding_idents().collect();
                    match info.kind {
                        Kind::Unit => quote! { #expr },
                        Kind::Named => {
                            quote! { #[allow(unused)] let Self { #( #binds ),* , .. } = self; #expr }
                        }
                        Kind::Tuple => {
                            quote! { #[allow(unused)] let Self( #( #binds ),* ) = self; #expr }
                        }
                    }
                }
                DisplayArgs::Default | DisplayArgs::Transparent => {
                    // Fallback to struct name
                    let text = info.ident.to_string();
                    quote! { write!(f, #text) }
                }
            }
        }
    };

    let get_debug = {
        let item_name = info.ident.to_string();
        match info.kind {
            Kind::Unit => {
                quote! {
                    write!(f, #item_name)?;
                }
            }
            Kind::Named => {
                let fields = info
                    .fields
                    .iter()
                    .filter_map(|f| f.ident.named())
                    .map(|ident| {
                        let ident_s = ident.to_string();
                        quote! { dbg.field(#ident_s, &self.#ident) }
                    });
                quote! {
                    let mut dbg = f.debug_struct(#item_name);
                    #(#fields);*;
                    dbg.finish()?;
                }
            }
            Kind::Tuple => {
                let binds = (0..info.fields.len()).map(|i| syn::Index::from(i));
                quote! {
                    let mut dbg = f.debug_tuple(#item_name);
                    #( dbg.field(&self.#binds); )*;
                    dbg.finish()?;
                }
            }
        }
    };

    // From impls for fields marked with #[error(from)] (or inferred via from_sources)
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let get_from = if let Some(field) = &info.from {
        let ty = &field.field.ty;
        let construct = match (info.kind, field.ident) {
            (Kind::Named, FieldIdent::Named(ident)) => {
                let meta = info
                    .meta
                    .as_ref()
                    .map(|_| quote!( meta: ::n0_error::Meta::new() ));
                let comma = (meta.is_some() && info.fields.len() > 1).then(|| quote!(,));
                quote! { Self { #ident: source #comma #meta } }
            }
            (Kind::Tuple, FieldIdent::Unnamed(src_idx)) => {
                let total = info.fields.len();
                let mut exprs: Vec<proc_macro2::TokenStream> = Vec::with_capacity(total);
                for i in 0..total {
                    if i == src_idx {
                        exprs.push(quote! { source });
                    } else if let Some(meta_field) = &info.meta {
                        if let FieldIdent::Unnamed(mi) = meta_field.ident {
                            if mi == i {
                                exprs.push(quote! { ::n0_error::Meta::new() });
                                continue;
                            }
                        }
                        exprs.push(quote!{ ::core::compile_error!("unsupported From impl: extra fields present") });
                    } else {
                        exprs.push(quote!{ ::core::compile_error!("unsupported From impl: extra fields present") });
                    }
                }
                quote! { Self( #(#exprs),* ) }
            }
            _ => unreachable!(),
        };
        Some(quote! {
            impl #impl_generics ::core::convert::From<#ty> for #item_ident #ty_generics #where_clause {
                #[track_caller]
                fn from(source: #ty) -> Self { #construct }
            }
        })
    } else {
        None
    };

    quote! {
        impl #impl_generics #item_ident #ty_generics #where_clause {
            #constructor
        }

        impl #impl_generics ::n0_error::StackError for #item_ident #ty_generics #where_clause {
            fn as_std(&self) -> &(dyn ::std::error::Error + ::std::marker::Send + ::std::marker::Sync + 'static) {
                self
            }

            fn as_dyn(&self) -> &(dyn ::n0_error::StackError) {
                self
            }

            fn meta(&self) -> Option<&::n0_error::Meta> {
                #get_meta
            }
            fn source(&self) -> Option<::n0_error::ErrorRef<'_>> {
                #get_error_source
            }
            fn is_transparent(&self) -> bool {
                #is_transparent
            }

            fn fmt_message(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                #get_display
            }
        }


        impl #impl_generics ::core::convert::From<#item_ident #ty_generics> for ::n0_error::AnyError #where_clause {
            fn from(value: #item_ident #ty_generics) -> ::n0_error::AnyError {
                ::n0_error::AnyError::from_stack(value)
            }
        }

        impl #impl_generics ::std::fmt::Display for #item_ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                use ::n0_error::{SourceFormat, StackError};
                let sources = f.alternate().then_some(SourceFormat::OneLine);
                write!(f, "{}", self.report().sources(sources))
            }
        }

        impl #impl_generics ::std::fmt::Debug for #item_ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                if f.alternate() {
                    #get_debug
                } else {
                    use ::n0_error::StackError;
                    write!(f, "{}", self.report().full())?;
                }
                Ok(())
            }
        }

        impl #impl_generics ::std::error::Error for #item_ident #ty_generics #where_clause {
            fn source(&self) -> Option<&(dyn ::std::error::Error + 'static)> {
                #get_std_source
            }
        }

        #get_from
    }
}

enum DisplayArgs {
    Default,
    Transparent,
    Format(proc_macro2::TokenStream),
}

impl DisplayArgs {
    fn is_transparent(&self) -> bool {
        matches!(self, Self::Transparent)
    }

    fn parse(attrs: &[Attribute]) -> Result<DisplayArgs, syn::Error> {
        // Only consider #[error(...)]
        let Some(attr) = attrs.iter().find(|a| a.path().is_ident("error")) else {
            return Ok(DisplayArgs::Default);
        };

        // syn 2: parse args inside the attribute's parentheses
        let args: Punctuated<Expr, syn::Token![,]> =
            attr.parse_args_with(Punctuated::<Expr, syn::Token![,]>::parse_terminated)?;

        if args.is_empty() {
            return Err(err(
                attr,
                "#[error(..)] requires arguments: a format string or `transparent`",
            ));
        }

        // #[error(transparent)]
        if args.len() == 1 {
            if let Expr::Path(p) = &args[0] {
                if p.path.is_ident("transparent") {
                    return Ok(DisplayArgs::Transparent);
                }
            }
        }

        // #[error("...", args...)]
        let mut it = args.into_iter();
        let first = it.next().unwrap();

        let fmt_lit = match first {
        Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(s), .. }) => s,
        other => {
            return Err(err(
                other,
                "first argument to #[error(\"...\")] must be a string literal, or use #[error(transparent)]",
            ))
        }
    };

        let rest: Vec<Expr> = it.collect();
        Ok(DisplayArgs::Format(
            quote! { write!(f, #fmt_lit #(, #rest)* ) },
        ))
    }
}

fn err(ident: impl ToTokens, err: impl ToString) -> syn::Error {
    syn::Error::new_spanned(ident, err.to_string())
}
