use darling::FromAttributes;
use heck::ToSnakeCase;
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, Attribute, DeriveInput, Expr, Field,
    Fields, FieldsNamed, Ident,
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
    match add_location_inner(parse_macro_input!(item as syn::Item)) {
        Err(err) => err.to_compile_error().into(),
        Ok(tokens) => tokens.into(),
    }
}

fn add_location_inner(mut input: syn::Item) -> Result<proc_macro2::TokenStream, syn::Error> {
    match &mut input {
        syn::Item::Enum(item_enum) => {
            for variant in item_enum.variants.iter_mut() {
                add_location_field(&mut variant.fields)?;
            }
            Ok(quote! { #item_enum })
        }
        syn::Item::Struct(item_struct) => {
            add_location_field(&mut item_struct.fields)?;
            Ok(quote! { #item_struct })
        }
        _ => Err(err(
            &input,
            "#[add_location] only supports enums and structs",
        )),
    }
}

fn add_location_field(fields: &mut Fields) -> Result<(), syn::Error> {
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
        Fields::Unnamed(_) => Err(err(
            &fields,
            "#[add_location] does not support tuple variants or structs",
        )),
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
    match derive_error_inner(input) {
        Err(tokens) => tokens.write_errors().into(),
        Ok(tokens) => tokens.into(),
    }
}

fn derive_error_inner(input: DeriveInput) -> Result<proc_macro2::TokenStream, darling::Error> {
    let top = TopAttrs::from_attributes(&input.attrs)?;
    match &input.data {
        syn::Data::Enum(item_enum) => {
            let infos = item_enum
                .variants
                .iter()
                .map(|v| VariantInfo::parse(&v.ident, &v.fields, &v.attrs, &top))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(generate_enum_impls(&input.ident, &input.generics, infos))
        }
        syn::Data::Struct(item) => {
            let info = VariantInfo::parse(&input.ident, &item.fields, &input.attrs, &top)?;
            Ok(generate_struct_impl(&input.ident, &input.generics, info))
        }
        _ => Err(err(&input, "#[derive(Error)] only supports enums or structs").into()),
    }
}

struct SourceField {
    ident: Ident,
    kind: SourceKind,
    transparent: bool,
}

impl SourceField {
    fn expr_error_ref(&self) -> proc_macro2::TokenStream {
        let ident = &self.ident;
        match self.kind {
            SourceKind::Std => quote! { Some(::n0_error::ErrorRef::std(#ident)) },
            SourceKind::Stack => quote! { Some(::n0_error::ErrorRef::stack(#ident)) },
        }
    }

    fn expr_error_std(&self) -> proc_macro2::TokenStream {
        let ident = &self.ident;
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

#[derive(Default, Clone, Copy, FromAttributes)]
#[darling(default, attributes(error))]
struct TopAttrs {
    from_sources: bool,
    std_sources: bool,
}

#[derive(Default, Clone, Copy, FromAttributes)]
#[darling(default, attributes(error))]
struct FieldAttrs {
    source: bool,
    from: bool,
    std_err: bool,
    stack_err: bool,
}

#[derive(Default, Clone, Copy, FromAttributes)]
#[darling(default, attributes(error))]
struct VariantAttrs {
    transparent: bool,
}

// For each variant, capture doc comment text or #[display] attr
struct VariantInfo<'a> {
    ident: Ident,
    fields: Vec<FieldInfo<'a>>,
    display: Option<proc_macro2::TokenStream>,
    source: Option<SourceField>,
    from_field: Option<&'a Field>,
}

struct FieldInfo<'a> {
    field: &'a Field,
    opts: FieldAttrs,
}

impl<'a> FieldInfo<'a> {
    fn from_field(field: &'a Field) -> Result<Self, syn::Error> {
        let opts = FieldAttrs::from_attributes(&field.attrs)?;
        Ok(Self { field, opts })
    }
}

impl<'a> VariantInfo<'a> {
    fn transparent(&self) -> Option<&Ident> {
        match &self.source {
            Some(field) if field.transparent => Some(&field.ident),
            _ => None,
        }
    }
    fn fields(&self) -> &Vec<FieldInfo<'_>> {
        &self.fields
    }

    fn field_idents(&self) -> impl Iterator<Item = &Ident> {
        self.fields()
            .iter()
            .map(|f| f.field.ident.as_ref().unwrap())
    }

    fn location(&self) -> Option<&Field> {
        self.fields()
            .iter()
            .map(|f| &f.field)
            .copied()
            .find(|f| f.ident.as_ref().unwrap() == "location")
    }

    fn fields_without_location(&self) -> impl Iterator<Item = &Field> {
        self.fields()
            .iter()
            .map(|f| &f.field)
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
        top: &TopAttrs,
    ) -> Result<VariantInfo<'a>, syn::Error> {
        let fields: Result<Vec<FieldInfo>, _> = match fields {
            Fields::Named(ref fields) => fields.named.iter().map(FieldInfo::from_field).collect(),
            Fields::Unit => Ok(vec![]),
            Fields::Unnamed(ref fields) => {
                fields.unnamed.iter().map(FieldInfo::from_field).collect()
            }
        };
        let fields = fields?;

        if fields.iter().filter(|f| f.opts.source).count() > 1 {
            return Err(err(
                ident,
                "Only one field per variant may have #[error(source)]",
            ));
        }
        let source_field = fields.iter().find(|f| f.opts.source).or_else(|| {
            fields.iter().find(|f| {
                f.field
                    .ident
                    .as_ref()
                    .map(|i| i == "source")
                    .unwrap_or(false)
            })
        });

        let variant_attrs = VariantAttrs::from_attributes(attrs)?;
        if variant_attrs.transparent && source_field.is_none() {
            return Err(err(
                ident,
                "Variants with #[error(transparent)] require a source field",
            ));
        }

        // Determine source kind and optional From based on field options and top-level switches
        let source = match source_field.as_ref() {
            None => None,
            Some(field) => {
                let ident = field
                    .field
                    .ident
                    .clone()
                    .ok_or_else(|| err(&field.field, "source fields must be named"))?;
                let kind = if field.opts.std_err || (top.std_sources && !field.opts.stack_err) {
                    SourceKind::Std
                } else {
                    SourceKind::Stack
                };
                Some(SourceField {
                    ident,
                    kind,
                    transparent: variant_attrs.transparent,
                })
            }
        };

        let from = fields
            .iter()
            .find(|f| f.opts.from)
            .or_else(|| top.from_sources.then(|| source_field).flatten());
        let from = from.map(|f| f.field);

        Ok(VariantInfo {
            ident: ident.clone(),
            fields,
            display: get_doc_or_display(&attrs)?,
            source,
            from_field: from,
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
        match &vi.source {
            Some(field) => {
                let ident = &field.ident;
                let expr = field.expr_error_ref();
                quote! { Self::#v_ident { #ident, .. } => #expr, }
            }
            None => quote! { Self::#v_ident { .. } => None, },
        }
    });

    let match_std_source_arms = variants.iter().map(|vi| {
        let v_ident = &vi.ident;
        match &vi.source {
            Some(field) => {
                let ident = &field.ident;
                let expr = field.expr_error_std();
                quote! { Self::#v_ident { #ident, .. } => #expr, }
            }
            None => quote! { Self::#v_ident { .. } => None, },
        }
    });

    let match_transparent_arms = variants.iter().map(|vi| {
        let v_ident = &vi.ident;
        let value = vi.transparent().is_some();
        quote! { Self::#v_ident { .. } => #value }
    });

    let match_fmt_message_arms = variants.iter().map(|vi| {
        let v_ident = &vi.ident;
        if let Some(ident) = vi.transparent() {
            quote! { Self::#v_ident { #ident, .. } => { write!(f, "{}", #ident) } }
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
    let from_impls = variants.iter().filter_map(|vi| vi.from_field.map(|field| (vi, field))).map(|(vi, field)| {
        let v_ident = &vi.ident;
        let Field { ty, ident, .. } = &field;
        let location = vi
            .location()
            .map(|_| quote!{ location: ::n0_error::location() });
        let comma = (location.is_some() && vi.fields().len() > 1).then(|| quote!(,));
        quote! {
            impl #impl_generics ::core::convert::From<#ty> for #enum_ident #ty_generics #where_clause {
                #[track_caller]
                fn from(source: #ty) -> Self {
                    Self::#v_ident { #ident: source #comma #location }
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

    let get_error_source = match &info.source {
        Some(field) => field.expr_error_ref(),
        None => quote! { None },
    };

    let get_std_source = match &info.source {
        None => quote! { None },
        Some(field) => field.expr_error_std(),
    };

    let is_transparent = info.transparent().is_some();

    let get_display = {
        if let Some(ident) = info.transparent() {
            quote! { write!(f, "{}", self.#ident) }
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
    let get_from = if let Some(field) = info.from_field {
        let Field { ty, ident, .. } = &field;
        let location = info
            .location()
            .map(|_| quote!(location: ::n0_error::location()));
        let comma = (location.is_some() && info.fields().len() > 1).then(|| quote!(,));
        Some(quote! {
            impl #impl_generics ::core::convert::From<#ty> for #item_ident #ty_generics #where_clause {
                #[track_caller]
                fn from(source: #ty) -> Self {
                    Self { #ident: source #comma #location }
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
                #is_transparent
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

        #get_from
    }
}

fn get_doc_or_display(attrs: &[Attribute]) -> Result<Option<proc_macro2::TokenStream>, syn::Error> {
    // Prefer #[display("...")]
    if let Some(attr) = attrs.iter().find(|a| a.path().is_ident("display")) {
        // Accept format!-style args: #[display("text {}", arg1, arg2, ...)]
        let args = attr.parse_args_with(Punctuated::<Expr, syn::Token![,]>::parse_terminated)?;
        if args.is_empty() {
            Err(err(
                attr,
                "#[display(..)] requires at least a format string",
            ))
        } else {
            let mut it = args.into_iter();
            let fmt = it.next().unwrap();
            let rest: Vec<_> = it.collect();
            Ok(Some(quote! { write!(f, #fmt #(, #rest)* ) }))
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
            Ok(None)
        } else {
            let doc = docs.join("\n");
            Ok(Some(quote! { write!(f, #doc) }))
        }
    }
}

fn err(ident: impl ToTokens, err: impl ToString) -> syn::Error {
    syn::Error::new_spanned(ident, err.to_string())
}
