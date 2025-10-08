//! ```toml
//! [dependencies]
//! ignored_derive = { version = "1.0", features = ["PartialEq", "PartialOrd", "Ord", "Debug", "Hash"] }
//! ```
//!
//! This crate provides two derive macros, `PartialEq` and `Hash`, that are
//! intended to be drop-in replacements for the standard library's derives.
//!
//! The key feature is the `#[eqgnore]` attribute. When placed on a field
//! of a struct or enum variant, that field will be completely ignored during
//! equality checks and hashing operations.

use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote};
use std::cmp::Ordering;
use syn::{DeriveInput, parse_macro_input};
use syn::{Error, Field, Ident, Variant, punctuated::Punctuated, token::Comma};
use syn::{Index, Member, parse_quote};

create_derive!(PartialEq);
create_derive!(PartialOrd);
create_derive!(Hash);
create_derive!(Debug);
create_derive!(Ord);

fn generate(
    input: &mut syn::DeriveInput,
    deriving: Deriving,
    errors: &mut Vec<Error>,
) -> TokenStream {
    let where_clause = input.generics.make_where_clause();

    let body = match &input.data {
        syn::Data::Struct(data) => {
            let handle_struct_fields = data
                .fields
                .iter()
                .enumerate()
                .filter(|(_, field)| !deriving.is_ignored_in(field, errors))
                .map(|(i, field)| {
                    let member = field
                        .ident
                        .clone()
                        .map(Member::Named)
                        .unwrap_or_else(|| Member::Unnamed(Index::from(i)));

                    // All fields (that we actually care about) must implement the trait
                    // that we are currently deriving
                    let field_ty = &field.ty;
                    where_clause
                        .predicates
                        .push(parse_quote! { #field_ty: #deriving });

                    deriving.handle_struct_field(member)
                });

            deriving.handle_struct(handle_struct_fields)
        }
        syn::Data::Enum(data) => 'body: {
            if data.variants.is_empty() {
                break 'body quote! { match *self {} };
            }

            let handle_variants = data.variants.iter().map(|variant| {
                let (members, (fields_patterns, handle_variant_fields)): (
                    Vec<_>,
                    (Vec<_>, Vec<_>),
                ) = variant
                    .fields
                    .iter()
                    .enumerate()
                    .filter(|(_, field)| !deriving.is_ignored_in(field, errors))
                    .map(|(i, field)| {
                        let member = field
                            .ident
                            .clone()
                            .map(Member::Named)
                            .unwrap_or_else(|| Member::Unnamed(Index::from(i)));

                        // All fields (that we actually care about) must implement the trait
                        // that we are currently deriving
                        let field_ty = &field.ty;
                        where_clause
                            .predicates
                            .push(parse_quote! { #field_ty: #deriving });

                        (member.clone(), deriving.handle_variant_field(member))
                    })
                    .unzip();

                let handle_variant = deriving.handle_struct(handle_variant_fields.into_iter());
                let variant_name = &variant.ident;

                if matches!(fields_patterns.first(), Some(EnumPatternField::One(_))) {
                    let patterns = fields_patterns.into_iter().map(|x| match x {
                        EnumPatternField::One(ident) => ident,
                        EnumPatternField::Two(..) => unreachable!(
                            "variant depends on variant of `deriving`, but it is constant in this arm"
                        ),
                    });

                    // match self {
                    quote! {
                        Self::#variant_name { #(#members: #patterns,)* } => {
                            #handle_variant
                        }
                    }
                } else {
                    let (lefts, rights): (Vec<_>, Vec<_>) = fields_patterns
                        .into_iter()
                        .map(|x| match x {
                            EnumPatternField::One(..) => unreachable!("variant depends on variant of `deriving`, but it is constant in this arm"),
                            EnumPatternField::Two(left, right) => (left, right),
                        })
                        .unzip();

                    // match (self, other) {
                    quote! {
                        (
                            Self::#variant_name { #(#members: #lefts,)* },
                            Self::#variant_name { #(#members: #rights,)* }
                        ) => {
                            #handle_variant
                        }
                    }
                }
            });

            deriving.handle_enum(handle_variants, &data.variants)
        }
        syn::Data::Union(_) => {
            return Error::new(Span::call_site(), "this trait cannot be derived for unions")
                .into_compile_error();
        }
    };

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let name = &input.ident;
    let signature = deriving.signature();

    quote! {
        #[allow(non_snake_case)]
        #[automatically_derived]
        impl #impl_generics #deriving for #name #ty_generics #where_clause {
            #signature {
                #body
            }
        }
    }
}

/// The standard library trait that we are deriving
///
/// Each trait supports `#[ignore]` attribute on fields. The `#[ignore]` attribute
/// receives a list of identifiers. If the `#[ignore]` attribute's list contains exactly
/// the identifier of this trait, then the trait will skip this field.
#[derive(Copy, Clone)]
enum Deriving {
    /// Deriving [`PartialEq`]
    #[cfg(feature = "PartialEq")]
    PartialEq,
    /// Deriving [`PartialOrd`]
    #[cfg(feature = "PartialOrd")]
    PartialOrd,
    /// Deriving [`Ord`]
    #[cfg(feature = "Ord")]
    Ord,
    /// Deriving [`Debug`]
    #[cfg(feature = "Debug")]
    Debug,
    /// Deriving [`Hash`]
    #[cfg(feature = "Hash")]
    Hash,
}

/// Path to the trait we are deriving
impl ToTokens for Deriving {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let path = match self {
            #[cfg(feature = "PartialEq")]
            Deriving::PartialEq => quote! { ::core::cmp::PartialEq },
            #[cfg(feature = "PartialOrd")]
            Deriving::PartialOrd => quote! { ::core::cmp::PartialOrd },
            #[cfg(feature = "Ord")]
            Deriving::Ord => quote! { ::core::cmp::Ord },
            #[cfg(feature = "Debug")]
            Deriving::Debug => quote! { ::core::fmt::Debug },
            #[cfg(feature = "Hash")]
            Deriving::Hash => quote! { ::core::hash::Hash },
        };

        tokens.extend(path);
    }
}

impl Deriving {
    /// Signature of the single function belonging to the trait we are deriving
    pub fn signature(self) -> TokenStream {
        match self {
            #[cfg(feature = "PartialEq")]
            Deriving::PartialEq => quote! {
                fn eq(&self, other: &Self) -> bool
            },
            #[cfg(feature = "PartialOrd")]
            Deriving::PartialOrd => quote! {
                fn partial_cmp(&self, other: &Self) -> ::core::option::Option<::core::cmp::Ordering>
            },
            #[cfg(feature = "Ord")]
            Deriving::Ord => quote! {
                fn cmp(&self, other: &Self) -> ::core::cmp::Ordering
            },
            #[cfg(feature = "Debug")]
            Deriving::Debug => quote! {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result
            },
            #[cfg(feature = "Hash")]
            Deriving::Hash => quote! {
                fn hash<__H>(&self, state: &mut __H) where __H: ::core::hash::Hasher
            },
        }
    }

    /// Generate logic for a single field
    pub fn handle_struct_field(self, member: Member) -> proc_macro2::TokenStream {
        match self {
            #[cfg(feature = "PartialEq")]
            Deriving::PartialEq => quote! {
                if self.#member != other.#member {
                    return false;
                }
            },
            #[cfg(feature = "PartialOrd")]
            Deriving::PartialOrd => quote! {
                match #self::partial_cmp(&self.#member, &other.#member) {
                    ::core::option::Option::Some(::core::cmp::Ordering::Equal) => {},
                    cmp => return cmp,
                }
            },
            #[cfg(feature = "Ord")]
            Deriving::Ord => quote! {
                match #self::cmp(&self.#member, &other.#member) {
                    ::core::cmp::Ordering::Equal => {},
                    cmp => return cmp,
                }
            },
            #[cfg(feature = "Debug")]
            Deriving::Debug => todo!(),
            #[cfg(feature = "Hash")]
            Deriving::Hash => todo!(),
        }
    }

    /// Generate logic for the enum
    pub fn handle_enum(
        self,
        handle_variants: impl Iterator<Item = TokenStream>,
        variants: &Punctuated<Variant, Comma>,
    ) -> proc_macro2::TokenStream {
        match self {
            Deriving::PartialEq => {
                let arms_eq_discriminants = map_enum_discriminant_permutations(
                    variants,
                    |discriminant_left, variant_left, discriminant_right, variant_right| {
                        let is_equal = discriminant_left == discriminant_right;

                        quote! {
                            (
                                Self::#variant_left { .. },
                                Self::#variant_right { .. }
                            ) => #is_equal
                        }
                    },
                );

                quote! {
                    match (self, other) {
                        #(#arms_eq_discriminants),*
                    } && match (self, other) {
                        #(#handle_variants),*
                        _ => true
                    }
                }
            }
            #[cfg(any(feature = "PartialOrd", feature = "Ord"))]
            Deriving::PartialOrd | Deriving::Ord => {
                let some = match self {
                    #[cfg(feature = "PartialOrd")]
                    Deriving::PartialOrd => Some(quote! { ::core::option::Option::Some }),
                    #[cfg(feature = "Ord")]
                    Deriving::Ord => None,
                    _ => unreachable!(),
                };

                let arms_cmp_discriminants = map_enum_discriminant_permutations(
                    variants,
                    |discriminant_left, variant_left, discriminant_right, variant_right| {
                        let ordering = match discriminant_left.cmp(&discriminant_right) {
                            Ordering::Less => quote!(Less),
                            Ordering::Equal => quote!(Equal),
                            Ordering::Greater => quote!(Greater),
                        };

                        quote! {
                            (
                                Self::#variant_left { .. },
                                Self::#variant_right { .. }
                            ) => { ::core::cmp::Ordering::#ordering }
                        }
                    },
                );

                // panic!(
                //     "{}",
                //     arms_cmp_discriminants
                //         .flatten()
                //         .collect::<TokenStream>()
                //         .to_string()
                // );

                quote! {
                    match (self, other) {
                        #(#handle_variants),*
                        _ => {
                            // if variants are not equal, then we just compare their discriminants
                            #some(match (self, other) {
                                #(#arms_cmp_discriminants)*
                            })
                        }
                    }
                }
            }
            Deriving::Debug => todo!(),
            Deriving::Hash => todo!(),
        }
    }

    /// Generates logic for handling a single field of a variant
    pub fn handle_variant_field(
        self,
        member: Member,
    ) -> (EnumPatternField, proc_macro2::TokenStream) {
        let member_str = member.to_token_stream().to_string();
        match self {
            #[cfg(feature = "PartialEq")]
            Deriving::PartialEq => {
                let left = format_ident!("__l_{}", member_str);
                let right = format_ident!("__r_{}", member_str);

                (
                    EnumPatternField::Two(left.clone(), right.clone()),
                    quote! {
                        if #left != #right {
                            return false;
                        }
                    },
                )
            }
            #[cfg(any(feature = "PartialOrd", feature = "Ord"))]
            Deriving::PartialOrd | Deriving::Ord => {
                let equal = match self {
                    #[cfg(feature = "PartialOrd")]
                    Deriving::PartialOrd => {
                        quote! { ::core::option::Option::Some(::core::cmp::Ordering::Equal) }
                    }
                    #[cfg(feature = "Ord")]
                    Deriving::Ord => quote! { ::core::cmp::Ordering::Equal },
                    _ => unreachable!(),
                };

                let left = format_ident!("__l_{member_str}");
                let right = format_ident!("__r_{member_str}");

                (
                    EnumPatternField::Two(left.clone(), right.clone()),
                    quote! {
                        match #self::partial_cmp(&#left, &#right) {
                            #equal => {},
                            cmp => return cmp,
                        }
                    },
                )
            }
            #[cfg(feature = "Debug")]
            Deriving::Debug => todo!(),
            #[cfg(feature = "Hash")]
            Deriving::Hash => todo!(),
        }
    }

    pub fn handle_struct(self, fields: impl Iterator<Item = TokenStream>) -> TokenStream {
        match self {
            #[cfg(feature = "PartialEq")]
            Deriving::PartialEq => quote! {
                #(#fields)*
                true
            },
            #[cfg(feature = "PartialOrd")]
            Deriving::PartialOrd => quote! {
                #(#fields)*
                ::core::option::Option::Some(::core::cmp::Ordering::Equal)
            },
            #[cfg(feature = "Ord")]
            Deriving::Ord => quote! {
                #(#fields)*
                ::core::cmp::Ordering::Equal
            },
            #[cfg(feature = "Debug")]
            Deriving::Debug => todo!(),
            #[cfg(feature = "Hash")]
            Deriving::Hash => todo!(),
        }
    }

    /// If this derive ignores `field`
    pub fn is_ignored_in(self, field: &Field, errors: &mut Vec<Error>) -> bool {
        #[cfg(feature = "PartialEq")]
        let mut partial_eq = false;
        #[cfg(feature = "PartialOrd")]
        let mut partial_ord = false;
        #[cfg(feature = "Ord")]
        let mut ord = false;
        #[cfg(feature = "Debug")]
        let mut debug = false;
        #[cfg(feature = "Hash")]
        let mut hash = false;

        for attr in &field.attrs {
            if attr.path().is_ident("ignored") {
                let result = attr.parse_nested_meta(|meta| {
                    let ident = meta.path.require_ident()?;

                    let value = match ident.to_string().as_str() {
                        #[cfg(feature = "PartialEq")]
                        "PartialEq" => &mut partial_eq,
                        #[cfg(feature = "PartialOrd")]
                        "PartialOrd" => &mut partial_ord,
                        #[cfg(feature = "Ord")]
                        "Ord" => &mut ord,
                        #[cfg(feature = "Debug")]
                        "Debug" => &mut debug,
                        #[cfg(feature = "Hash")]
                        "Hash" => &mut hash,
                        // necessary due to the way our `cfg` works
                        #[allow(clippy::vec_init_then_push)]
                        _ => {
                            let mut expected = Vec::new();
                            #[cfg(feature = "PartialEq")]
                            expected.push("`PartialEq`");
                            #[cfg(feature = "PartialOrd")]
                            expected.push("`PartialOrd`");
                            #[cfg(feature = "Ord")]
                            expected.push("`Ord`");
                            #[cfg(feature = "Debug")]
                            expected.push("`Debug`");
                            #[cfg(feature = "Hash")]
                            expected.push("`Hash`");

                            return Err(Error::new(
                                ident.span(),
                                format!("expected one of: {}", expected.join(", ")),
                            ));
                        }
                    };

                    if *value {
                        errors.push(Error::new(
                            ident.span(),
                            "this derive has already been ignored",
                        ));
                    }

                    *value = true;

                    Ok(())
                });

                if let Err(error) = result {
                    errors.push(error);
                }
            }
        }

        match self {
            #[cfg(feature = "PartialEq")]
            Deriving::PartialEq => partial_eq,
            #[cfg(feature = "PartialOrd")]
            Deriving::PartialOrd => partial_ord,
            #[cfg(feature = "Ord")]
            Deriving::Ord => ord,
            #[cfg(feature = "Debug")]
            Deriving::Debug => debug,
            #[cfg(feature = "Hash")]
            Deriving::Hash => hash,
        }
    }
}

/// Maps permutations of the enum `variants` with `f` into a `TokenStream`
///
/// Because Rust provides us no way of getting the discriminant value
/// of an arbitrary enum, iterate through `n^2` permutations instead
///
/// For example, when deciding if enum `A`'s discriminant is greater than
/// enum `B`'s discriminant (and both enums must be the same type), we
/// `match` against every variant in `A`, and in each arm we again `match`
/// against each variant in `B`. All sub-arms evaluate to `PartialOrd`.
///
/// This should optimize away at compile-time, but it could be done
/// more nicely if we had a stable `core::intrinsics::discriminant_value`, for example
/// the RFC <https://github.com/rust-lang/rfcs/pull/3607> would suffice.
///
/// # Correctness
///
/// This function is not always correct. For example, in this enum:
///
/// ```rust
/// enum Foo {
///     Foo = 3,
///     Bar = 0,
///     Baz
/// }
/// ```
///
/// `Foo::Foo`'s discriminant is greater than `Foo::Bar` or `Foo::Baz`,
/// but our macro assigns `Foo::Foo` the smallest discriminant.
///
/// **There's no way to fix this**. Because value of the discriminant is
/// only available after our macro generates code.
///
/// But we need to get value of the discriminant **inside** our proc macro
/// in order to know what code to emit.
///
/// For the purposes of our macros, we don't care about **what** the actual discriminant
/// value is - we only care that it's equal to, greater than or less than some
/// other discriminant value of the same enum - which means it's sufficient
/// to start discriminant at 0 for the first variant, and increment it for
/// each variant.
fn map_enum_discriminant_permutations(
    variants: &Punctuated<Variant, Comma>,
    f: fn(usize, &Ident, usize, &Ident) -> TokenStream,
) -> impl Iterator<Item = TokenStream> {
    variants
        .iter()
        .enumerate()
        .flat_map(move |(discriminant_left, variant_left)| {
            variants
                .iter()
                .enumerate()
                .map(move |(discriminant_right, variant_right)| {
                    f(
                        discriminant_left,
                        &variant_left.ident,
                        discriminant_right,
                        &variant_right.ident,
                    )
                })
        })
}

/// We need to construct a pattern from a bunch of pieces, but this pattern
/// can `match` either 1 enum or 2 enums.
enum EnumPatternField {
    /// For implementation of [`Debug`] and [`Hash`]
    ///
    /// ```ignore
    /// match self {
    ///     Self::One { 0: __0 } => { /* ... */ },
    ///     // ...
    /// }
    /// ```
    ///
    /// `One` would hold `Ident(__0)`
    One(Ident),
    /// For implementation of [`PartialOrd`], [`Ord`] and [`PartialEq`]
    ///
    /// ```ignore
    /// match (self, other) {
    ///     (Self::One { 0: __l_0 }, Self::One { 0: __r_0 }) => { /* ... */ },
    ///     // ...
    /// }
    /// ```
    ///
    /// `Two` would hold `(Ident(__l_0), Ident(__r_0))`
    Two(Ident, Ident),
}

impl ToTokens for EnumPatternField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let new = match self {
            EnumPatternField::One(ident) => quote! { #ident },
            EnumPatternField::Two(left, right) => quote! { (#left, #right) },
        };

        tokens.extend(new);
    }
}

macro_rules! create_derive {
    { $Derive:ident $($tt:tt)* } => {
        #[doc = concat!("Derives an `", stringify!($Derive), "` implementation.")]
        ///
        /// The only difference from the standard library's derive is that
        #[doc = concat!("fields marked with `#[ignored(", stringify!($Derive), ")]`")]
        /// will be ignored when implementing this trait
        ///
        /// See the [crate-level](crate) documentation for more info
        #[proc_macro_derive($Derive, attributes(ignored))]
        #[allow(non_snake_case)]
        pub fn $Derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
            let mut input = parse_macro_input!(input as DeriveInput);

            let mut errors = Vec::new();
            let output = generate(&mut input, Deriving::$Derive, &mut errors);
            let errors = errors.into_iter().map(|error| error.into_compile_error());

            quote! {
                #output
                #(#errors)*
            }
            .into()
        }
    };
}
use create_derive;
