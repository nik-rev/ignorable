//! [![crates.io](https://img.shields.io/crates/v/ignorable?style=flat-square&logo=rust)](https://crates.io/crates/ignorable)
//! [![docs.rs](https://img.shields.io/badge/docs.rs-ignorable-blue?style=flat-square&logo=docs.rs)](https://docs.rs/ignorable)
//! ![license](https://img.shields.io/badge/license-Apache--2.0_OR_MIT-blue?style=flat-square)
//! ![msrv](https://img.shields.io/badge/msrv-1.56-blue?style=flat-square&logo=rust)
//! [![github](https://img.shields.io/github/stars/nik-rev/ignorable)](https://github.com/nik-rev/ignorable)
//!
//! This crate provides 5 derives that are just like the standard library's, but they allow
//! to ignore fields when deriving. Inspired by [RFC 3869](https://github.com/rust-lang/rfcs/pull/3869)
//!
//! ```toml
//! [dependencies]
//! ignorable = "0.1"
//! ```
//!
//! # Usage
//!
//! This crate provides 5 derive macros:
//!
//! - `PartialEq`
//! - `PartialOrd`
//! - `Ord`
//! - `Debug`
//! - `Hash`
//!
//! The advantage of these derives over the standard library's is that they support
//! the `#[ignored]` attribute to ignore individual fields when deriving the respective traits.
//!
//! ```rust
//! use ignorable::{PartialEq, Hash};
//!
//! // `PartialEq` and `Hash` impls will only check
//! // the `id` field of 2 `User`s
//! #[derive(Clone, PartialEq, Eq, Hash)]
//! struct User {
//!     #[ignored(PartialEq, Hash)]
//!     name: String,
//!     #[ignored(PartialEq, Hash)]
//!     age: u8,
//!     id: u64
//! }
//! ```
//!
//! Advantages:
//!
//! - **Significantly** less boilerplate
//! - Less maintenance overhead, it's not your responsibility to remember to update manual implementations of traits,
//!   keep traits like `Hash` and `PartialEq` in sync. We've got that covered!
//! - This might become a language feature in the future ([RFC 3869](https://github.com/rust-lang/rfcs/pull/3869)),
//!   so you'll be able to transition away from this crate once that time comes!
//!
//! Remember that it is a [logic error](https://doc.rust-lang.org/stable/std/hash/trait.Hash.html#hash-and-eq)
//! for the implementations of `Hash` and `PartialEq` to differ, and if you need to manually implement the traits
//! to skip certain fields, **you** must remember to keep them in sync because you can't use the `derive` anymore.
//!
//! # With `ignorable`
//!
//! Uses derives provided by this crate.
//!
//! ```rust
//! # use std::marker::PhantomData;
//! # use std::cell::RefCell;
//! # use std::rc::Rc;
//! #
//! # #[derive(PartialEq, Debug, Hash, Clone)]
//! # struct Symbol;
//! # #[derive(PartialEq, Debug, Hash, Clone)]
//! # struct Value;
//! #
//! # mod protocols {
//! #     #[derive(PartialEq, Debug, Hash, Clone)]
//! #     pub struct IPersistentMap;
//! # };
//! use ignorable::{Debug, PartialEq, Hash};
//!
//! #[derive(Clone, Debug, PartialEq, Hash)]
//! pub struct Var<T> {
//!     pub ns: Symbol,
//!     pub sym: Symbol,
//!     #[ignored(PartialEq, Hash)]
//!     meta: RefCell<protocols::IPersistentMap>,
//!     #[ignored(PartialEq, Hash)]
//!     pub root: RefCell<Rc<Value>>,
//!     #[ignored(Debug)]
//!     _phantom: PhantomData<T>
//! }
//! ```
//!
//! # Without
//!
//! You must manually implement each trait.
//!
//! ```rust
//! # use std::marker::PhantomData;
//! # use std::hash::{Hasher, Hash};
//! # use std::fmt;
//! # use std::cell::RefCell;
//! # use std::rc::Rc;
//! #
//! # #[derive(PartialEq, Debug, Hash, Clone)]
//! # struct Symbol;
//! # #[derive(PartialEq, Debug, Hash, Clone)]
//! # struct Value;
//! #
//! # mod protocols {
//! #     #[derive(PartialEq, Debug, Hash, Clone)]
//! #     pub struct IPersistentMap;
//! # };
//! #[derive(Clone)]
//! pub struct Var<T> {
//!     pub ns: Symbol,
//!     pub sym: Symbol,
//!     meta: RefCell<protocols::IPersistentMap>,
//!     pub root: RefCell<Rc<Value>>,
//!     _phantom: PhantomData<T>
//! }
//!
//! impl<T> fmt::Debug for Var<T> {
//!     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//!         f.debug_struct("Var")
//!             .field("ns", &self.ns)
//!             .field("sym", &self.sym)
//!             .field("meta", &self.meta)
//!             .field("root", &self.root)
//!             .finish()
//!     }
//! }
//!
//! impl<T> PartialEq for Var<T> {
//!     fn eq(&self, other: &Self) -> bool {
//!         self.ns == other.ns && self.sym == other.sym
//!     }
//! }
//!
//! impl<T> Hash for Var<T> {
//!     fn hash<H: Hasher>(&self, state: &mut H) {
//!         (&self.ns, &self.sym).hash(state);
//!     }
//! }
//! ```
//!
//! Notes:
//!
//! - It is logically incorrect for `Hash` and `PartialEq` implementations
//!   to differ, so you must remember to keep them in sync if `Var` changes
//! - You must remember to update the string names of the `Debug` impl if you
//!   ever rename the fields or `Var` itself
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::cmp::Ordering;
use syn::{parse_macro_input, DeriveInput};
use syn::{parse_quote, Index, Member};
use syn::{punctuated::Punctuated, token::Comma, Error, Field, Ident, Variant};

create_derive!(PartialEq);
create_derive!(PartialOrd);
create_derive!(Ord);
create_derive!(Debug);
create_derive!(Hash);

fn generate(
    input: &mut syn::DeriveInput,
    deriving: Deriving,
    errors: &mut Vec<Error>,
) -> TokenStream {
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

                    deriving.handle_struct_field(member)
                });

            deriving.handle_struct(
                handle_struct_fields,
                &input.ident,
                matches!(data.fields, syn::Fields::Unnamed(_)),
            )
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

                        (member.clone(), deriving.handle_variant_field(member))
                    })
                    .unzip();

                let handle_variant = deriving.handle_struct(handle_variant_fields.into_iter(), &variant.ident, matches!(variant.fields, syn::Fields::Unnamed(_)));
                let variant_name = &variant.ident;

                if matches!(deriving, Deriving::Debug | Deriving::Hash) {
                    let patterns = fields_patterns.into_iter().map(|x| match x {
                        EnumPatternField::One(ident) => ident,
                        EnumPatternField::Two(..) => unreachable!(
                            "variant depends on variant of `deriving`, but it is constant in this arm"
                        ),
                    });

                    // match self {
                    quote! {
                        Self::#variant_name { #(#members: #patterns,)* .. } => {
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
                            Self::#variant_name { #(#members: #lefts,)* .. },
                            Self::#variant_name { #(#members: #rights,)* .. }
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

    for type_param in input.generics.type_params_mut() {
        type_param.bounds.push(parse_quote!( #deriving ));
    }
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let name = &input.ident;
    let signature = deriving.signature();

    quote! {
        #[allow(non_snake_case, non_shorthand_field_patterns)]
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
    PartialEq,
    /// Deriving [`PartialOrd`]
    PartialOrd,
    /// Deriving [`Ord`]
    Ord,
    /// Deriving [`Debug`]
    Debug,
    /// Deriving [`Hash`]
    Hash,
}

/// Path to the trait we are deriving
impl ToTokens for Deriving {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let path = match self {
            Deriving::PartialEq => quote! { ::core::cmp::PartialEq },
            Deriving::PartialOrd => quote! { ::core::cmp::PartialOrd },
            Deriving::Ord => quote! { ::core::cmp::Ord },
            Deriving::Debug => quote! { ::core::fmt::Debug },
            Deriving::Hash => quote! { ::core::hash::Hash },
        };

        tokens.extend(path);
    }
}

impl Deriving {
    /// Signature of the single function belonging to the trait we are deriving
    pub fn signature(self) -> TokenStream {
        match self {
            Deriving::PartialEq => quote! {
                fn eq(&self, other: &Self) -> bool
            },
            Deriving::PartialOrd => quote! {
                fn partial_cmp(&self, other: &Self) -> ::core::option::Option<::core::cmp::Ordering>
            },
            Deriving::Ord => quote! {
                fn cmp(&self, other: &Self) -> ::core::cmp::Ordering
            },
            Deriving::Debug => quote! {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result
            },
            Deriving::Hash => quote! {
                fn hash<__H>(&self, state: &mut __H) where __H: ::core::hash::Hasher
            },
        }
    }

    /// Generate logic for a single field
    pub fn handle_struct_field(self, member: Member) -> proc_macro2::TokenStream {
        match self {
            Deriving::PartialEq => quote! {
                if self.#member != other.#member {
                    return false;
                }
            },
            Deriving::PartialOrd => quote! {
                match #self::partial_cmp(&self.#member, &other.#member) {
                    ::core::option::Option::Some(::core::cmp::Ordering::Equal) => {},
                    cmp => return cmp,
                }
            },
            Deriving::Ord => quote! {
                match #self::cmp(&self.#member, &other.#member) {
                    ::core::cmp::Ordering::Equal => {},
                    cmp => return cmp,
                }
            },
            Deriving::Debug => match &member {
                Member::Named(ident) => {
                    let name = ident.to_string();
                    quote! { .field(#name, &self.#member) }
                }
                Member::Unnamed(_) => {
                    quote! { .field(&self.#member) }
                }
            },
            Deriving::Hash => quote! {
                #self::hash(&self.#member, state);
            },
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
                    let discriminant = match (self, other) {
                        #(#arms_eq_discriminants),*
                    };
                    discriminant && match (self, other) {
                        #(#handle_variants),*
                        _ => true
                    }
                }
            }
            Deriving::PartialOrd | Deriving::Ord => {
                let some = match self {
                    Deriving::PartialOrd => Some(quote! { ::core::option::Option::Some }),
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
            Deriving::Debug => quote! {
                match &self {
                    #(#handle_variants,)*
                }
            },
            Deriving::Hash => {
                let arms = variants.iter().enumerate().map(|(discriminant, variant)| {
                    let ident = &variant.ident;
                    quote! { Self::#ident { .. } => #self::hash(&#discriminant, state) }
                });
                quote! {
                    match self {
                        #(#arms,)*
                    }
                    match &self {
                        #(#handle_variants)*
                    }
                }
            }
        }
    }

    /// Generates logic for handling a single field of a variant
    pub fn handle_variant_field(
        self,
        member: Member,
    ) -> (EnumPatternField, proc_macro2::TokenStream) {
        let member_str = member.to_token_stream().to_string();
        match self {
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
            Deriving::PartialOrd | Deriving::Ord => {
                let (method, equal) = match self {
                    Deriving::PartialOrd => (
                        quote! { partial_cmp },
                        quote! { ::core::option::Option::Some(::core::cmp::Ordering::Equal) },
                    ),
                    Deriving::Ord => (quote! { cmp }, quote! { ::core::cmp::Ordering::Equal }),
                    _ => unreachable!(),
                };

                let left = format_ident!("__l_{member_str}");
                let right = format_ident!("__r_{member_str}");

                (
                    EnumPatternField::Two(left.clone(), right.clone()),
                    quote! {
                        match #self::#method(&#left, &#right) {
                            #equal => {},
                            cmp => return cmp,
                        }
                    },
                )
            }
            Deriving::Debug => {
                let ident = format_ident!("__{member_str}");
                match &member {
                    Member::Named(ident) => {
                        let name = ident.to_string();
                        (
                            EnumPatternField::One(ident.clone()),
                            quote! { .field(#name, #ident) },
                        )
                    }
                    Member::Unnamed(_) => (
                        EnumPatternField::One(ident.clone()),
                        quote! { .field(#ident) },
                    ),
                }
            }
            Deriving::Hash => {
                let ident = format_ident!("__{member_str}");
                (
                    EnumPatternField::One(ident.clone()),
                    quote! {
                        #self::hash(&#ident, state);
                    },
                )
            }
        }
    }

    pub fn handle_struct(
        self,
        fields: impl Iterator<Item = TokenStream>,
        name: &Ident,
        is_tuple: bool,
    ) -> TokenStream {
        match self {
            Deriving::PartialEq => quote! {
                #(#fields)*
                true
            },
            Deriving::PartialOrd => quote! {
                #(#fields)*
                ::core::option::Option::Some(::core::cmp::Ordering::Equal)
            },
            Deriving::Ord => quote! {
                #(#fields)*
                ::core::cmp::Ordering::Equal
            },
            Deriving::Debug => {
                let name = name.to_string();
                match is_tuple {
                    true => quote! {
                        f.debug_tuple(#name)
                            #(#fields)*
                            .finish()
                    },
                    false => quote! {
                        f.debug_struct(#name)
                            #(#fields)*
                            .finish()
                    },
                }
            }
            Deriving::Hash => quote! {
                #(#fields)*
            },
        }
    }

    /// If this derive ignores `field`
    pub fn is_ignored_in(self, field: &Field, errors: &mut Vec<Error>) -> bool {
        let mut partial_eq = false;
        let mut partial_ord = false;
        let mut ord = false;
        let mut debug = false;
        let mut hash = false;

        for attr in &field.attrs {
            if attr.path().is_ident("ignored") {
                let result = attr.parse_nested_meta(|meta| {
                    let ident = meta.path.require_ident()?;

                    let value = match ident.to_string().as_str() {
                        "PartialEq" => &mut partial_eq,
                        "PartialOrd" => &mut partial_ord,
                        "Ord" => &mut ord,
                        "Debug" => &mut debug,
                        "Hash" => &mut hash,
                        _ => {
                            return Err(Error::new(
                                ident.span(),
                                concat!(
                                    "expected one of: `PartialEq`, `PartialOrd`, ",
                                    "`Ord`, `Debug`, `Hash`"
                                ),
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
            Deriving::PartialEq => partial_eq,
            Deriving::PartialOrd => partial_ord,
            Deriving::Ord => ord,
            Deriving::Debug => debug,
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
) -> impl Iterator<Item = TokenStream> + '_ {
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
#[derive(std::fmt::Debug)]
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
