use convert_case::{Case::Snake, Casing};
use proc_macro::{Span, TokenStream};
use proc_macro_crate::FoundCrate;
use quote::{format_ident, quote, ToTokens};
use std::collections::HashMap;
use syn::{
	parse_macro_input, Attribute, Data, DataEnum, DeriveInput, Expr, Fields, Generics, Ident, Lit,
	Meta, NestedMeta, Path, Variant, Visibility,
};

type TokenStream2 = proc_macro2::TokenStream;

#[proc_macro_derive(EnumComponent, attributes(component))]
pub fn derive_enum_component(input: TokenStream) -> TokenStream {
	let _crate = proc_macro_crate::crate_name("sond-bevy-enum-components")
		.map(|found| match found {
			FoundCrate::Itself => format_ident!("sond_bevy_enum_components"),
			FoundCrate::Name(name) => format_ident!("{name}"),
		})
		.unwrap();

	let DeriveInput {
		attrs,
		vis,
		ident,
		generics,
		data,
	} = parse_macro_input!(input as DeriveInput);

	let data = match &data {
		Data::Enum(data) => data,
		_ => {
			return syn::Error::new(Span::call_site().into(), "Only enums are supported")
				.into_compile_error()
				.into()
		}
	};

	let attrs = Attrs::parse(attrs);
	let idents = Idents::generate(ident, data);

	let variants = data.variants.iter().map(|v| (&v.ident, v.into())).collect();

	let ctx = Context {
		_crate,
		attrs,
		vis,
		generics,
		idents,
		variants,
		data,
	};

	module(&ctx).into_token_stream().into()
}

#[allow(unused)]
pub(crate) struct Context<'data> {
	pub _crate: Ident,
	pub attrs: Attrs,
	pub vis: Visibility,
	pub generics: Generics,
	pub idents: Idents<'data>,
	pub variants: HashMap<&'data Ident, VariantData<'data>>,
	pub data: &'data DataEnum,
}

#[allow(unused)]
pub(crate) struct Attrs {
	pub component_attrs: Vec<NestedMeta>,
	pub derives: Vec<Path>,
	pub mutable: bool,
	pub storage: Option<Lit>,
	pub input: Vec<Attribute>,
}

impl Attrs {
	fn parse(attrs: Vec<Attribute>) -> Self {
		let component_attrs = attr_list(&mut attrs.iter(), "component");
		let derives = collect_derives(&mut component_attrs.iter());
		let mutable = component_attrs
			.iter()
			.find_map(|attr| match attr {
				NestedMeta::Meta(meta) => meta
					.path()
					.segments
					.last()
					.and_then(|seg| (seg.ident == "mutable").then_some(())),
				_ => None,
			})
			.is_some();
		let storage = component_attrs.iter().find_map(|attr| match attr {
			NestedMeta::Meta(Meta::NameValue(meta)) => meta
				.path
				.segments
				.last()
				.and_then(|seg| (seg.ident == "storage").then_some(meta.lit.clone())),
			_ => None,
		});

		Attrs {
			component_attrs,
			derives,
			mutable,
			storage,
			input: attrs,
		}
	}
}

#[allow(unused)]
pub(crate) struct Idents<'data> {
	pub main_enum: Ident,
	pub module: Ident,
	pub variant_trait: Ident,
	pub tag_enum: Ident,
	pub tag_method: Ident,
	pub component_struct: Ident,
	pub state: Ident,
	pub item: Ident,
	pub query: Ident,
	pub fetch: Ident,
	pub fetch_item: Ident,
	pub query_mut_struct: Ident,
	pub item_mut: Ident,
	pub query_mut: Ident,
	pub fetch_mut: Ident,
	pub fetch_mut_item: Ident,
	pub with: Ident,
	pub without: Ident,
	pub variants: Vec<&'data Ident>,
}

#[allow(unused)]
pub(crate) struct VariantData<'data> {
	pub ident: &'data Ident,
	pub derives: Vec<Path>,
	pub discriminant: Option<&'data Expr>,
	pub input: &'data Variant,
}

impl<'data> From<&'data Variant> for VariantData<'data> {
	fn from(value: &'data Variant) -> Self {
		let component_attrs = attr_list(&mut value.attrs.iter(), "component");
		let derives = collect_derives(&mut component_attrs.iter());

		Self {
			ident: &value.ident,
			derives,
			discriminant: value.discriminant.as_ref().map(|d| &d.1),
			input: value,
		}
	}
}

impl<'data> Idents<'data> {
	fn generate(main_enum: Ident, data: &'data DataEnum) -> Self {
		// TODO: Could some of these names be simiplified since they are inside the module?
		//   - Alternatively, should they be moved outside the module?
		let tag_enum = format_ident!("{main_enum}Tag");
		Self {
			module: format_ident!("{}", main_enum.to_string().to_case(Snake)),
			variant_trait: format_ident!("{main_enum}Variant"),
			tag_method: format_ident!("tag"),
			tag_enum,
			component_struct: format_ident!("{main_enum}Component"),
			state: format_ident!("{main_enum}State"),
			item: format_ident!("{main_enum}Item"),
			query: format_ident!("{main_enum}Query"),
			fetch: format_ident!("{main_enum}Fetch"),
			fetch_item: format_ident!("{main_enum}FetchItem"),
			query_mut_struct: format_ident!("{main_enum}Mut"),
			item_mut: format_ident!("{main_enum}ItemMut"),
			query_mut: format_ident!("{main_enum}QueryMut"),
			fetch_mut: format_ident!("{main_enum}FetchMut"),
			fetch_mut_item: format_ident!("{main_enum}FetchMutItem"),
			with: format_ident!("With{main_enum}"),
			without: format_ident!("Without{main_enum}"),
			variants: data.variants.iter().map(|v| &v.ident).collect::<Vec<_>>(),
			main_enum, // move last
		}
	}
}

fn collect_derives<'a>(metas: impl Iterator<Item = &'a NestedMeta>) -> Vec<Path> {
	metas.fold(vec![], |mut acc, item| {
		if let NestedMeta::Meta(Meta::List(list)) = item {
			if list.path.segments.last().unwrap().ident == "derive" {
				list.nested.iter().for_each(|meta| match meta {
					NestedMeta::Meta(Meta::Path(path)) => acc.push(path.clone()),
					other => panic!("unexpected derive input {other:?}"),
				});
			}
		}
		acc
	})
}

fn attr_list<'a>(
	attrs: impl Iterator<Item = &'a Attribute>,
	path: impl AsRef<str>,
) -> Vec<NestedMeta> {
	attrs.fold(vec![], |mut acc, attr| {
		if attr
			.path
			.segments
			.last()
			.filter(|seg| seg.ident == path)
			.is_some()
		{
			match attr.parse_meta().unwrap() {
				Meta::List(list) => acc.extend(list.nested),
				other => panic!("unexpected component attribute {other:?}"),
			}
		};
		acc
	})
}

fn module(ctx: &Context) -> impl ToTokens {
	let enum_impls = enum_impls(ctx);
	let tag_enum = tag_enum(ctx);
	// let variant_component = variant_component(ctx);
	// let variant_trait = variant_trait_decl(ctx);
	let variant_structs = variant_structs(ctx);
	let enum_world_queries = enum_world_queries(ctx);
	let variant_world_queries = variant_world_queries(ctx);
	let Context {
		vis,
		idents: Idents { module, .. },
		..
	} = ctx;

	quote! {
		#[automatically_derived]
		#vis mod #module {
			use super::*;

			#enum_impls
			#enum_world_queries

			#tag_enum

			// #variant_trait
			#(#variant_structs)*

			// #variant_component
			#variant_world_queries
		}
	}
}

fn enum_impls(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		idents: Idents {
			main_enum,
			tag_enum,
			..
		},
		data,
		..
	} = ctx;

	let type_match_arms = data.variants.iter().map(|v| {
		let ident = &v.ident;
		match v.fields {
			Fields::Named(..) => quote! { Self::#ident { .. } => #tag_enum::#ident },
			Fields::Unnamed(..) => quote! { Self::#ident(..) => #tag_enum::#ident },
			Fields::Unit => quote! { Self::#ident => #tag_enum::#ident },
		}
	});

	quote! {
		#[automatically_derived]
		impl ::#_crate::EnumComponent for #main_enum {
			type Tag = #tag_enum;

			fn tag(&self) -> Self::Tag {
				match self {
					#(#type_match_arms),*
				}
			}
		}
	}
}

fn tag_enum(ctx: &Context) -> impl ToTokens {
	let Context {
		vis,
		idents: Idents {
			main_enum,
			tag_enum,
			item,
			variants,
			..
		},
		data,
		..
	} = ctx;

	let enum_match_arms = data.variants.iter().map(|v| {
		let ident = &v.ident;
		match v.fields {
			Fields::Named(..) => quote! { #main_enum::#ident { .. } => #tag_enum::#ident },
			Fields::Unnamed(..) => quote! { #main_enum::#ident(..) => #tag_enum::#ident },
			Fields::Unit => quote! { #main_enum::#ident => #tag_enum::#ident },
		}
	});

	let item_match_arms = variants.iter().map(|v| {
		quote! { #item::#v(..) => #tag_enum::#v }
	});

	quote! {
		#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
		#[automatically_derived]
		#vis enum #tag_enum {
			#(#variants),*
		}

		#[automatically_derived]
		impl From<&#main_enum> for #tag_enum {
			fn from(value: &#main_enum) -> Self {
				match value {
					#(#enum_match_arms),*
				}
			}
		}

		#[automatically_derived]
		impl From<&#item<'_>> for #tag_enum {
			fn from(value: &#item) -> Self {
				match value {
					#(#item_match_arms),*
				}
			}
		}
	}
}

fn enum_world_queries(ctx: &Context) -> impl ToTokens {
	let query_items = world_query_items(ctx);
	let type_aliases = world_query_type_aliases(ctx);
	let read_impl = world_query_read_impl(ctx);
	let mut_impl = world_query_mut_impl(ctx);
	quote! {
		#query_items
		#type_aliases
		#read_impl
		#mut_impl
	}
}

fn world_query_items(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		vis,
		attrs: Attrs { derives, .. },
		idents:
			Idents {
				main_enum,
				tag_enum,
				tag_method,
				item,
				fetch_item,
				variants,
				..
			},
		..
	} = ctx;

	let item_decl = quote! {
		#[derive(#(#derives),*)]
		#[automatically_derived]
		#vis enum #item<'w> {
			#(#variants(&'w #variants),)*
		}
	};

	let len = variants.len();
	let match_arms = variants
		.iter()
		.enumerate()
		.map(|(i, v)| {
			let var = format_ident!("{}", v.to_string().to_case(Snake));
			let mut patterns = vec![None; len];
			patterns[i] = Some(&var);
			let patterns = patterns.iter().map(|opt| {
				opt.as_ref()
					.map_or_else(|| quote! { None }, |v| quote! { Some(#v) })
			});
			quote! {
				( #(#patterns),* ) => #item::#v(#var)
			}
		})
		.chain({
			let nones = std::iter::repeat(quote! { None }).take(len);
			[
				quote! {
					( #(#nones),* ) => panic!("No {} variant components exist", stringify!(#main_enum))
				},
				quote! {
					other => panic!(
							"Multiple `{}` variant components exist",
							stringify!(#main_enum)
						)
				},
			]
		});
	let type_match_arms = variants.iter().map(|v| {
		quote! { Self::#v(..) => #tag_enum::#v }
	});
	let item_impls = quote! {
		#[automatically_derived]
		impl<'w> From<#fetch_item<'w>> for #item<'w> {
			fn from(fetch: #fetch_item<'w>) -> Self {
				match fetch {
					#(#match_arms),*
				}
			}
		}

		#[automatically_derived]
		impl #item<'_> {
			#vis fn #tag_method(&self) -> #tag_enum {
				match self {
					#(#type_match_arms),*
				}
			}
		}
	};

	let mut_decls = world_query_item_mut_decls(ctx);

	quote! {
		#item_decl
		#item_impls
		#mut_decls
	}
}

fn world_query_item_mut_decls(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		vis,
		idents:
			Idents {
				main_enum,
				tag_enum,
				tag_method,
				item_mut,
				fetch_mut_item,
				variants,
				..
			},
		attrs: Attrs { mutable, .. },
		..
	} = ctx;

	if !mutable {
		return quote! {};
	}

	let len = variants.len();

	let item_mut_decl = quote! {
		#[derive(Debug)]
		#vis enum #item_mut<'w> {
			#(#variants(::#_crate::bevy_ecs::world::Mut<'w, #variants>),)*
		}
	};
	let mut_match_arms = variants
		.iter()
		.enumerate()
		.map(|(i, v)| {
			let var = format_ident!("{}", v.to_string().to_case(Snake));
			let mut patterns = vec![None; len];
			patterns[i] = Some(&var);
			let patterns = patterns.iter().map(|opt| {
				opt.as_ref()
					.map_or_else(|| quote! { None }, |v| quote! { Some(#v) })
			});
			quote! {
				( #(#patterns),* ) => #item_mut::#v(#var)
			}
		})
		.chain({
			let nones = ::std::iter::repeat(quote! { None }).take(len);
			[
				quote! {
					( #(#nones),* ) => panic!("No {} variant components exist", stringify!(#main_enum))
				},
				quote! {
					other => panic!(
							"Multiple `{}` variant components exist",
							stringify!(#main_enum)
						)
				},
			]
		});
	let type_match_arms = variants.iter().map(|v| {
		quote! { Self::#v(..) => #tag_enum::#v }
	});
	let item_mut_impls = quote! {
		#[automatically_derived]
		impl<'w> From<#fetch_mut_item<'w>> for #item_mut<'w> {
			fn from(fetch: #fetch_mut_item<'w>) -> #item_mut<'w> {
				match fetch {
					#(#mut_match_arms),*
				}
			}
		}

		#[automatically_derived]
		impl #item_mut<'_> {
			#vis fn #tag_method(&self) -> #tag_enum {
				match self {
					#(#type_match_arms),*
				}
			}
		}
	};

	quote! {
		#item_mut_decl
		#item_mut_impls
	}
}

fn world_query_type_aliases(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		idents:
			Idents {
				state,
				query,
				fetch,
				fetch_item,
				query_mut,
				fetch_mut,
				fetch_mut_item,
				with,
				without,
				variants,
				..
			},
		attrs: Attrs { mutable, .. },
		..
	} = ctx;

	let state_alias = quote! {
		#[automatically_derived]
		type #state = <#query<'static> as ::#_crate::bevy_ecs::query::WorldQuery>::State;
	};

	let read_only_aliases = quote! {
		#[automatically_derived]
		type #query<'w> = ::#_crate::bevy_ecs::query::AnyOf<(
			#(::#_crate::ERef<'w, #variants>),*
		)>;

		#[automatically_derived]
		type #fetch<'w> = <#query<'w> as ::#_crate::bevy_ecs::query::WorldQuery>::Fetch<'w>;

		#[automatically_derived]
		type #fetch_item<'w> = (
			#(Option<&'w #variants>),*
		);
	};

	let mut_aliases = if *mutable {
		quote! {
			#[automatically_derived]
			type #query_mut<'w> = ::#_crate::bevy_ecs::query::AnyOf<(
				#(::#_crate::EMut<'w, #variants>),*
			)>;

			#[automatically_derived]
			type #fetch_mut<'w> = <#query_mut<'w> as ::#_crate::bevy_ecs::query::WorldQuery>::Fetch<'w>;

			#[automatically_derived]
			type #fetch_mut_item<'w> = (
				#(Option<::#_crate::bevy_ecs::world::Mut<'w, #variants>>),*
			);
		}
	} else {
		quote! {}
	};

	let filter_aliases = quote! {
		#[automatically_derived]
		pub type #with = ::#_crate::bevy_ecs::query::Or<(
			#(::#_crate::WithVariant<#variants>),*
		)>;

		#[automatically_derived]
		pub type #without = (
			#(::#_crate::WithoutVariant<#variants>),*
		);
	};

	quote! {
		#state_alias
		#read_only_aliases
		#mut_aliases
		#filter_aliases
	}
}

fn world_query_read_impl(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		idents: Idents {
			main_enum,
			state,
			item,
			query,
			fetch,
			variants,
			..
		},
		..
	} = ctx;

	let shrinks = variants
		.iter()
		.map(|v| quote! { #item::#v(it) => #item::#v(<::#_crate::ERef<#v>>::shrink(it)) });

	let world_query_impl = quote! {
		#[automatically_derived]
		unsafe impl ::#_crate::bevy_ecs::query::QueryData for #main_enum {
			type ReadOnly = Self;
		}

		#[automatically_derived]
		unsafe impl ::#_crate::bevy_ecs::query::WorldQuery for #main_enum {
			type Item<'a> = #item<'a>;
			type Fetch<'a> = #fetch<'a>;
			type State = #state;

			fn shrink<'wlong: 'wshort, 'wshort>(
				item: ::#_crate::bevy_ecs::query::QueryItem<'wlong, Self>,
			) -> ::#_crate::bevy_ecs::query::QueryItem<'wshort, Self> {
				match item {
					#(#shrinks,)*
				}
			}

			unsafe fn init_fetch<'w>(
				world: ::#_crate::bevy_ecs::world::unsafe_world_cell::UnsafeWorldCell<'w>,
				state: &Self::State,
				last_change_tick: ::#_crate::bevy_ecs::component::Tick,
				change_tick: ::#_crate::bevy_ecs::component::Tick,
			) -> Self::Fetch<'w> {
				#query::init_fetch(world, state, last_change_tick, change_tick)
			}

			const IS_DENSE: bool = <#query as ::#_crate::bevy_ecs::query::WorldQuery>::IS_DENSE;

			unsafe fn set_archetype<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				archetype: &'w ::#_crate::bevy_ecs::archetype::Archetype,
				tables: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				#query::set_archetype(fetch, state, archetype, tables)
			}

			unsafe fn set_table<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				table: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				#query::set_table(fetch, state, table)
			}

			unsafe fn fetch<'w>(
				fetch: &mut Self::Fetch<'w>,
				entity: Entity,
				table_row: ::#_crate::bevy_ecs::storage::TableRow,
			) -> Self::Item<'w> {
				#query::fetch(fetch, entity, table_row).into()
			}


			fn update_component_access(state: &Self::State, access: &mut ::#_crate::bevy_ecs::query::FilteredAccess<::#_crate::bevy_ecs::component::ComponentId>) {
				#query::update_component_access(state, access)
			}

			fn init_state(world: &mut World) -> Self::State {
				#query::init_state(world)
			}

			fn get_state(world: &World) -> Option<Self::State> {
				#query::get_state(world)
			}

			fn matches_component_set(
				state: &Self::State,
				set_contains_id: &impl Fn(::#_crate::bevy_ecs::component::ComponentId) -> bool,
			) -> bool {
				#query::matches_component_set(state, set_contains_id)
			}
		}
	};

	let safety = "SAFETY: All access defers to `&Variant`";
	let read_only_impl = quote! {
		#[automatically_derived]
		unsafe impl ::#_crate::bevy_ecs::query::ReadOnlyQueryData for #main_enum {
			#![doc = #safety]
		}
	};

	quote! {
		#world_query_impl
		#read_only_impl
	}
}

fn world_query_mut_impl(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		vis,
		attrs: Attrs { mutable, .. },
		idents:
			Idents {
				main_enum,
				state,
				query_mut_struct,
				item_mut,
				query_mut,
				fetch_mut,
				variants,
				..
			},
		..
	} = ctx;

	if !mutable {
		return quote! {};
	}

	let struct_def = quote! {
		#[automatically_derived]
		#vis struct #query_mut_struct;
	};
	let shrinks = variants
		.iter()
		.map(|v| quote! { #item_mut::#v(it) => #item_mut::#v(<::#_crate::EMut<#v>>::shrink(it)) });

	let world_query_mut_impl = quote! {
		#[automatically_derived]
		unsafe impl ::#_crate::bevy_ecs::query::QueryData for #query_mut_struct {
			type ReadOnly = #main_enum;
		}

		#[automatically_derived]
		unsafe impl ::#_crate::bevy_ecs::query::WorldQuery for #query_mut_struct {
			type Item<'a> = #item_mut<'a>;
			type Fetch<'a> = #fetch_mut<'a>;
			type State = #state;

			fn shrink<'wlong: 'wshort, 'wshort>(
				item: ::#_crate::bevy_ecs::query::QueryItem<'wlong, Self>,
			) -> ::#_crate::bevy_ecs::query::QueryItem<'wshort, Self> {
				match item {
					#(#shrinks,)*
				}
			}

			unsafe fn init_fetch<'w>(
				world: ::#_crate::bevy_ecs::world::unsafe_world_cell::UnsafeWorldCell<'w>,
				state: &Self::State,
				last_change_tick: ::#_crate::bevy_ecs::component::Tick,
				change_tick: ::#_crate::bevy_ecs::component::Tick,
			) -> Self::Fetch<'w> {
				#query_mut::init_fetch(world, state, last_change_tick, change_tick)
			}

			const IS_DENSE: bool = <#query_mut as ::#_crate::bevy_ecs::query::WorldQuery>::IS_DENSE;

			unsafe fn set_archetype<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				archetype: &'w ::#_crate::bevy_ecs::archetype::Archetype,
				tables: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				#query_mut::set_archetype(fetch, state, archetype, tables)
			}

			unsafe fn set_table<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				table: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				#query_mut::set_table(fetch, state, table)
			}

			unsafe fn fetch<'w>(
				fetch: &mut Self::Fetch<'w>,
				entity: Entity,
				table_row: ::#_crate::bevy_ecs::storage::TableRow,
			) -> Self::Item<'w> {
				#query_mut::fetch(fetch, entity, table_row).into()
			}

			fn update_component_access(state: &Self::State, access: &mut ::#_crate::bevy_ecs::query::FilteredAccess<::#_crate::bevy_ecs::component::ComponentId>) {
				#query_mut::update_component_access(state, access)
			}

			fn init_state(world: &mut World) -> Self::State {
				#query_mut::init_state(world)
			}

			fn get_state(world: &World) -> Option<Self::State> {
				#query_mut::get_state(world)
			}

			fn matches_component_set(
				state: &Self::State,
				set_contains_id: &impl Fn(::#_crate::bevy_ecs::component::ComponentId) -> bool,
			) -> bool {
				#query_mut::matches_component_set(state, set_contains_id)
			}
		}
	};

	quote! {
		#struct_def
		#world_query_mut_impl
	}
}

fn variant_world_queries(ctx: &Context) -> impl ToTokens {
	let read_impl = variant_world_query_read_impl(ctx);
	let mut_impl = variant_world_query_mut_impl(ctx);

	quote! {
		#read_impl
		#mut_impl
	}
}

fn variant_world_query_read_impl(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		idents: Idents {
			main_enum,
			variants,
			..
		},
		attrs: Attrs { storage, .. },
		..
	} = ctx;

	let wo = variants.len() - 1;

	let enum_component_variant_impls = variants.iter().map(|variant| {
		let excluded = variants
			.iter()
			.filter_map(|other| if variant == other { None } else { Some(*other) })
			.collect::<Vec<_>>();

		// TODO: Check variant attrs first, then common attrs
		let storage = storage
			.as_ref()
			.map(|lit| match lit {
				Lit::Str(s) => match &*s.value() {
					"SparseSet" => quote! { ::#_crate::bevy_ecs::component::SparseStorage },
					"Table" => quote! { ::#_crate::bevy_ecs::component::TableStorage },
					s => panic!("Unknown storage type: {s}"),
				},
				other => panic!("Unexpected literal: {other:?}"),
			})
			.unwrap_or_else(|| quote! { ::#_crate::bevy_ecs::component::TableStorage });

		quote! {
			impl ::#_crate::EnumComponentVariant for #variant {
				type Enum = #main_enum;
				type State = ::#_crate::EnumVariantIndex<#wo>;
				type Storage = #storage;

				fn tag() -> <Self::Enum as EnumComponent>::Tag {
					<Self::Enum as EnumComponent>::Tag::#variant
				}

				fn init_state(world: &mut World) -> Self::State {
					::#_crate::EnumVariantIndex {
						with: #variant::init_component(world),
						without: [
							#(#excluded::init_component(world),)*
						],
					}
				}

				fn get_state(world: &World) -> Option<Self::State> {
					Some(::#_crate::EnumVariantIndex {
						with: #variant::get_component(world)?,
						without: [
							#(#excluded::get_component(world)?,)*
						],
					})
				}

				fn dispatch_to(self, cmds: &mut ::#_crate::bevy_ecs::system::EntityCommands) {
					#(::#_crate::remove_variant::<#excluded>(cmds);)*
					// SAFETY: Already ensured removal of all other variants.
					unsafe { ::#_crate::insert_variant(cmds, self) };
				}

				fn remove_from(cmds: &mut ::#_crate::bevy_ecs::system::EntityCommands) {
					#(::#_crate::remove_variant::<#variants>(cmds);)*
				}
			}
		}
	});

	quote! {
		#(#enum_component_variant_impls)*
	}
}

fn variant_world_query_mut_impl(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		attrs: Attrs { mutable, .. },
		idents: Idents { variants, .. },
		..
	} = ctx;

	if !mutable {
		return quote! {};
	}

	let enum_component_variant_mut_impls = variants.iter().map(|variant| {
		quote! {
			impl ::#_crate::EnumComponentVariantMut for #variant {}
		}
	});

	quote! {
		#(#enum_component_variant_mut_impls)*
	}
}

fn variant_structs(ctx: &Context) -> Vec<TokenStream2> {
	let variant_struct_defs = variant_struct_defs(ctx).collect::<Vec<_>>();

	variant_struct_defs
		.into_iter()
		.map(|(_, tokens)| tokens)
		.collect::<Vec<_>>()
}

fn variant_struct_defs<'ctx>(
	ctx: &'ctx Context,
) -> impl Iterator<Item = (&'ctx Ident, TokenStream2)> + 'ctx {
	ctx.variants.iter().map(|(variant, data)| {
		let v = data.input;
		let common_derives = ctx.attrs.derives.iter();
		let derives = common_derives.chain(data.derives.iter());
		(
			*variant,
			match &v.fields {
				Fields::Named(fields) => {
					let fields = fields.named.iter();
					quote! {
						#[automatically_derived]
						#[derive(#(#derives),*)]
						pub struct #variant {
							#(pub #fields,)*
						}
					}
				}
				Fields::Unnamed(fields) => {
					let fields = fields.unnamed.iter();
					quote! {
						#[automatically_derived]
						#[derive(#(#derives),*)]
						pub struct #variant ( #(pub #fields,)* );
					}
				}
				Fields::Unit => {
					quote! {
						#[automatically_derived]
						#[derive(#(#derives),*)]
						pub struct #variant;
					}
				}
			},
		)
	})
}
