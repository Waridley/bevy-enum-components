use convert_case::{Case::Snake, Casing};
use proc_macro::{Span, TokenStream};
use proc_macro_crate::FoundCrate;
use quote::{format_ident, quote, ToTokens};
use syn::{
	parse_macro_input, Attribute, Data, DataEnum, DeriveInput, Fields, Generics, Ident, Visibility,
};

type TokenStream2 = proc_macro2::TokenStream;

#[allow(unused)]
pub(crate) struct Context<'data> {
	_crate: Ident,
	attrs: Vec<Attribute>,
	vis: Visibility,
	generics: Generics,
	idents: Idents<'data>,
	data: &'data DataEnum,
}

pub(crate) struct Idents<'data> {
	main_enum: Ident,
	module: Ident,
	variant_trait: Ident,
	type_enum: Ident,
	type_method: Ident,
	component_struct: Ident,
	state: Ident,
	item: Ident,
	query: Ident,
	fetch: Ident,
	query_mut_struct: Ident,
	item_mut: Ident,
	query_mut: Ident,
	fetch_mut: Ident,
	fetch_mut_item: Ident,
	read_variant: Ident,
	write_variant: Ident,
	variants: Vec<&'data Ident>,
}

#[proc_macro_derive(EnumComponent)]
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

	// TODO: Could some of these names be simiplified since they are inside the module?
	//   - Alternatively, should they be moved outside the module?
	let type_enum = format_ident!("{ident}Type");
	let ctx = Context {
		_crate,
		attrs,
		vis,
		generics,
		idents: Idents {
			module: format_ident!("{}", ident.to_string().to_case(Snake)),
			variant_trait: format_ident!("{ident}Variant"),
			type_method: format_ident!("{}", type_enum.to_string().to_case(Snake)),
			type_enum,
			component_struct: format_ident!("{ident}Component"),
			state: format_ident!("{ident}State"),
			item: format_ident!("{ident}Item"),
			query: format_ident!("{ident}Query"),
			fetch: format_ident!("{ident}Fetch"),
			query_mut_struct: format_ident!("{ident}Mut"),
			item_mut: format_ident!("{ident}ItemMut"),
			query_mut: format_ident!("{ident}QueryMut"),
			fetch_mut: format_ident!("{ident}FetchMut"),
			fetch_mut_item: format_ident!("{ident}FetchMutItem"),
			read_variant: format_ident!("Read{ident}"),
			write_variant: format_ident!("Write{ident}"),
			variants: data.variants.iter().map(|v| &v.ident).collect::<Vec<_>>(),
			main_enum: ident, // move last
		},
		data,
	};

	module(&ctx).into_token_stream().into()
}

fn module(ctx: &Context) -> impl ToTokens {
	let enum_impls = enum_impls(ctx);
	let type_enum = type_enum(ctx);
	let variant_component = variant_component(ctx);
	let variant_trait = variant_trait_decl(ctx);
	let variant_structs = variant_structs(ctx);
	let enum_world_queries = enum_world_queries(ctx);
	let variant_world_queries = variant_world_queries(ctx);
	let Context { vis, idents: Idents { module, .. }, .. } = ctx;

	quote! {
		#[automatically_derived]
		#vis mod #module {
			use super::*;

			#enum_impls
			#enum_world_queries

			#type_enum

			#variant_trait
			#(#variant_structs)*

			#variant_component
			#variant_world_queries
		}
	}
}

fn enum_impls(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		vis,
		idents: Idents {
			main_enum,
			type_enum,
			type_method,
			component_struct,
			variants,
			..
		},
		data,
		..
	} = ctx;

	let variant_patterns = data
		.variants
		.iter()
		.map(|v| {
			let ident = &v.ident;
			match &v.fields {
				Fields::Named(fields) => {
					let fields = fields.named.iter().map(|field| &field.ident);
					quote! {
						#ident {
							#(#fields,)*
						}
					}
				}
				Fields::Unnamed(fields) => {
					let fields = (0..fields.unnamed.len())
						.into_iter()
						.map(|i| format_ident!("field_{i}"));
					quote! {
						#ident(
							#(#fields,)*
						)
					}
				}
				Fields::Unit => quote! { #ident },
			}
		})
		.collect::<Vec<_>>();

	let type_match_arms = data.variants.iter().map(|v| {
		let ident = &v.ident;
		match v.fields {
			Fields::Named(..) => quote! { Self::#ident { .. } => #type_enum::#ident },
			Fields::Unnamed(..) => quote! { Self::#ident(..) => #type_enum::#ident },
			Fields::Unit => quote! { Self::#ident => #type_enum::#ident },
		}
	});

	quote! {
		#[automatically_derived]
		impl ::#_crate::EnumComponent for #main_enum {
			fn dispatch_to(self, cmds: &mut ::#_crate::bevy_ecs::system::EntityCommands) {
				match self {
					#(Self::#variant_patterns =>
						#variant_patterns.insert_into(cmds),)*
				}
			}

			fn remove_from(cmds: &mut ::#_crate::bevy_ecs::system::EntityCommands) {
				cmds
					#(.remove::<#component_struct<#variants>>())*;
			}
		}

		#[automatically_derived]
		impl ::#_crate::bevy_ecs::query::ArchetypeFilter for #main_enum {}

		#[automatically_derived]
		impl #main_enum {
			#vis fn #type_method(&self) -> #type_enum {
				match self {
					#(#type_match_arms),*
				}
			}
		}
	}
}

fn type_enum(ctx: &Context) -> impl ToTokens {
	let Context {
		vis,
		idents: Idents {
			main_enum,
			type_enum,
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
			Fields::Named(..) => quote! { #main_enum::#ident { .. } => #type_enum::#ident },
			Fields::Unnamed(..) => quote! { #main_enum::#ident(..) => #type_enum::#ident },
			Fields::Unit => quote! { #main_enum::#ident => #type_enum::#ident },
		}
	});

	let item_match_arms = variants.iter().map(|v| {
		quote! { #item::#v(..) => #type_enum::#v }
	});

	quote! {
		#[allow(clippy::derive_partial_eq_without_eq)]
		#[derive(Debug, Copy, Clone, PartialEq)]
		#[automatically_derived]
		#vis enum #type_enum {
			#(#variants),*
		}

		#[automatically_derived]
		impl From<&#main_enum> for #type_enum {
			fn from(value: &#main_enum) -> Self {
				match value {
					#(#enum_match_arms),*
				}
			}
		}

		#[automatically_derived]
		impl From<&#item<'_>> for #type_enum {
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
		idents: Idents {
			main_enum,
			type_enum,
			type_method,
			item,
			query,
			item_mut,
			fetch_mut_item,
			variants,
			..
		},
		..
	} = ctx;

	let item_decl = quote! {
		#[allow(clippy::derive_partial_eq_without_eq)]
		#[derive(Debug, Clone, Copy, PartialEq)]
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
				( #(#patterns),* ) => #item::#v(&#var.0)
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
							"Multiple `{}` variant components exist: {:?}\n\
							Enum component variant should only be changed via EntityEnumCommands",
							stringify!(#main_enum), other
						)
				},
			]
		});
	let type_match_arms = variants.iter().map(|v| {
		quote! { Self::#v(..) => #type_enum::#v }
	});
	let type_mut_match_arms = type_match_arms.clone();
	let item_impls = quote! {
		#[automatically_derived]
		impl<'w> From<#query<'w>> for #item<'w> {
			fn from(fetch: #query<'w>) -> Self {
				match fetch {
					#(#match_arms),*
				}
			}
		}

		#[automatically_derived]
		impl #item<'_> {
			#vis fn #type_method(&self) -> #type_enum {
				match self {
					#(#type_match_arms),*
				}
			}
		}
	};

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
				( #(#patterns),* ) => #item_mut::#v(#var.map_unchanged(|#var| &mut #var.0))
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
							"Multiple `{}` variant components exist: {:?}\n\
							Enum component variant should only be changed via EnumCommands",
							stringify!(#main_enum), other
						)
				},
			]
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
			#vis fn #type_method(&self) -> #type_enum {
				match self {
					#(#type_mut_match_arms),*
				}
			}
		}
	};

	quote! {
		#item_decl
		#item_impls
		#item_mut_decl
		#item_mut_impls
	}
}

fn world_query_type_aliases(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		idents: Idents {
			component_struct,
			state,
			query,
			fetch,
			query_mut,
			fetch_mut,
			fetch_mut_item,
			variants,
			..
		},
		..
	} = ctx;

	let len = variants.len();
	let state_fields = vec![quote! { ::#_crate::bevy_ecs::component::ComponentId }; len];
	let state_alias = quote! {
		#[automatically_derived]
		type #state = (#(#state_fields),*);
	};

	let read_only_aliases = quote! {
		#[automatically_derived]
		type #query<'w> = (
			#(Option<&'w #component_struct<#variants>>,)*
		);

		#[automatically_derived]
		type #fetch<'w> = (
			#(::#_crate::bevy_ecs::query::OptionFetch<'w, &'w #component_struct<#variants>>,)*
		);
	};

	let mut_aliases = quote! {
		#[automatically_derived]
		type #query_mut<'w> = (
			#(Option<&'w mut #component_struct<#variants>>,)*
		);

		#[automatically_derived]
		type #fetch_mut<'w> = (
			#(::#_crate::bevy_ecs::query::OptionFetch<'w, &'w mut #component_struct<#variants>>,)*
		);

		#[automatically_derived]
		type #fetch_mut_item<'w> = (
			#(Option<::#_crate::bevy_ecs::world::Mut<'w, #component_struct<#variants>>>,)*
		);
	};

	quote! {
		#state_alias
		#read_only_aliases
		#mut_aliases
	}
}

fn world_query_read_impl(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		idents: Idents {
			main_enum,
			component_struct,
			state,
			item,
			query,
			fetch,
			variants,
			..
		},
		..
	} = ctx;

	let indices = (0..variants.len()).into_iter().map(syn::Index::from);
	let add_reads = indices
		.clone()
		.map(|i| quote! { access.add_read(state.#i) });
	let init_components = variants
		.iter()
		.map(|v| quote! { world.init_component::<#component_struct<#v>>() });
	let set_contains_ids = indices.map(|i| quote! { set_contains_id(state.#i) });

	let world_query_impl = quote! {
		#[automatically_derived]
		unsafe impl ::#_crate::bevy_ecs::query::WorldQuery for #main_enum {
			type Item<'a> = #item<'a>;
			type Fetch<'a> = #fetch<'a>;
			type ReadOnly = Self;
			type State = #state;

			fn shrink<'wlong: 'wshort, 'wshort>(
				item: ::#_crate::bevy_ecs::query::QueryItem<'wlong, Self>,
			) -> ::#_crate::bevy_ecs::query::QueryItem<'wshort, Self> {
				item
			}

			unsafe fn init_fetch<'w>(
				world: &'w World,
				state: &Self::State,
				last_change_tick: u32,
				change_tick: u32,
			) -> Self::Fetch<'w> {
				#query::init_fetch(world, state, last_change_tick, change_tick).into()
			}

			unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
				#query::clone_fetch(fetch)
			}

			const IS_DENSE: bool = false;
			const IS_ARCHETYPAL: bool = true;

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
				table_row: usize,
			) -> Self::Item<'w> {
				#query::fetch(fetch, entity, table_row).into()
			}


			fn update_component_access(state: &Self::State, access: &mut ::#_crate::bevy_ecs::query::FilteredAccess<::#_crate::bevy_ecs::component::ComponentId>) {
				#(#add_reads);*
			}

			fn update_archetype_component_access(
				state: &Self::State,
				archetype: &::#_crate::bevy_ecs::archetype::Archetype,
				access: &mut ::#_crate::bevy_ecs::query::Access<::#_crate::bevy_ecs::archetype::ArchetypeComponentId>,
			) {
				#query::update_archetype_component_access(state, archetype, access)
			}

			fn init_state(world: &mut World) -> Self::State {
				( #(#init_components),* )
			}

			fn matches_component_set(
				state: &Self::State,
				set_contains_id: &impl Fn(::#_crate::bevy_ecs::component::ComponentId) -> bool,
			) -> bool {
				#(#set_contains_ids) || *
			}
		}
	};

	let read_only_impl = quote! {
		#[automatically_derived]
		unsafe impl ::#_crate::bevy_ecs::query::ReadOnlyWorldQuery for #main_enum {
			//! SAFETY: All access defers to `&#component`
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
		idents: Idents {
			main_enum,
			component_struct,
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

	let struct_def = quote! {
		#[automatically_derived]
		#vis struct #query_mut_struct;
	};

	let indices = (0..variants.len()).into_iter().map(syn::Index::from);
	let add_writes = indices
		.clone()
		.map(|i| quote! { access.add_write(state.#i) });
	let init_components = variants
		.iter()
		.map(|v| quote! { world.init_component::<#component_struct<#v>>() });
	let set_contains_ids = indices.map(|i| quote! { set_contains_id(state.#i) });

	let world_query_mut_impl = quote! {
		#[automatically_derived]
		unsafe impl ::#_crate::bevy_ecs::query::WorldQuery for #query_mut_struct {
			type Item<'a> = #item_mut<'a>;
			type Fetch<'a> = #fetch_mut<'a>;
			type ReadOnly = #main_enum;
			type State = #state;

			fn shrink<'wlong: 'wshort, 'wshort>(
				item: ::#_crate::bevy_ecs::query::QueryItem<'wlong, Self>,
			) -> ::#_crate::bevy_ecs::query::QueryItem<'wshort, Self> {
				item
			}

			unsafe fn init_fetch<'w>(
				world: &'w World,
				state: &Self::State,
				last_change_tick: u32,
				change_tick: u32,
			) -> Self::Fetch<'w> {
				#query_mut::init_fetch(world, state, last_change_tick, change_tick).into()
			}

			unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
				#query_mut::clone_fetch(fetch)
			}

			const IS_DENSE: bool = false; // TODO: Make configurable
			const IS_ARCHETYPAL: bool = true;

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
				table_row: usize,
			) -> Self::Item<'w> {
				#query_mut::fetch(fetch, entity, table_row).into()
			}

			fn update_component_access(state: &Self::State, access: &mut ::#_crate::bevy_ecs::query::FilteredAccess<::#_crate::bevy_ecs::component::ComponentId>) {
				#(#add_writes);*
			}

			fn update_archetype_component_access(
				state: &Self::State,
				archetype: &::#_crate::bevy_ecs::archetype::Archetype,
				access: &mut ::#_crate::bevy_ecs::query::Access<::#_crate::bevy_ecs::archetype::ArchetypeComponentId>,
			) {
				#query_mut::update_archetype_component_access(state, archetype, access)
			}

			fn init_state(world: &mut World) -> Self::State {
				(#(#init_components),*)
			}

			fn matches_component_set(
				state: &Self::State,
				set_contains_id: &impl Fn(::#_crate::bevy_ecs::component::ComponentId) -> bool,
			) -> bool {
				#(#set_contains_ids) || *
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
		vis,
		idents: Idents {
			component_struct,
			variant_trait,
			read_variant,
			..
		},
		..
	} = ctx;

	let struct_def = quote! {
		#[derive(Debug)]
		#[automatically_derived]
		#vis struct #read_variant<T: #variant_trait>(::std::marker::PhantomData<T>);
	};

	let world_query_impl = quote! {
		#[automatically_derived]
		unsafe impl<T, const N: usize> ::#_crate::bevy_ecs::query::WorldQuery for #read_variant<T>
		where T: #variant_trait<State = ::#_crate::EnumVariantIndex<N>> {
			type Item<'a> = &'a T;
			type Fetch<'a> = <&'a #component_struct<T> as ::#_crate::bevy_ecs::query::WorldQuery>::Fetch<'a>;
			type ReadOnly = Self;
			type State = T::State;

			fn shrink<'wlong: 'wshort, 'wshort>(
				item: ::#_crate::bevy_ecs::query::QueryItem<'wlong, Self>,
			) -> ::#_crate::bevy_ecs::query::QueryItem<'wshort, Self> {
				item
			}

			unsafe fn init_fetch<'w>(
				world: &'w World,
				state: &Self::State,
				last_change_tick: u32,
				change_tick: u32,
			) -> Self::Fetch<'w> {
				<&#component_struct<T>>::init_fetch(world, &state.with, last_change_tick, change_tick)
			}

			unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
				<&'w #component_struct<T> as ::#_crate::bevy_ecs::query::WorldQuery>::clone_fetch(fetch)
			}

			const IS_DENSE: bool = false; // TODO: Make configurable
			const IS_ARCHETYPAL: bool = true;

			unsafe fn set_archetype<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				archetype: &'w ::#_crate::bevy_ecs::archetype::Archetype,
				tables: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				<&#component_struct<T>>::set_archetype(fetch, &state.with, archetype, tables)
			}

			unsafe fn set_table<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				table: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				<&#component_struct<T>>::set_table(fetch, &state.with, table)
			}

			unsafe fn fetch<'w>(
				fetch: &mut Self::Fetch<'w>,
				entity: Entity,
				table_row: usize,
			) -> Self::Item<'w> {
				&<&#component_struct<T>>::fetch(fetch, entity, table_row).0
			}

			fn update_component_access(state: &Self::State, access: &mut ::#_crate::bevy_ecs::query::FilteredAccess<::#_crate::bevy_ecs::component::ComponentId>) {
				state.update_component_access(access)
			}

			fn update_archetype_component_access(
				state: &Self::State,
				archetype: &::#_crate::bevy_ecs::archetype::Archetype,
				access: &mut ::#_crate::bevy_ecs::query::Access<::#_crate::bevy_ecs::archetype::ArchetypeComponentId>,
			) {
				<&#component_struct<T>>::update_archetype_component_access(&state.with, archetype, access)
			}

			fn init_state(world: &mut World) -> Self::State {
				T::init_state(world)
			}

			fn matches_component_set(
				&state: &Self::State,
				set_contains_id: &impl Fn(::#_crate::bevy_ecs::component::ComponentId) -> bool,
			) -> bool {
				if !set_contains_id(state.with) {
					return false;
				}
				for id in state.without {
					if set_contains_id(id) {
						return false;
					}
				}
				true
			}
		}
	};

	let read_only_impl = quote! {
		#[automatically_derived]
		unsafe impl<T, const N: usize> ::#_crate::bevy_ecs::query::ReadOnlyWorldQuery for #read_variant<T>
		where T: #variant_trait<State = ::#_crate::EnumVariantIndex<N>> {
			//! Safety: All access defers to `&#component_struct`
		}
	};

	quote! {
		#struct_def
		#world_query_impl
		#read_only_impl
	}
}

fn variant_world_query_mut_impl(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		vis,
		idents: Idents {
			component_struct,
			variant_trait,
			read_variant,
			write_variant,
			..
		},
		..
	} = ctx;

	let struct_def = quote! {
		#[derive(Debug)]
		#[automatically_derived]
		#vis struct #write_variant<T: #variant_trait>(::std::marker::PhantomData<T>);
	};

	let world_query_impl = quote! {
		#[automatically_derived]
		unsafe impl<T, const N: usize> ::#_crate::bevy_ecs::query::WorldQuery for #write_variant<T>
		where T: #variant_trait<State = ::#_crate::EnumVariantIndex<N>> {
			type Item<'a> = ::#_crate::bevy_ecs::world::Mut<'a, T>;
			type Fetch<'a> = <&'a mut #component_struct<T> as ::#_crate::bevy_ecs::query::WorldQuery>::Fetch<'a>;
			type ReadOnly = #read_variant<T>;
			type State = T::State;

			fn shrink<'wlong: 'wshort, 'wshort>(
				item: ::#_crate::bevy_ecs::query::QueryItem<'wlong, Self>,
			) -> ::#_crate::bevy_ecs::query::QueryItem<'wshort, Self> {
				item
			}

			unsafe fn init_fetch<'w>(
				world: &'w World,
				state: &Self::State,
				last_change_tick: u32,
				change_tick: u32,
			) -> Self::Fetch<'w> {
				<&mut #component_struct<T>>::init_fetch(world, &state.with, last_change_tick, change_tick)
			}

			unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
				<&'w mut #component_struct<T> as ::#_crate::bevy_ecs::query::WorldQuery>::clone_fetch(fetch)
			}

			const IS_DENSE: bool = false; // TODO: Make configurable
			const IS_ARCHETYPAL: bool = true;

			unsafe fn set_archetype<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				archetype: &'w ::#_crate::bevy_ecs::archetype::Archetype,
				tables: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				<&mut #component_struct<T>>::set_archetype(fetch, &state.with, archetype, tables)
			}

			unsafe fn set_table<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				table: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				<&mut #component_struct<T>>::set_table(fetch, &state.with, table)
			}

			unsafe fn fetch<'w>(
				fetch: &mut Self::Fetch<'w>,
				entity: Entity,
				table_row: usize,
			) -> Self::Item<'w> {
				<&mut #component_struct<T>>::fetch(fetch, entity, table_row).map_unchanged(|it| &mut it.0)
			}

			fn update_component_access(state: &Self::State, access: &mut ::#_crate::bevy_ecs::query::FilteredAccess<::#_crate::bevy_ecs::component::ComponentId>) {
				state.update_component_access(access)
			}

			fn update_archetype_component_access(
				state: &Self::State,
				archetype: &::#_crate::bevy_ecs::archetype::Archetype,
				access: &mut ::#_crate::bevy_ecs::query::Access<::#_crate::bevy_ecs::archetype::ArchetypeComponentId>,
			) {
				<&mut #component_struct<T>>::update_archetype_component_access(
					&state.with,
					archetype,
					access,
				)
			}

			fn init_state(world: &mut World) -> Self::State {
				T::init_state(world)
			}

			fn matches_component_set(
				state: &Self::State,
				set_contains_id: &impl Fn(::#_crate::bevy_ecs::component::ComponentId) -> bool,
			) -> bool {
				if !set_contains_id(state.with) {
					return false;
				}
				for id in state.without {
					if set_contains_id(id) {
						return false;
					}
				}
				true
			}
		}
	};

	quote! {
		#struct_def
		#world_query_impl
	}
}

fn variant_component(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		idents: Idents {
			component_struct,
			variant_trait,
			..
		},
		..
	} = ctx;

	quote! {
		#[automatically_derived]
		mod component {
			use super::*;
			use ::#_crate::bevy_ecs::prelude::Component;

			#[derive(Debug, Component)]
			#[automatically_derived]
			#[repr(transparent)]
			pub struct #component_struct<T: #variant_trait>(pub T);
		}

		use component::#component_struct; // TODO: make private to prevent user from adding component manually
	}
}

fn variant_trait_decl(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		idents: Idents {
			type_enum,
			type_method,
			variant_trait,
			..
		},
		..
	} = &ctx;

	quote! {
		#[automatically_derived]
		mod sealed { pub trait Sealed {} }
		#[automatically_derived]
		pub trait #variant_trait : sealed::Sealed + Send + Sync + 'static {
			type State: Send + Sync + Sized;
			fn init_state(world: &mut World) -> Self::State;
			fn insert_into(self, cmds: &mut ::#_crate::bevy_ecs::system::EntityCommands);
			fn #type_method() -> #type_enum;
		}
	}
}

fn variant_structs(ctx: &Context) -> Vec<TokenStream2> {
	let Context {
		_crate,
		idents: Idents {
			type_enum,
			type_method,
			component_struct,
			variant_trait,
			variants,
			..
		},
		data,
		..
	} = ctx;

	let num_excluded_types = data.variants.len() - 1;
	let variant_struct_defs = variant_struct_defs(ctx).collect::<Vec<_>>();

	variant_struct_defs
		.iter()
		.map(|(variant, tokens)| {
			let excluded = variants
				.iter()
				.filter_map(|other| {
					if variant == other {
						None
					} else {
						Some(*other)
					}
				})
				.collect::<Vec<_>>();
			quote! {
				#[allow(clippy::derive_partial_eq_without_eq)]
				#[derive(Debug, Clone, PartialEq)]
				#[automatically_derived]
				#tokens
				impl sealed::Sealed for #variant {}
				#[automatically_derived]
				impl #variant_trait for #variant {
					type State = ::#_crate::EnumVariantIndex<{#num_excluded_types}>;

					fn init_state(world: &mut World) -> Self::State {
						::#_crate::EnumVariantIndex {
							with: world.init_component::<#component_struct<#variant>>(),
							without: [
								#(world.init_component::<#component_struct<#excluded>>(),)*
							],
						}
					}

					fn insert_into(self, cmds: &mut ::#_crate::bevy_ecs::system::EntityCommands) {
						cmds.insert(#component_struct(self))
							#(.remove::<#component_struct<#excluded>>())*;
					}

					fn #type_method() -> #type_enum {
						#type_enum::#variant
					}
				}
			}
		})
		.collect::<Vec<_>>()
}

fn variant_struct_defs<'ctx>(
	ctx: &'ctx Context,
) -> impl Iterator<Item = (&'ctx Ident, TokenStream2)> + 'ctx {
	ctx.data.variants.iter().map(|v| {
		let variant = &v.ident;
		(
			variant,
			match &v.fields {
				Fields::Named(fields) => {
					let fields = fields.named.iter();
					quote! {
						#[automatically_derived]
						pub struct #variant {
							#(pub #fields,)*
						}
					}
				}
				Fields::Unnamed(fields) => {
					let fields = fields.unnamed.iter();
					quote! {
						#[automatically_derived]
						pub struct #variant ( #(pub #fields,)* );
					}
				}
				Fields::Unit => {
					quote! {
						#[automatically_derived]
						pub struct #variant;
					}
				}
			},
		)
	})
}
