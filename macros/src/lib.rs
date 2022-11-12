use convert_case::{Case::Snake, Casing};
use proc_macro::{Span, TokenStream};
use proc_macro_crate::FoundCrate;
use quote::{format_ident, quote, ToTokens};
use syn::{
	parse_macro_input, Attribute, Data, DataEnum, DeriveInput, Fields, Generics, Ident, Visibility,
};

type TokenStream2 = proc_macro2::TokenStream;

#[allow(unused)]
struct Context<'data> {
	_crate: Ident,
	attrs: Vec<Attribute>,
	vis: Visibility,
	enum_ident: Ident,
	generics: Generics,
	mod_ident: Ident,
	variant_trait_ident: Ident,
	type_enum_ident: Ident,
	type_method_ident: Ident,
	component_ident: Ident,
	state_ident: Ident,
	item_ident: Ident,
	query_ident: Ident,
	fetch_ident: Ident,
	query_mut_struct_ident: Ident,
	item_mut_ident: Ident,
	query_mut_ident: Ident,
	fetch_mut_ident: Ident,
	fetch_mut_item_ident: Ident,
	read_variant_ident: Ident,
	write_variant_ident: Ident,
	variant_idents: Vec<&'data Ident>,
	data: &'data DataEnum,
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
	let type_enum_ident = format_ident!("{ident}Type");
	let ctx = Context {
		_crate,
		attrs,
		vis,
		generics,
		mod_ident: format_ident!("{}", ident.to_string().to_case(Snake)),
		variant_trait_ident: format_ident!("{ident}Variant"),
		type_method_ident: format_ident!("{}", type_enum_ident.to_string().to_case(Snake)),
		type_enum_ident,
		component_ident: format_ident!("{ident}Component"),
		state_ident: format_ident!("{ident}State"),
		item_ident: format_ident!("{ident}Item"),
		query_ident: format_ident!("{ident}Query"),
		fetch_ident: format_ident!("{ident}Fetch"),
		query_mut_struct_ident: format_ident!("{ident}Mut"),
		item_mut_ident: format_ident!("{ident}ItemMut"),
		query_mut_ident: format_ident!("{ident}QueryMut"),
		fetch_mut_ident: format_ident!("{ident}FetchMut"),
		fetch_mut_item_ident: format_ident!("{ident}FetchMutItem"),
		read_variant_ident: format_ident!("Read{ident}"),
		write_variant_ident: format_ident!("Write{ident}"),
		variant_idents: data.variants.iter().map(|v| &v.ident).collect::<Vec<_>>(),
		enum_ident: ident, // move last
		data,
	};

	module(&ctx).into_token_stream().into()
}

fn module(ctx: &Context) -> impl ToTokens {
	let enum_impls = enum_impls(&ctx);
	let type_enum = type_enum(&ctx);
	let variant_component = variant_component(ctx);
	let variant_trait = variant_trait_decl(ctx);
	let variant_structs = variant_structs(ctx);
	let enum_world_queries = enum_world_queries(&ctx);
	let variant_world_queries = variant_world_queries(&ctx);
	let Context { vis, mod_ident, .. } = ctx;

	quote! {
		#vis mod #mod_ident {
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
		enum_ident,
		type_enum_ident,
		type_method_ident,
		component_ident,
		variant_idents,
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
			Fields::Named(..) => quote! { Self::#ident { .. } => #type_enum_ident::#ident },
			Fields::Unnamed(..) => quote! { Self::#ident(..) => #type_enum_ident::#ident },
			Fields::Unit => quote! { Self::#ident => #type_enum_ident::#ident },
		}
	});

	quote! {
		impl ::#_crate::EnumComponent for #enum_ident {
			fn dispatch_to(self, cmds: &mut ::#_crate::bevy_ecs::system::EntityCommands) {
				match self {
					#(Self::#variant_patterns =>
						#variant_patterns.insert_into(cmds),)*
				}
			}

			fn remove_from(cmds: &mut ::#_crate::bevy_ecs::system::EntityCommands) {
				cmds
					#(.remove::<#component_ident<#variant_idents>>())*;
			}
		}

		impl #enum_ident {
			#vis fn #type_method_ident(&self) -> #type_enum_ident {
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
		enum_ident,
		type_enum_ident,
		item_ident,
		variant_idents,
		data,
		..
	} = ctx;

	let enum_match_arms = data.variants.iter().map(|v| {
		let ident = &v.ident;
		match v.fields {
			Fields::Named(..) => quote! { #enum_ident::#ident { .. } => #type_enum_ident::#ident },
			Fields::Unnamed(..) => quote! { #enum_ident::#ident(..) => #type_enum_ident::#ident },
			Fields::Unit => quote! { #enum_ident::#ident => #type_enum_ident::#ident },
		}
	});

	let item_match_arms = variant_idents.iter().map(|v| {
		quote! { #item_ident::#v(..) => #type_enum_ident::#v }
	});

	quote! {
		#[derive(Debug, Copy, Clone, PartialEq)]
		#vis enum #type_enum_ident {
			#(#variant_idents),*
		}

		impl From<&#enum_ident> for #type_enum_ident {
			fn from(value: &#enum_ident) -> Self {
				match value {
					#(#enum_match_arms),*
				}
			}
		}

		impl From<&#item_ident<'_>> for #type_enum_ident {
			fn from(value: &#item_ident) -> Self {
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
		enum_ident,
		type_enum_ident,
		type_method_ident,
		item_ident,
		query_ident,
		item_mut_ident,
		fetch_mut_item_ident,
		variant_idents,
		..
	} = ctx;

	let item = quote! {
		#[derive(Debug, Clone, Copy, PartialEq)]
		#vis enum #item_ident<'w> {
			#(#variant_idents(&'w #variant_idents),)*
		}
	};

	let len = variant_idents.len();
	let match_arms = variant_idents
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
				( #(#patterns),* ) => #item_ident::#v(&#var.0)
			}
		})
		.chain({
			let nones = std::iter::repeat(quote! { None }).take(len);
			[
				quote! {
					( #(#nones),* ) => panic!("No {} variant components exist", stringify!(#enum_ident))
				},
				quote! {
					other => panic!(
							"Multiple `{}` variant components exist: {:?}\n\
							Enum component variant should only be changed via EntityEnumCommands",
							stringify!(#enum_ident), other
						)
				},
			]
		});
	let type_match_arms = variant_idents.iter().map(|v| {
		quote! { Self::#v(..) => #type_enum_ident::#v }
	});
	let type_mut_match_arms = type_match_arms.clone();
	let item_impls = quote! {
		impl<'w> From<#query_ident<'w>> for #item_ident<'w> {
			fn from(fetch: #query_ident<'w>) -> Self {
				match fetch {
					#(#match_arms),*
				}
			}
		}

		impl #item_ident<'_> {
			#vis fn #type_method_ident(&self) -> #type_enum_ident {
				match self {
					#(#type_match_arms),*
				}
			}
		}
	};

	let item_mut = quote! {
		#[derive(Debug)]
		#vis enum #item_mut_ident<'w> {
			#(#variant_idents(::#_crate::bevy_ecs::world::Mut<'w, #variant_idents>),)*
		}
	};
	let mut_match_arms = variant_idents
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
				( #(#patterns),* ) => #item_mut_ident::#v(#var.map_unchanged(|#var| &mut #var.0))
			}
		})
		.chain({
			let nones = ::std::iter::repeat(quote! { None }).take(len);
			[
				quote! {
					( #(#nones),* ) => panic!("No {} variant components exist", stringify!(#enum_ident))
				},
				quote! {
					other => panic!(
							"Multiple `{}` variant components exist: {:?}\n\
							Enum component variant should only be changed via EnumCommands",
							stringify!(#enum_ident), other
						)
				},
			]
		});
	let item_mut_impls = quote! {
		impl<'w> From<#fetch_mut_item_ident<'w>> for #item_mut_ident<'w> {
			fn from(fetch: #fetch_mut_item_ident<'w>) -> #item_mut_ident<'w> {
				match fetch {
					#(#mut_match_arms),*
				}
			}
		}

		impl #item_mut_ident<'_> {
			#vis fn #type_method_ident(&self) -> #type_enum_ident {
				match self {
					#(#type_mut_match_arms),*
				}
			}
		}
	};

	quote! {
		#item
		#item_impls
		#item_mut
		#item_mut_impls
	}
}

fn world_query_type_aliases(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		component_ident,
		state_ident,
		query_ident,
		fetch_ident,
		query_mut_ident,
		fetch_mut_ident,
		fetch_mut_item_ident,
		variant_idents,
		..
	} = ctx;

	let len = variant_idents.len();
	let state_fields = vec![quote! { ::#_crate::bevy_ecs::component::ComponentId }; len];
	let state_alias = quote! {
		type #state_ident = (#(#state_fields),*);
	};

	let read_only_aliases = quote! {
		type #query_ident<'w> = (
			#(Option<&'w #component_ident<#variant_idents>>,)*
		);

		type #fetch_ident<'w> = (
			#(::bevy::ecs::query::OptionFetch<'w, &'w #component_ident<#variant_idents>>,)*
		);
	};

	let mut_aliases = quote! {
		type #query_mut_ident<'w> = (
			#(Option<&'w mut #component_ident<#variant_idents>>,)*
		);

		type #fetch_mut_ident<'w> = (
			#(::bevy::ecs::query::OptionFetch<'w, &'w mut #component_ident<#variant_idents>>,)*
		);

		type #fetch_mut_item_ident<'w> = (
			#(Option<::#_crate::bevy_ecs::world::Mut<'w, #component_ident<#variant_idents>>>,)*
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
		enum_ident,
		component_ident,
		state_ident,
		item_ident,
		query_ident,
		fetch_ident,
		variant_idents,
		..
	} = ctx;

	let indices = (0..variant_idents.len())
		.into_iter()
		.map(|i| syn::Index::from(i));
	let add_reads = indices
		.clone()
		.map(|i| quote! { access.add_read(state.#i) });
	let init_components = variant_idents
		.iter()
		.map(|v| quote! { world.init_component::<#component_ident<#v>>() });
	let set_contains_ids = indices.map(|i| quote! { set_contains_id(state.#i) });

	let world_query_impl = quote! {
		unsafe impl ::#_crate::bevy_ecs::query::WorldQuery for #enum_ident {
			type Item<'a> = #item_ident<'a>;
			type Fetch<'a> = #fetch_ident<'a>;
			type ReadOnly = Self;
			type State = #state_ident;

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
				#query_ident::init_fetch(world, state, last_change_tick, change_tick).into()
			}

			unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
				#query_ident::clone_fetch(fetch)
			}

			const IS_DENSE: bool = false;
			const IS_ARCHETYPAL: bool = true;

			unsafe fn set_archetype<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				archetype: &'w ::#_crate::bevy_ecs::archetype::Archetype,
				tables: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				#query_ident::set_archetype(fetch, state, archetype, tables)
			}

			unsafe fn set_table<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				table: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				#query_ident::set_table(fetch, state, table)
			}

			unsafe fn fetch<'w>(
				fetch: &mut Self::Fetch<'w>,
				entity: Entity,
				table_row: usize,
			) -> Self::Item<'w> {
				#query_ident::fetch(fetch, entity, table_row).into()
			}


			fn update_component_access(state: &Self::State, access: &mut ::#_crate::bevy_ecs::query::FilteredAccess<::#_crate::bevy_ecs::component::ComponentId>) {
				#(#add_reads);*
			}

			fn update_archetype_component_access(
				state: &Self::State,
				archetype: &::#_crate::bevy_ecs::archetype::Archetype,
				access: &mut ::#_crate::bevy_ecs::query::Access<::#_crate::bevy_ecs::archetype::ArchetypeComponentId>,
			) {
				#query_ident::update_archetype_component_access(state, archetype, access)
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
		unsafe impl ::#_crate::bevy_ecs::query::ReadOnlyWorldQuery for #enum_ident {
			//! SAFETY: All access defers to `&#component_ident`
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
		enum_ident,
		component_ident,
		state_ident,
		query_mut_struct_ident,
		item_mut_ident,
		query_mut_ident,
		fetch_mut_ident,
		variant_idents,
		..
	} = ctx;

	let struct_def = quote! {
		#vis struct #query_mut_struct_ident;
	};

	let indices = (0..variant_idents.len())
		.into_iter()
		.map(|i| syn::Index::from(i));
	let add_writes = indices
		.clone()
		.map(|i| quote! { access.add_write(state.#i) });
	let init_components = variant_idents
		.iter()
		.map(|v| quote! { world.init_component::<#component_ident<#v>>() });
	let set_contains_ids = indices.map(|i| quote! { set_contains_id(state.#i) });

	let world_query_mut_impl = quote! {
		unsafe impl ::#_crate::bevy_ecs::query::WorldQuery for #query_mut_struct_ident {
			type Item<'a> = #item_mut_ident<'a>;
			type Fetch<'a> = #fetch_mut_ident<'a>;
			type ReadOnly = #enum_ident;
			type State = #state_ident;

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
				#query_mut_ident::init_fetch(world, state, last_change_tick, change_tick).into()
			}

			unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
				#query_mut_ident::clone_fetch(fetch)
			}

			const IS_DENSE: bool = false; // TODO: Make configurable
			const IS_ARCHETYPAL: bool = true;

			unsafe fn set_archetype<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				archetype: &'w ::#_crate::bevy_ecs::archetype::Archetype,
				tables: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				#query_mut_ident::set_archetype(fetch, state, archetype, tables)
			}

			unsafe fn set_table<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				table: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				#query_mut_ident::set_table(fetch, state, table)
			}

			unsafe fn fetch<'w>(
				fetch: &mut Self::Fetch<'w>,
				entity: Entity,
				table_row: usize,
			) -> Self::Item<'w> {
				#query_mut_ident::fetch(fetch, entity, table_row).into()
			}

			fn update_component_access(state: &Self::State, access: &mut ::#_crate::bevy_ecs::query::FilteredAccess<::#_crate::bevy_ecs::component::ComponentId>) {
				#(#add_writes);*
			}

			fn update_archetype_component_access(
				state: &Self::State,
				archetype: &::#_crate::bevy_ecs::archetype::Archetype,
				access: &mut ::#_crate::bevy_ecs::query::Access<::#_crate::bevy_ecs::archetype::ArchetypeComponentId>,
			) {
				#query_mut_ident::update_archetype_component_access(state, archetype, access)
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
		component_ident,
		variant_trait_ident,
		read_variant_ident,
		..
	} = ctx;

	let struct_def = quote! {
		#[derive(Debug)]
		#vis struct #read_variant_ident<T: #variant_trait_ident>(::std::marker::PhantomData<T>);
	};

	let world_query_impl = quote! {
		unsafe impl<T, const N: usize> ::#_crate::bevy_ecs::query::WorldQuery for #read_variant_ident<T>
		where T: #variant_trait_ident<State = ::#_crate::EnumVariantIndex<N>> {
			type Item<'a> = &'a T;
			type Fetch<'a> = <&'a #component_ident<T> as ::#_crate::bevy_ecs::query::WorldQuery>::Fetch<'a>;
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
				<&#component_ident<T>>::init_fetch(world, &state.with, last_change_tick, change_tick)
			}

			unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
				<&'w #component_ident<T> as ::#_crate::bevy_ecs::query::WorldQuery>::clone_fetch(fetch)
			}

			const IS_DENSE: bool = false; // TODO: Make configurable
			const IS_ARCHETYPAL: bool = true;

			unsafe fn set_archetype<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				archetype: &'w ::#_crate::bevy_ecs::archetype::Archetype,
				tables: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				<&#component_ident<T>>::set_archetype(fetch, &state.with, archetype, tables)
			}

			unsafe fn set_table<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				table: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				<&#component_ident<T>>::set_table(fetch, &state.with, table)
			}

			unsafe fn fetch<'w>(
				fetch: &mut Self::Fetch<'w>,
				entity: Entity,
				table_row: usize,
			) -> Self::Item<'w> {
				&<&#component_ident<T>>::fetch(fetch, entity, table_row).0
			}

			fn update_component_access(state: &Self::State, access: &mut ::#_crate::bevy_ecs::query::FilteredAccess<::#_crate::bevy_ecs::component::ComponentId>) {
				state.update_component_access(access)
			}

			fn update_archetype_component_access(
				state: &Self::State,
				archetype: &::#_crate::bevy_ecs::archetype::Archetype,
				access: &mut ::#_crate::bevy_ecs::query::Access<::#_crate::bevy_ecs::archetype::ArchetypeComponentId>,
			) {
				<&#component_ident<T>>::update_archetype_component_access(&state.with, archetype, access)
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
		unsafe impl<T, const N: usize> ::#_crate::bevy_ecs::query::ReadOnlyWorldQuery for #read_variant_ident<T>
		where T: #variant_trait_ident<State = ::#_crate::EnumVariantIndex<N>> {
			//! Safety: All access defers to `&#component_ident`
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
		component_ident,
		variant_trait_ident,
		read_variant_ident,
		write_variant_ident,
		..
	} = ctx;

	let struct_def = quote! {
		#[derive(Debug)]
		#vis struct #write_variant_ident<T: #variant_trait_ident>(::std::marker::PhantomData<T>);
	};

	let world_query_impl = quote! {
		unsafe impl<T, const N: usize> ::#_crate::bevy_ecs::query::WorldQuery for #write_variant_ident<T>
		where T: #variant_trait_ident<State = ::#_crate::EnumVariantIndex<N>> {
			type Item<'a> = ::#_crate::bevy_ecs::world::Mut<'a, T>;
			type Fetch<'a> = <&'a mut #component_ident<T> as ::#_crate::bevy_ecs::query::WorldQuery>::Fetch<'a>;
			type ReadOnly = #read_variant_ident<T>;
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
				<&mut #component_ident<T>>::init_fetch(world, &state.with, last_change_tick, change_tick)
			}

			unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
				<&'w mut #component_ident<T> as ::#_crate::bevy_ecs::query::WorldQuery>::clone_fetch(fetch)
			}

			const IS_DENSE: bool = false; // TODO: Make configurable
			const IS_ARCHETYPAL: bool = true;

			unsafe fn set_archetype<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				archetype: &'w ::#_crate::bevy_ecs::archetype::Archetype,
				tables: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				<&mut #component_ident<T>>::set_archetype(fetch, &state.with, archetype, tables)
			}

			unsafe fn set_table<'w>(
				fetch: &mut Self::Fetch<'w>,
				state: &Self::State,
				table: &'w ::#_crate::bevy_ecs::storage::Table,
			) {
				<&mut #component_ident<T>>::set_table(fetch, &state.with, table)
			}

			unsafe fn fetch<'w>(
				fetch: &mut Self::Fetch<'w>,
				entity: Entity,
				table_row: usize,
			) -> Self::Item<'w> {
				<&mut #component_ident<T>>::fetch(fetch, entity, table_row).map_unchanged(|foo| &mut foo.0)
			}

			fn update_component_access(state: &Self::State, access: &mut ::#_crate::bevy_ecs::query::FilteredAccess<::#_crate::bevy_ecs::component::ComponentId>) {
				state.update_component_access(access)
			}

			fn update_archetype_component_access(
				state: &Self::State,
				archetype: &::#_crate::bevy_ecs::archetype::Archetype,
				access: &mut ::#_crate::bevy_ecs::query::Access<::#_crate::bevy_ecs::archetype::ArchetypeComponentId>,
			) {
				<&mut #component_ident<T>>::update_archetype_component_access(
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
		component_ident,
		variant_trait_ident,
		..
	} = ctx;

	quote! {
		mod component {
			use super::*;
			use ::bevy::ecs::prelude::Component; // TODO: Find crate

			#[derive(Debug, Component)]
			#[repr(transparent)]
			pub struct #component_ident<T: #variant_trait_ident>(pub T);
		}

		use component::#component_ident; // TODO: make private to prevent user from adding component manually
	}
}

fn variant_trait_decl(ctx: &Context) -> impl ToTokens {
	let Context {
		_crate,
		type_enum_ident,
		type_method_ident,
		variant_trait_ident,
		..
	} = &ctx;

	quote! {
		mod sealed { pub trait Sealed {} }
		pub trait #variant_trait_ident : sealed::Sealed + Send + Sync + 'static {
			type State: Send + Sync + Sized;
			fn init_state(world: &mut World) -> Self::State;
			fn insert_into(self, cmds: &mut ::#_crate::bevy_ecs::system::EntityCommands);
			fn #type_method_ident() -> #type_enum_ident;
		}
	}
}

fn variant_structs(ctx: &Context) -> Vec<TokenStream2> {
	let Context {
		_crate,
		type_enum_ident,
		type_method_ident,
		component_ident,
		variant_trait_ident,
		variant_idents,
		data,
		..
	} = ctx;

	let num_excluded_types = data.variants.len() - 1;
	let variant_struct_defs = variant_struct_defs(ctx).collect::<Vec<_>>();

	variant_struct_defs
		.iter()
		.map(|(variant_ident, tokens)| {
			let excluded = variant_idents
				.iter()
				.filter_map(|other_ident| {
					if variant_ident == other_ident {
						None
					} else {
						Some(*other_ident)
					}
				})
				.collect::<Vec<_>>();
			quote! {
				#[derive(Debug, Clone, PartialEq)]
				#tokens
				impl sealed::Sealed for #variant_ident {}
				impl #variant_trait_ident for #variant_ident {
					type State = ::#_crate::EnumVariantIndex<{#num_excluded_types}>;

					fn init_state(world: &mut World) -> Self::State {
						::#_crate::EnumVariantIndex {
							with: world.init_component::<#component_ident<#variant_ident>>(),
							without: [
								#(world.init_component::<#component_ident<#excluded>>(),)*
							],
						}
					}

					fn insert_into(self, cmds: &mut ::#_crate::bevy_ecs::system::EntityCommands) {
						cmds.insert(#component_ident(self))
							#(.remove::<#component_ident<#excluded>>())*;
					}

					fn #type_method_ident() -> #type_enum_ident {
						#type_enum_ident::#variant_ident
					}
				}
			}
			.into()
		})
		.collect::<Vec<_>>()
}

fn variant_struct_defs<'ctx>(
	ctx: &'ctx Context,
) -> impl Iterator<Item = (&'ctx Ident, TokenStream2)> + 'ctx {
	ctx.data.variants.iter().map(|v| {
		let variant_ident = &v.ident;
		(
			variant_ident,
			match &v.fields {
				Fields::Named(fields) => {
					let fields = fields.named.iter();
					quote! {
						pub struct #variant_ident {
							#(pub #fields,)*
						}
					}
				}
				Fields::Unnamed(fields) => {
					let fields = fields.unnamed.iter();
					quote! {
						pub struct #variant_ident ( #(pub #fields,)* );
					}
				}
				Fields::Unit => {
					quote! {
						pub struct #variant_ident;
					}
				}
			},
		)
	})
}
