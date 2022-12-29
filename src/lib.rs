#[doc(hidden)]
/// Exported for derive macro usage.
pub use bevy_ecs;

use bevy_ecs::query::{FilteredAccess, ReadOnlyWorldQuery, WorldQuery};
use bevy_ecs::system::EntityCommands;
use bevy_ecs::{bundle::Bundle, component::ComponentId};

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariantIndex<const WO: usize> {
	pub with: ComponentId,
	pub without: [ComponentId; WO],
}

impl<const N: usize> EnumVariantIndex<N> {
	pub fn update_component_access(&self, access: &mut FilteredAccess<ComponentId>) {
		access.add_read(self.with);
		for id in self.without {
			access.add_without(id)
		}
	}
}

pub use macros::EnumComponent;
pub trait EnumComponent {
	type Tag: Send + Sync + 'static;

	fn tag(&self) -> Self::Tag;
	fn dispatch_to(self, cmds: &mut EntityCommands);
	fn remove_from(cmds: &mut EntityCommands);
}

pub trait EntityEnumCommands {
	fn set_enum<E: EnumComponent>(&mut self, value: E) -> &mut Self;
	fn remove_enum<E: EnumComponent>(&mut self) -> &mut Self;
}

impl<'w, 's, 'a> EntityEnumCommands for EntityCommands<'w, 's, 'a> {
	fn set_enum<E: EnumComponent>(&mut self, value: E) -> &mut Self {
		value.dispatch_to(self);
		self
	}

	fn remove_enum<E: EnumComponent>(&mut self) -> &mut Self {
		E::remove_from(self);
		self
	}
}

pub trait EnumComponentVariant: Send + Sync + 'static {
	type Enum: EnumComponent;
	type State: Send + Sync + 'static;

	fn tag() -> <Self::Enum as EnumComponent>::Tag;

	fn bundle(self) -> EnumBundle<Self>
	where
		Self: Sized,
	{
		EnumBundle {
			tag: EnumTag(Self::tag()),
			value: Variant(self),
		}
	}

	fn init_state(world: &mut bevy_ecs::world::World) -> Self::State;
}

pub trait EnumComponentVariantMut: EnumComponentVariant {}

pub struct ERef<'w, T: EnumComponentVariant>(&'w T);

unsafe impl<T: EnumComponentVariant<State = EnumVariantIndex<WO>>, const WO: usize> WorldQuery
	for ERef<'_, T>
{
	type Item<'a> = &'a T;
	type Fetch<'a> = <&'a Variant<T> as WorldQuery>::Fetch<'a>;
	type ReadOnly = Self;
	type State = T::State;

	fn shrink<'wlong: 'wshort, 'wshort>(
		item: bevy_ecs::query::QueryItem<'wlong, Self>,
	) -> bevy_ecs::query::QueryItem<'wshort, Self> {
		item
	}

	unsafe fn init_fetch<'w>(
		world: &'w bevy_ecs::world::World,
		state: &Self::State,
		last_change_tick: u32,
		change_tick: u32,
	) -> Self::Fetch<'w> {
		<&Variant<T>>::init_fetch(world, &state.with, last_change_tick, change_tick)
	}

	unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
		<&'w Variant<T> as bevy_ecs::query::WorldQuery>::clone_fetch(fetch)
	}

	const IS_DENSE: bool = false; // TODO: Make configurable
	const IS_ARCHETYPAL: bool = true;

	unsafe fn set_archetype<'w>(
		fetch: &mut Self::Fetch<'w>,
		state: &Self::State,
		archetype: &'w bevy_ecs::archetype::Archetype,
		tables: &'w bevy_ecs::storage::Table,
	) {
		<&Variant<T>>::set_archetype(fetch, &state.with, archetype, tables)
	}

	unsafe fn set_table<'w>(
		fetch: &mut Self::Fetch<'w>,
		state: &Self::State,
		table: &'w bevy_ecs::storage::Table,
	) {
		<&Variant<T>>::set_table(fetch, &state.with, table)
	}

	unsafe fn fetch<'w>(
		fetch: &mut Self::Fetch<'w>,
		entity: bevy_ecs::entity::Entity,
		table_row: usize,
	) -> Self::Item<'w> {
		&<&Variant<T>>::fetch(fetch, entity, table_row).0
	}

	fn update_component_access(
		state: &Self::State,
		access: &mut bevy_ecs::query::FilteredAccess<bevy_ecs::component::ComponentId>,
	) {
		state.update_component_access(access)
	}

	fn update_archetype_component_access(
		state: &Self::State,
		archetype: &bevy_ecs::archetype::Archetype,
		access: &mut bevy_ecs::query::Access<bevy_ecs::archetype::ArchetypeComponentId>,
	) {
		<&Variant<T>>::update_archetype_component_access(&state.with, archetype, access)
	}

	fn init_state(world: &mut bevy_ecs::world::World) -> Self::State {
		T::init_state(world)
	}

	fn matches_component_set(
		&state: &Self::State,
		set_contains_id: &impl Fn(bevy_ecs::component::ComponentId) -> bool,
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

unsafe impl<T: EnumComponentVariant<State = EnumVariantIndex<WO>>, const WO: usize>
	ReadOnlyWorldQuery for ERef<'_, T>
{
}

pub struct EMut<'w, T: EnumComponentVariantMut>(&'w mut T);

unsafe impl<'v, T: EnumComponentVariantMut, const WO: usize> bevy_ecs::query::WorldQuery
	for EMut<'v, T>
where
	T: EnumComponentVariant<State = EnumVariantIndex<WO>>,
{
	type Item<'a> = bevy_ecs::world::Mut<'a, T>;
	type Fetch<'a> = <&'a mut Variant<T> as bevy_ecs::query::WorldQuery>::Fetch<'a>;
	type ReadOnly = ERef<'v, T>;
	type State = T::State;

	fn shrink<'wlong: 'wshort, 'wshort>(
		item: bevy_ecs::query::QueryItem<'wlong, Self>,
	) -> bevy_ecs::query::QueryItem<'wshort, Self> {
		item
	}

	unsafe fn init_fetch<'w>(
		world: &'w bevy_ecs::world::World,
		state: &Self::State,
		last_change_tick: u32,
		change_tick: u32,
	) -> Self::Fetch<'w> {
		<&mut Variant<T>>::init_fetch(world, &state.with, last_change_tick, change_tick)
	}

	unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
		<&'w mut Variant<T> as bevy_ecs::query::WorldQuery>::clone_fetch(fetch)
	}

	const IS_DENSE: bool = false; // TODO: Make configurable
	const IS_ARCHETYPAL: bool = true;

	unsafe fn set_archetype<'w>(
		fetch: &mut Self::Fetch<'w>,
		state: &Self::State,
		archetype: &'w bevy_ecs::archetype::Archetype,
		tables: &'w bevy_ecs::storage::Table,
	) {
		<&mut Variant<T>>::set_archetype(fetch, &state.with, archetype, tables)
	}

	unsafe fn set_table<'w>(
		fetch: &mut Self::Fetch<'w>,
		state: &Self::State,
		table: &'w bevy_ecs::storage::Table,
	) {
		<&mut Variant<T>>::set_table(fetch, &state.with, table)
	}

	unsafe fn fetch<'w>(
		fetch: &mut Self::Fetch<'w>,
		entity: bevy_ecs::entity::Entity,
		table_row: usize,
	) -> Self::Item<'w> {
		<&mut Variant<T>>::fetch(fetch, entity, table_row).map_unchanged(|it| &mut it.0)
	}

	fn update_component_access(
		state: &Self::State,
		access: &mut bevy_ecs::query::FilteredAccess<bevy_ecs::component::ComponentId>,
	) {
		state.update_component_access(access)
	}

	fn update_archetype_component_access(
		state: &Self::State,
		archetype: &bevy_ecs::archetype::Archetype,
		access: &mut bevy_ecs::query::Access<bevy_ecs::archetype::ArchetypeComponentId>,
	) {
		<&mut Variant<T>>::update_archetype_component_access(&state.with, archetype, access)
	}

	fn init_state(world: &mut bevy_ecs::world::World) -> Self::State {
		T::init_state(world)
	}

	fn matches_component_set(
		state: &Self::State,
		set_contains_id: &impl Fn(bevy_ecs::component::ComponentId) -> bool,
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

mod private {
	use super::{EnumComponent, EnumComponentVariant};
	use bevy_ecs::component::Component;

	#[derive(Component)]
	pub struct EnumTag<T: EnumComponent>(pub(crate) T::Tag);

	#[derive(Component)]
	pub struct Variant<T: EnumComponentVariant>(pub(crate) T);
}

use private::*;

#[derive(Bundle)]
pub struct EnumBundle<T: EnumComponentVariant> {
	tag: EnumTag<T::Enum>,
	value: Variant<T>,
}

#[cfg(test)]
pub mod tests {
	use bevy::app::{App, AppExit};
	use bevy::prelude::*;
	use sond_bevy_enum_components::*;
	use std::time::Duration;

	#[test]
	fn enum_component() {
		App::new()
			.add_plugins(MinimalPlugins)
			.add_startup_system(insert_foo_variant)
			.add_system(test_foo_variant)
			.add_system(change_foo)
			.add_system(incr_things::<1>)
			.run()
	}

	#[derive(Debug, PartialEq, Eq, EnumComponent)]
	#[component(mutable, derive(PartialEq, Eq))]
	pub enum Foo {
		Bar,
		Baz,
		Things { stuff: i32 },
	}

	use foo::*;

	pub fn insert_foo_variant(mut cmds: Commands) {
		cmds.spawn((
			Transform::default(),
			FooChangeTimer(Timer::from_seconds(0.5, TimerMode::Repeating)),
			IncrTimer(Timer::new(
				Duration::from_secs_f64(0.01),
				TimerMode::Repeating,
			)),
		))
		.set_enum(Foo::Bar);
	}

	pub fn test_foo_variant(
		foo_q: Query<Foo>,
		bar_q: Query<(&mut Transform, ERef<Bar>)>,
		baz_q: Query<(&mut Transform, ERef<Baz>)>,
		things_q: Query<(&mut Transform, ERef<Things>)>,
	) {
		let foo = foo_q.single();
		if let Ok((_, bar)) = bar_q.get_single() {
			assert_eq!(foo, FooItem::Bar(bar))
		} else if let Ok((_, baz)) = baz_q.get_single() {
			assert_eq!(foo, FooItem::Baz(baz))
		} else if let Ok((_, things)) = things_q.get_single() {
			assert_eq!(foo, FooItem::Things(things))
		} else {
			panic!("no components for {foo:?}")
		};
	}

	#[derive(Component, Debug, Default)]
	pub struct FooChangeTimer(Timer);

	pub fn change_foo(
		mut app_exit_events: EventWriter<AppExit>,
		mut cmds: Commands,
		mut q: Query<(Entity, Foo, &mut FooChangeTimer)>,
		t: Res<Time>,
	) {
		for (id, foo, mut timer) in &mut q {
			let mut cmds = cmds.entity(id);
			let finished = timer.0.tick(t.delta()).finished();
			match foo {
				FooItem::Bar(..) => {
					if finished {
						cmds.set_enum(Foo::Baz);
					}
				}
				FooItem::Baz(..) => {
					if finished {
						cmds.set_enum(Foo::Things { stuff: 0 });
					}
				}
				FooItem::Things(Things { stuff: 42 }) => {
					let t = t.elapsed();
					assert!(t > Duration::from_secs_f32(1.42), "{t:?}");
					app_exit_events.send(AppExit);
					cmds.set_enum(Foo::Bar);
				}
				_ => {
					// wait for incr_things
				}
			};
		}
	}

	#[derive(Component)]
	pub struct IncrTimer(Timer);
	pub fn incr_things<const N: i32>(mut q: Query<(EMut<Things>, &mut IncrTimer)>, t: Res<Time>) {
		for (mut things, mut timer) in &mut q {
			if timer.0.tick(t.delta()).finished() {
				things.stuff += N
			}
		}
	}
}
