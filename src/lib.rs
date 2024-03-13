#[doc(hidden)]
/// Exported for derive macro usage.
pub use bevy_ecs;
use std::marker::PhantomData;

use bevy_ecs::world::EntityWorldMut;
use bevy_ecs::{
	component::{ComponentId, ComponentStorage, StorageType, Tick},
	entity::Entity,
	prelude::World,
	query::{QueryData, QueryFilter, ReadOnlyQueryData, Without, WorldQuery},
	storage::TableRow,
	system::EntityCommands,
	world::unsafe_world_cell::UnsafeWorldCell,
};

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariantIndex<const WO: usize> {
	pub with: ComponentId,
	pub without: [ComponentId; WO],
}

impl<const WO: usize> EnumVariantIndex<WO> {
	fn matches_component_set(&self, set_contains_id: &impl Fn(ComponentId) -> bool) -> bool {
		if !set_contains_id(self.with) {
			return false;
		}
		for id in self.without {
			if set_contains_id(id) {
				return false;
			}
		}
		true
	}
}

pub use macros::EnumComponent;
pub trait EnumComponent {
	type Tag: Copy + Send + Sync + 'static;

	fn tag(&self) -> Self::Tag;
}

pub trait EntityEnumCommands {
	fn set_enum<E: EnumComponentVariant>(&mut self, value: E) -> &mut Self;
	fn with_enum<E: EnumComponentVariant>(mut self, value: E) -> Self
	where
		Self: Sized,
	{
		self.set_enum(value);
		self
	}
	fn remove_variant<E: EnumComponentVariant>(&mut self) -> &mut Self;
}

impl<'a> EntityEnumCommands for EntityCommands<'a> {
	fn set_enum<E: EnumComponentVariant>(&mut self, value: E) -> &mut Self {
		value.dispatch_to(self);
		self
	}

	fn remove_variant<E: EnumComponentVariant>(&mut self) -> &mut Self {
		E::remove_from(self);
		self
	}
}

pub trait EntityWorldEnumMut {
	fn set_enum<E: EnumComponentVariant>(&mut self, value: E) -> &mut Self;
	fn with_enum<E: EnumComponentVariant>(mut self, value: E) -> Self
	where
		Self: Sized,
	{
		self.set_enum(value);
		self
	}
	fn remove_variant<E: EnumComponentVariant>(&mut self) -> &mut Self;
}

impl EntityWorldEnumMut for EntityWorldMut<'_> {
	fn set_enum<E: EnumComponentVariant>(&mut self, value: E) -> &mut Self {
		value.dispatch_to_world(self);
		self
	}

	fn remove_variant<E: EnumComponentVariant>(&mut self) -> &mut Self {
		E::remove_from_world(self);
		self
	}
}

pub trait EnumComponentVariant: Send + Sync + 'static {
	type Enum: EnumComponent;
	type State: Send + Sync + 'static;
	type Storage: ComponentStorage;

	fn tag() -> <Self::Enum as EnumComponent>::Tag;
	fn init_state(world: &mut bevy_ecs::world::World) -> Self::State;
	fn get_state(world: &bevy_ecs::world::World) -> Option<Self::State>;
	fn init_component(world: &mut bevy_ecs::world::World) -> ComponentId
	where
		Self: Sized,
	{
		world.init_component::<Variant<Self>>()
	}
	fn get_component(world: &bevy_ecs::world::World) -> Option<ComponentId>
	where
		Self: Sized,
	{
		world.component_id::<Variant<Self>>()
	}
	fn dispatch_to(self, cmds: &mut EntityCommands)
	where
		Self: Sized,
	{
		cmds.add(|id, world: &mut World| self.dispatch_to_world(&mut world.entity_mut(id)));
	}
	fn remove_from(cmds: &mut EntityCommands) {
		cmds.add(|id, world: &mut World| Self::remove_from_world(&mut world.entity_mut(id)));
	}
	fn dispatch_to_world(self, world: &mut EntityWorldMut);
	fn remove_from_world(world: &mut EntityWorldMut);
}

pub trait EnumComponentVariantMut: EnumComponentVariant {}

#[derive(Debug)]
pub struct ERef<'w, T: EnumComponentVariant>(PhantomData<&'w T>);

unsafe impl<T: EnumComponentVariant<State = EnumVariantIndex<WO>>, const WO: usize> QueryData
	for ERef<'_, T>
{
	type ReadOnly = Self;
}

unsafe impl<T: EnumComponentVariant<State = EnumVariantIndex<WO>>, const WO: usize> WorldQuery
	for ERef<'_, T>
{
	type Item<'a> = &'a T;
	type Fetch<'a> = <&'a Variant<T> as WorldQuery>::Fetch<'a>;
	type State = T::State;

	fn shrink<'wlong: 'wshort, 'wshort>(
		item: bevy_ecs::query::QueryItem<'wlong, Self>,
	) -> bevy_ecs::query::QueryItem<'wshort, Self> {
		item
	}

	unsafe fn init_fetch<'w>(
		world: UnsafeWorldCell<'w>,
		state: &Self::State,
		last_change_tick: Tick,
		change_tick: Tick,
	) -> Self::Fetch<'w> {
		<&Variant<T>>::init_fetch(world, &state.with, last_change_tick, change_tick)
	}

	const IS_DENSE: bool = match T::Storage::STORAGE_TYPE {
		StorageType::Table => true,
		StorageType::SparseSet => false,
	};

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
		table_row: TableRow,
	) -> Self::Item<'w> {
		&<&Variant<T>>::fetch(fetch, entity, table_row).0
	}

	fn update_component_access(
		state: &Self::State,
		access: &mut bevy_ecs::query::FilteredAccess<bevy_ecs::component::ComponentId>,
	) {
		access.add_read(state.with);
		for id in state.without {
			access.and_without(id)
		}
	}

	fn init_state(world: &mut bevy_ecs::world::World) -> Self::State {
		<T as EnumComponentVariant>::init_state(world)
	}

	fn get_state(world: &World) -> Option<Self::State> {
		<T as EnumComponentVariant>::get_state(world)
	}

	fn matches_component_set(
		&state: &Self::State,
		set_contains_id: &impl Fn(bevy_ecs::component::ComponentId) -> bool,
	) -> bool {
		state.matches_component_set(set_contains_id)
	}
}

// SAFETY: All access defers to `&Variant`
unsafe impl<T: EnumComponentVariant<State = EnumVariantIndex<WO>>, const WO: usize>
	ReadOnlyQueryData for ERef<'_, T>
{
}

#[derive(Debug)]
pub struct EMut<'w, T: EnumComponentVariantMut>(PhantomData<&'w mut T>);

unsafe impl<'v, T: EnumComponentVariantMut<State = EnumVariantIndex<WO>>, const WO: usize> QueryData
	for EMut<'v, T>
{
	type ReadOnly = ERef<'v, T>;
}

unsafe impl<'v, T: EnumComponentVariantMut, const WO: usize> WorldQuery for EMut<'v, T>
where
	T: EnumComponentVariant<State = EnumVariantIndex<WO>>,
{
	type Item<'a> = bevy_ecs::world::Mut<'a, T>;
	type Fetch<'a> = <&'a mut Variant<T> as bevy_ecs::query::WorldQuery>::Fetch<'a>;
	type State = T::State;

	fn shrink<'wlong: 'wshort, 'wshort>(
		item: bevy_ecs::query::QueryItem<'wlong, Self>,
	) -> bevy_ecs::query::QueryItem<'wshort, Self> {
		item
	}

	unsafe fn init_fetch<'w>(
		world: UnsafeWorldCell<'w>,
		state: &Self::State,
		last_change_tick: Tick,
		change_tick: Tick,
	) -> Self::Fetch<'w> {
		<&mut Variant<T>>::init_fetch(world, &state.with, last_change_tick, change_tick)
	}

	const IS_DENSE: bool = match T::Storage::STORAGE_TYPE {
		StorageType::Table => true,
		StorageType::SparseSet => false,
	};

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
		table_row: TableRow,
	) -> Self::Item<'w> {
		<&mut Variant<T>>::fetch(fetch, entity, table_row).map_unchanged(|it| &mut it.0)
	}

	fn update_component_access(
		state: &Self::State,
		access: &mut bevy_ecs::query::FilteredAccess<bevy_ecs::component::ComponentId>,
	) {
		access.add_write(state.with);
		for id in state.without {
			access.and_without(id)
		}
	}

	fn init_state(world: &mut bevy_ecs::world::World) -> Self::State {
		<T as EnumComponentVariant>::init_state(world)
	}

	fn get_state(world: &bevy_ecs::world::World) -> Option<Self::State> {
		<T as EnumComponentVariant>::get_state(world)
	}

	fn matches_component_set(
		state: &Self::State,
		set_contains_id: &impl Fn(bevy_ecs::component::ComponentId) -> bool,
	) -> bool {
		state.matches_component_set(set_contains_id)
	}
}

pub struct WithVariant<T: EnumComponentVariant>(PhantomData<T>);

impl<T: EnumComponentVariant<State = EnumVariantIndex<WO>>, const WO: usize> QueryFilter
	for WithVariant<T>
{
	const IS_ARCHETYPAL: bool = true;

	unsafe fn filter_fetch(
		_fetch: &mut Self::Fetch<'_>,
		_entity: Entity,
		_table_row: TableRow,
	) -> bool {
		true
	}
}

unsafe impl<T: EnumComponentVariant<State = EnumVariantIndex<WO>>, const WO: usize> WorldQuery
	for WithVariant<T>
{
	type Item<'a> = ();
	type Fetch<'a> = ();
	type State = T::State;

	fn shrink<'wlong: 'wshort, 'wshort>(
		_item: bevy_ecs::query::QueryItem<'wlong, Self>,
	) -> bevy_ecs::query::QueryItem<'wshort, Self> {
	}

	#[inline]
	unsafe fn init_fetch<'w>(
		_world: UnsafeWorldCell<'w>,
		_state: &Self::State,
		_last_change_tick: Tick,
		_change_tick: Tick,
	) -> Self::Fetch<'w> {
	}

	const IS_DENSE: bool = match T::Storage::STORAGE_TYPE {
		StorageType::Table => true,
		StorageType::SparseSet => false,
	};

	unsafe fn set_archetype<'w>(
		_fetch: &mut Self::Fetch<'w>,
		_state: &Self::State,
		_archetype: &'w bevy_ecs::archetype::Archetype,
		_tables: &'w bevy_ecs::storage::Table,
	) {
	}

	unsafe fn set_table<'w>(
		_fetch: &mut Self::Fetch<'w>,
		_state: &Self::State,
		_table: &'w bevy_ecs::storage::Table,
	) {
	}

	unsafe fn fetch<'w>(
		_fetch: &mut Self::Fetch<'w>,
		_entity: bevy_ecs::entity::Entity,
		_table_row: TableRow,
	) -> Self::Item<'w> {
	}

	fn update_component_access(
		state: &Self::State,
		access: &mut bevy_ecs::query::FilteredAccess<bevy_ecs::component::ComponentId>,
	) {
		access.add_read(state.with);
		for id in state.without {
			access.and_without(id)
		}
	}

	fn init_state(world: &mut bevy_ecs::world::World) -> Self::State {
		<T as EnumComponentVariant>::init_state(world)
	}

	fn get_state(world: &World) -> Option<Self::State> {
		<T as EnumComponentVariant>::get_state(world)
	}

	fn matches_component_set(
		&state: &Self::State,
		set_contains_id: &impl Fn(bevy_ecs::component::ComponentId) -> bool,
	) -> bool {
		state.matches_component_set(set_contains_id)
	}
}

pub type WithoutVariant<T> = Without<Variant<T>>;

mod private {
	use super::EnumComponentVariant;
	use bevy_ecs::component::Component;

	#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
	pub struct Variant<T: EnumComponentVariant>(pub(crate) T);

	impl<T: EnumComponentVariant> Component for Variant<T> {
		type Storage = <T as EnumComponentVariant>::Storage;
	}
}

use private::*;

#[doc(hidden)]
pub fn remove_variant<V: EnumComponentVariant>(cmds: &mut EntityCommands) {
	cmds.remove::<Variant<V>>();
}

#[doc(hidden)]
pub fn world_remove_variant<V: EnumComponentVariant>(world: &mut EntityWorldMut) {
	world.remove::<Variant<V>>();
}

/// SAFETY: All other variants of the enum must already be removed.
#[doc(hidden)]
pub unsafe fn insert_variant<V: EnumComponentVariant>(cmds: &mut EntityCommands, value: V) {
	cmds.insert(Variant(value));
}

/// SAFETY: All other variants of the enum must already be removed.
#[doc(hidden)]
pub unsafe fn world_insert_variant<V: EnumComponentVariant>(world: &mut EntityWorldMut, value: V) {
	world.insert(Variant(value));
}

#[cfg(test)]
pub mod tests {
	use bevy::{app::AppExit, prelude::*};
	use sond_bevy_enum_components::*;
	use std::time::Duration;

	#[test]
	fn enum_component() {
		App::new()
			.add_plugins(MinimalPlugins)
			.add_systems(Startup, insert_foo_variant)
			.add_systems(Update, (test_foo_variant, change_foo, incr_things::<1>))
			.run()
	}

	#[derive(Debug, PartialEq, Eq, EnumComponent)]
	#[component(mutable, derive(Debug, Clone, PartialEq, Eq))]
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
		.set_enum(Bar);
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
						cmds.set_enum(Baz);
					}
				}
				FooItem::Baz(..) => {
					if finished {
						cmds.set_enum(Things { stuff: 0 });
					}
				}
				FooItem::Things(Things { stuff: 42 }) => {
					let t = t.elapsed();
					assert!(t > Duration::from_secs_f32(1.42), "{t:?}");
					app_exit_events.send(AppExit);
					cmds.set_enum(Bar);
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
