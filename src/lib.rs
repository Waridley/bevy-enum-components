#[doc(hidden)]
/// Exported for derive macro usage.
pub use bevy_ecs;

use bevy_ecs::component::ComponentId;
use bevy_ecs::query::FilteredAccess;
use bevy_ecs::system::EntityCommands;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariantIndex<const N: usize> {
	pub with: ComponentId,
	pub without: [ComponentId; N],
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
	pub enum Foo {
		Bar,
		Baz,
		Things { stuff: i32 },
	}

	use foo::*;

	pub fn insert_foo_variant(mut cmds: Commands) {
		cmds.spawn((
			Transform::default(),
			FooChangeTimer(Timer::from_seconds(0.5, true)),
			IncrTimer(Timer::new(Duration::from_secs_f64(0.01), true)),
		))
		.set_enum(Foo::Bar);
	}

	pub fn test_foo_variant(
		foo_q: Query<Foo>,
		bar_q: Query<(&mut Transform, ReadFoo<Bar>)>,
		baz_q: Query<(&mut Transform, ReadFoo<Baz>)>,
		things_q: Query<(&mut Transform, ReadFoo<Things>)>,
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
					let t = t.time_since_startup();
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
	pub fn incr_things<const N: i32>(
		mut q: Query<(WriteFoo<Things>, &mut IncrTimer)>,
		t: Res<Time>,
	) {
		for (mut things, mut timer) in &mut q {
			if timer.0.tick(t.delta()).finished() {
				things.stuff += N
			}
		}
	}
}
