use bevy::app::AppExit;
use bevy::prelude::*;
use sond_bevy_enum_components::*;

/// Bundles for each Avatar
const AVATARS: &[(&str, Nation)] = &[
	("Aang", Nation::Air),
	("Korra", Nation::Water(WaterTribe::Southern)),
	(
		"Kyoshi",
		Nation::Earth {
			residence: "Kyoshi Island",
		},
	),
	("Roku", Nation::Fire),
];

/// Bundles for each non-Avatar bender
const BENDERS: &[(&str, Nation)] = &[
	("Katara", Nation::Water(WaterTribe::Southern)),
	(
		"Toph",
		Nation::Earth {
			residence: "Foggy Swamp",
		},
	),
	("Zuko", Nation::Fire),
];

fn main() {
	use element::*;
	App::new()
		.add_plugins(MinimalPlugins)
		.add_startup_system(setup)
		.add_system(switch_elements)
		.add_system(attack::<Air>)
		.add_system(attack::<Water>)
		.add_system(attack::<Earth>)
		.add_system(attack::<Fire>)
		.add_system(check_avatar)
		.run()
}

#[derive(Debug, Deref, DerefMut, Component)]
struct Name(&'static str);

fn setup(mut cmds: Commands) {
	for (name, nation) in AVATARS {
		cmds.spawn((
			Name(*name),
			MasteredElements::default(),
			DiscoveredAsAvatar::default(),
			Avatar::default(),
		))
		.set_enum(nation.clone())
		.set_enum(Element::from(nation));
	}
	for (name, nation) in BENDERS {
		cmds.spawn((
			Name(*name),
			MasteredElements::default(),
			DiscoveredAsAvatar::default(),
		))
		.set_enum(nation.clone())
		.set_enum(Element::from(nation));
	}
}

/// Nation of origin for a bender
#[derive(Debug, Clone, EnumComponent)]
pub enum Nation {
	Air,
	Water(WaterTribe),
	Earth { residence: &'static str },
	Fire,
}
use nation::*;

/// Which tribe of the Water nation a bender is from
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WaterTribe {
	Northern,
	Southern,
}

/// The 4 elements
#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumComponent)]
pub enum Element {
	Air,
	Water,
	Earth,
	Fire,
}
use element::*;

/// How each element is used as an attack
trait Attack {
	fn attack_sound() -> &'static str;
}

impl Attack for element::Air {
	fn attack_sound() -> &'static str {
		"Woosh!"
	}
}

impl Attack for element::Water {
	fn attack_sound() -> &'static str {
		"Splash!"
	}
}

impl Attack for element::Earth {
	fn attack_sound() -> &'static str {
		"Crunch!"
	}
}

impl Attack for element::Fire {
	fn attack_sound() -> &'static str {
		"Fwoomph!"
	}
}

impl PartialEq<NationType> for ElementType {
	fn eq(&self, nation: &NationType) -> bool {
		match nation {
			NationType::Air => *self == ElementType::Air,
			NationType::Water => *self == ElementType::Water,
			NationType::Earth => *self == ElementType::Earth,
			NationType::Fire => *self == ElementType::Fire,
		}
	}
}

impl From<&Nation> for Element {
	fn from(nation: &Nation) -> Self {
		match nation.nation_type() {
			NationType::Air => Element::Air,
			NationType::Water => Element::Water,
			NationType::Earth => Element::Earth,
			NationType::Fire => Element::Fire,
		}
	}
}

/// Whether this bender actually is the Avatar, even if not yet discovered
#[derive(Default, Component)]
struct Avatar;

/// Whether this bender has yet been discovered to be the Avatar.
#[derive(Default, Component)]
struct DiscoveredAsAvatar(bool);

/// Cycle through elements to bend for each Avatar
fn switch_elements(mut cmds: Commands, mut q: Query<(Entity, Element), With<Avatar>>) {
	use ElementType::*;
	for (id, element) in &mut q {
		let next_element = match element.element_type() {
			Air => Element::Water,
			Water => Element::Earth,
			Earth => Element::Fire,
			Fire => Element::Air,
		};
		cmds.entity(id).set_enum(next_element);
	}
}

/// Make all current benders of element E perform an attack
fn attack<E: Attack + EnumComponentVariant<Enum = Element, State = EnumVariantIndex<3>>>(
	mut q: Query<(&Name, Nation, &mut DiscoveredAsAvatar), ERef<E>>,
) {
	use nation::NationItem::*;
	for (Name(name), nation, mut discovered) in &mut q {
		println!("{name}: {}", E::attack_sound());
		let origin = match nation {
			Air(..) => "the Air Nation".into(),
			Water(nation::Water(tribe)) => format!("the {tribe:?} Water Tribe"),
			Earth(nation::Earth { residence }) => {
				format!("{residence}")
			}
			Fire(..) => "the Fire Nation".into(),
		};
		if !discovered.0 && E::tag() != NationType::from(&nation) {
			discovered.0 = true;
			println!("Whoa! Isn't {name} from {origin}?! They must be the Avatar!");
		}
	}
}

/// Set of currently mastered elements for each bender. Only one should be true for non-Avatars.
#[derive(Debug, Default, Copy, Clone, Component)]
pub struct MasteredElements {
	pub air: bool,
	pub water: bool,
	pub earth: bool,
	pub fire: bool,
}

/// Check if bender has mastered more than 1 element, revealing them as the Avatar.
fn check_avatar(
	mut q: Query<(&Name, Element, &mut MasteredElements)>,
	mut exit: EventWriter<AppExit>,
) {
	use ElementType::*;
	for (Name(name), curr_elem, mut mastered) in &mut q {
		let MasteredElements {
			air,
			water,
			earth,
			fire,
		} = &mut *mastered;
		match curr_elem.element_type() {
			Air => *air = true,
			Water => *water = true,
			Earth => *earth = true,
			Fire => *fire = true,
		}
		if *air && *water && *earth && *fire {
			println!("Congratulations! {name} mastered all 4 elements!");
			exit.send(AppExit)
		}
	}
}
