use bevy::app::AppExit;
use bevy::prelude::*;
use sond_bevy_enum_components::*;

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
struct Name(String);

fn setup(mut cmds: Commands) {
	for (name, nation) in AVATARS {
		cmds.spawn((Name((&**name).into()), MasteredElements::default()))
			.set_enum(nation.clone())
			.set_enum(Element::from(nation));
	}
}

#[derive(Debug, Clone, EnumComponent)]
pub enum Nation {
	Air,
	Water(WaterTribe),
	Earth { residence: &'static str },
	Fire,
}
use nation::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WaterTribe {
	Northern,
	Southern,
}

#[derive(Debug, Clone, EnumComponent)]
pub enum Element {
	Air,
	Water,
	Earth,
	Fire,
}

trait Attack: element::ElementVariant<State = EnumVariantIndex<3>> {
	fn attack_sound() -> &'static str;
	fn matches_nation(nation: &NationItem) -> bool;
}

impl Attack for element::Air {
	fn attack_sound() -> &'static str {
		"Woosh!"
	}
	fn matches_nation(nation: &NationItem) -> bool {
		matches!(nation, NationItem::Air(_))
	}
}

impl Attack for element::Water {
	fn attack_sound() -> &'static str {
		"Splash!"
	}
	fn matches_nation(nation: &NationItem) -> bool {
		matches!(nation, NationItem::Water(_))
	}
}

impl Attack for element::Earth {
	fn attack_sound() -> &'static str {
		"Crunch!"
	}
	fn matches_nation(nation: &NationItem) -> bool {
		matches!(nation, NationItem::Earth(_))
	}
}

impl Attack for element::Fire {
	fn attack_sound() -> &'static str {
		"Fwoomph!"
	}
	fn matches_nation(nation: &NationItem) -> bool {
		matches!(nation, NationItem::Fire(_))
	}
}

impl From<&Nation> for Element {
	fn from(nation: &Nation) -> Self {
		match nation {
			Nation::Air => Element::Air,
			Nation::Water(_) => Element::Water,
			Nation::Earth { .. } => Element::Earth,
			Nation::Fire => Element::Fire,
		}
	}
}

fn switch_elements(mut cmds: Commands, mut q: Query<(Entity, Element)>) {
	use element::ElementItem::*;
	for (id, element) in &mut q {
		let next_element = match element {
			Air(_) => Element::Water,
			Water(_) => Element::Earth,
			Earth(_) => Element::Fire,
			Fire(_) => Element::Air,
		};
		cmds.entity(id).set_enum(next_element);
	}
}

fn attack<E: Attack>(q: Query<(&Name, Nation), element::ReadElement<E>>) {
	use nation::NationItem::*;
	for (Name(name), nation) in &q {
		println!("{}", E::attack_sound());
		let origin = match nation {
			Air(_) => "the Air Nation".into(),
			Water(nation::Water(tribe)) => format!("the {tribe:?} Water Tribe"),
			Earth(nation::Earth { residence }) => {
				format!("{residence}")
			}
			Fire(_) => "the Fire Nation".into(),
		};
		if !E::matches_nation(&nation) {
			println!("Whoa! Isn't {name} from {origin}?! They must be the Avatar!");
		}
	}
}

#[derive(Debug, Default, Copy, Clone, Component)]
pub struct MasteredElements {
	air: bool,
	water: bool,
	earth: bool,
	fire: bool,
}

fn check_avatar(
	mut q: Query<(&Name, Element, &mut MasteredElements)>,
	mut exit: EventWriter<AppExit>,
) {
	use element::ElementItem::*;
	for (Name(name), curr_elem, mut mastered) in &mut q {
		let MasteredElements {
			air,
			water,
			earth,
			fire,
		} = &mut *mastered;
		match curr_elem {
			Air(_) => *air = true,
			Water(_) => *water = true,
			Earth(_) => *earth = true,
			Fire(_) => *fire = true,
		}
		if *air && *water && *earth && *fire {
			println!("Congratulations! {name} mastered all 4 elements!");
			exit.send(AppExit)
		}
	}
}
