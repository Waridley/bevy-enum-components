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
	App::new()
		.add_plugins(MinimalPlugins)
		.add_startup_system(setup)
		.add_system(switch_elements)
		.add_system(attack)
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

#[derive(Debug, Clone, PartialEq, Eq, EnumComponent)]
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

#[derive(Debug, Clone, PartialEq, Eq, EnumComponent)]
pub enum Element {
	Air,
	Water,
	Earth,
	Fire,
}

use element::*;

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
	use ElementItem::*;
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

fn attack(q: Query<(&Name, Element, Nation)>) {
	for (Name(name), element, nation) in &q {
		let origin = match nation {
			NationItem::Air(_) => "the Air Nation".into(),
			NationItem::Water(nation::Water(tribe)) => format!("the {tribe:?} Water Tribe"),
			NationItem::Earth(nation::Earth { residence }) => {
				format!("{residence}")
			}
			NationItem::Fire(_) => "the Fire Nation".into(),
		};
		let mut elem_matches_nation = false;
		match element {
			ElementItem::Air(_) => {
				println!("Woosh!");
				if let NationItem::Air(_) = nation {
					elem_matches_nation = true;
				}
			}
			ElementItem::Water(_) => {
				println!("Splash!");
				if let NationItem::Water(tribe) = nation {
					elem_matches_nation = true
				}
			}
			ElementItem::Earth(_) => {
				println!("Crunch!");
				if let NationItem::Earth(_) = nation {
					elem_matches_nation = true
				}
			}
			ElementItem::Fire(_) => {
				println!("Fwoosh!");
				if let NationItem::Fire(_) = nation {
					elem_matches_nation = true
				}
			}
		}
		if !elem_matches_nation {
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
	use ElementItem::*;
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
