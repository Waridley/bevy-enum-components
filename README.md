# bevy-enum-components
Mutually-exclusive Bevy components derived from enums.

Each variant is stored as a separate component, but the current variant can only be changed via `EntityCommands`.
Structs are generated for querying for particular variants, eliminating the need to match on the enum if a system
should only run for entities with a particular variant.

# Usage
see the [Avatar example](examples/avatar.rs)
