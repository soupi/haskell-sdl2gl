- id
- type
- state
- functionality
- position
- movement
- graphics

all components (functionality, position, movement, graphics) can be of type Maybe,
we can use Applicative to operate on components.
also, instead of putting all components in one object we can create maps of

- positions :: Map Id Position
- movements :: Map Id Movement
- graphics  :: Map Id Graphics

etc.


http://fho.f12n.de/posts/2014-10-25-easily-extensible-entity-enigma.html

