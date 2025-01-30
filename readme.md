Elm Id is a library for making opaque Id types that can be used in dictionaries.

## Example

```elm
module User exposing (User, Id, newCounter, newUser)

import Id
import Id.Counter

type alias Id =
    Id.Id IdInternal

type IdInternal
    = IdInternal

type alias User =
    { id : Id
    , name : String
    }


newCounter : Id.Counter Id
newCounter =
    Id.Counter.new IdInternal


newUser : Id.Counter -> String -> ( Id.Counter, User )
newUser counter name =
    let
        ( id, newCounter ) = Id.Counter.newId counter
    in
    ( newCounter, { id = id, name = name } )

```
