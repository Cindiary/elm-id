Elm ID is a library for making opaque ID types that can be used in dictionaries.

## Example

```elm
module User exposing (User, ID, newCounter, newUser)

import ID
import ID.Counter

type alias ID =
    ID.ID IDInternal

type IDInternal
    = IDInternal

type alias User =
    { id : ID
    , name : String
    }


newCounter : ID.Counter ID
newCounter =
    ID.Counter.new IDInternal


newUser : ID.Counter -> String -> ( ID.Counter, User )
newUser counter name =
    let
        ( id, newCounter ) = ID.Counter.newID counter
    in
    ( newCounter, { id = id, name = name } )

```
