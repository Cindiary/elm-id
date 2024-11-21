module Internal exposing (ID(..), Dict(..), unpack)

import Dict

type ID a
    = ID Int

type Dict id value
    = IDDict ( Dict.Dict Int value )


unpack : ID a -> Int
unpack (ID id) =
    id