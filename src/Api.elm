port module Api exposing (createMatchups, scoreMatchup, onMatchupsUpdated)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, Value)

port create : Encode.Value -> Cmd msg
port score : Encode.Value -> Cmd msg

createMatchups : List a -> Cmd msg
createMatchups people =
    create (Encode.int (List.length people))


scoreMatchup : String -> Int -> Cmd msg
scoreMatchup matchId winner =
    score (Encode.object
        [ ("winnerId", Encode.int winner)
        , ("matchId", Encode.string matchId)
        ])

port onMatchupsUpdated : (Value -> msg) -> Sub msg
