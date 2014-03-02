module Pull where

import Http
import String

{-| Pull the contents of a file of the format "message\nuniqueID". The message
is propagated whenever the uniqueID changes.

Example sender client: `printf "message\n"\`head -c 8 /dev/random | base64\` > pull.txt`
-}

pull : Signal (Maybe String)
pull = let pulled = Http.sendGet (always "pull.txt" <~ fps 0.5)
           extract resp = case resp of
               Http.Success s -> Just <| head <| String.lines s
               _ -> Nothing
       in extract <~ dropRepeats pulled

main = asText <~ pull
