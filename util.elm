module Util exposing (bind, isJust, filterMaybe, maybeFromPredicate, takeUntil)

import Maybe


bind : (a -> Maybe b) -> Maybe a -> Maybe b
bind fn o =
    Maybe.andThen o fn


isJust : Maybe a -> Bool
isJust =
    Maybe.map (\_ -> True) >> Maybe.withDefault False


filterMaybe : List (Maybe a) -> List a
filterMaybe lst =
    List.filterMap identity lst


maybeFromPredicate : (a -> Bool) -> a -> Maybe a
maybeFromPredicate fn v =
    if fn v then
        Just v
    else
        Nothing


takeUntil : (a -> Bool) -> List a -> List a
takeUntil pred lst =
    takeUntilRec pred lst []


takeUntilRec : (a -> Bool) -> List a -> List a -> List a
takeUntilRec pred lst res =
    case lst of
        [] ->
            res

        hd :: tl ->
            if pred hd then
                List.reverse <| hd :: res
            else
                takeUntilRec pred tl (hd :: res)
