module List.Nonempty exposing
    ( Nonempty(..)
    , singleton, fromList
    , head, tail, toList, get, last, take, sample
    , isSingleton, length, member, all, any
    , cons, append, pop, reverse, concat
    , replaceHead, replaceTail, dropTail
    , map, indexedMap, map2, andMap, concatMap
    , filter
    , foldl, foldl1
    , zip, unzip
    , sort, sortBy, sortWith
    , dedup, uniq
    , consumeWith
    , fromElement
    )

{-| A list that cannot be empty. The head and tail can be accessed without Maybes. Most other list functions are
available.


# Definition

@docs Nonempty


# Create

@docs singleton, fromList


# Access

@docs head, tail, toList, get, last, take, sample


# Inspect

Nonempty lists support equality with the usual `(==)` operator (provided their contents also support equality).

@docs isSingleton, length, member, all, any


# Convert

@docs cons, append, pop, reverse, concat


# Swap

@docs replaceHead, replaceTail, dropTail


# Map

@docs map, indexedMap, map2, andMap, concatMap


# Filter

@docs filter


# Fold

To fold or scan from the right, reverse the list first.

@docs foldl, foldl1


# Zipping

@docs zip, unzip


# Sort

@docs sort, sortBy, sortWith


# Deduplicate

The nonempty list's elements must support equality (e.g. not functions). Otherwise you will get a runtime error.

@docs dedup, uniq


# Consumption

@docs consumeWith


# Deprecated

@docs fromElement

-}

import Random


{-| The Nonempty type. If you have both a head and tail, you can construct a
nonempty list directly. Otherwise use the helpers below instead.
-}
type Nonempty a
    = Nonempty a (List a)


{-| Create a singleton list with the given element.
-}
singleton : a -> Nonempty a
singleton x =
    Nonempty x []


{-| Create a singleton list with the given element. Deprecated in favor of `singleton`, which is what elm-lang/core uses.
-}
fromElement : a -> Nonempty a
fromElement =
    singleton


{-| Create a nonempty list from an ordinary list, failing on the empty list.
-}
fromList : List a -> Maybe (Nonempty a)
fromList ys =
    case ys of
        x :: xs ->
            Just (Nonempty x xs)

        _ ->
            Nothing


{-| Return the head of the list.
-}
head : Nonempty a -> a
head (Nonempty x xs) =
    x


{-| Return the tail of the list.
-}
tail : Nonempty a -> List a
tail (Nonempty x xs) =
    xs


{-| Convert to an ordinary list.
-}
toList : Nonempty a -> List a
toList (Nonempty x xs) =
    x :: xs


{-| Get the item at the specified index. The head has index 0. Indices are modulused by the length so out-of-range
errors can't happen. This means that negative indices are supported, e.g. -1 to get the last element.
-}
get : Int -> Nonempty a -> a
get i ((Nonempty x xs) as ne) =
    let
        j =
            modBy (length ne) i

        find k ys =
            case ys of
                [] ->
                    {- This should never happen, but to avoid Debug.crash,
                       we return the head of the list.
                    -}
                    x

                z :: zs ->
                    if k == 0 then
                        z

                    else
                        find (k - 1) zs
    in
    if j == 0 then
        x

    else
        find (j - 1) xs


{-| Return the last element of the list.
-}
last : Nonempty a -> a
last (Nonempty x xs) =
    case xs of
        [] ->
            x

        [ y ] ->
            y

        _ :: rest ->
            last (Nonempty x rest)


{-| Create a nonempty list consisting of the first n elements. If n < 1, returns a nonempty list consiting of just the first element

    take 2 (Nonempty 1 [ 2, 3 ]) --> Nonempty 1 [ 2 ]

    take 0 (Nonempty 1 [ 2, 3 ]) --> fromElement 1

    take -3 (Nonempty 1 [ 2, 3 ]) --> fromElement 1

-}
take : Int -> Nonempty a -> Nonempty a
take n (Nonempty x xs) =
    Nonempty x (List.take (n - 1) xs)


{-| Create a random generator that returns a value of the nonempty list chosen uniformly at random.
-}
sample : Nonempty a -> Random.Generator a
sample nonempty =
    Random.int 0 (length nonempty - 1)
        |> Random.map (\i -> get i nonempty)


{-| Add another element as the head of the list, pushing the previous head to the tail.
-}
cons : a -> Nonempty a -> Nonempty a
cons y (Nonempty x xs) =
    Nonempty y (x :: xs)


{-| Append two nonempty lists together. `(++)` is _not_ supported.
-}
append : Nonempty a -> Nonempty a -> Nonempty a
append (Nonempty x xs) (Nonempty y ys) =
    Nonempty x (xs ++ y :: ys)


{-| Pop and discard the head, or do nothing for a singleton list. Useful if you
want to exhaust a list but hang on to the last item indefinitely.

    pop (Nonempty 3 [ 2, 1 ]) --> Nonempty 2 [1]

    pop (Nonempty 1 []) --> Nonempty 1 []

-}
pop : Nonempty a -> Nonempty a
pop (Nonempty x xs) =
    case xs of
        [] ->
            Nonempty x xs

        y :: ys ->
            Nonempty y ys


{-| Reverse a nonempty list.
-}
reverse : Nonempty a -> Nonempty a
reverse (Nonempty x xs) =
    let
        revapp : ( List a, a, List a ) -> Nonempty a
        revapp ( ls, c, rs ) =
            case rs of
                [] ->
                    Nonempty c ls

                r :: rss ->
                    revapp ( c :: ls, r, rss )
    in
    revapp ( [], x, xs )


{-| Flatten a nonempty list of nonempty lists into a single nonempty list.
-}
concat : Nonempty (Nonempty a) -> Nonempty a
concat (Nonempty xs xss) =
    let
        hd =
            head xs

        tl =
            tail xs ++ List.concat (List.map toList xss)
    in
    Nonempty hd tl


{-| Exchange the head element while leaving the tail alone.
-}
replaceHead : a -> Nonempty a -> Nonempty a
replaceHead y (Nonempty x xs) =
    Nonempty y xs


{-| Exchange the tail for another while leaving the head alone.
-}
replaceTail : List a -> Nonempty a -> Nonempty a
replaceTail ys (Nonempty x xs) =
    Nonempty x ys


{-| Replace the tail with the empty list while leaving the head alone.
-}
dropTail : Nonempty a -> Nonempty a
dropTail (Nonempty x xs) =
    Nonempty x []


{-| Map a function over a nonempty list.
-}
map : (a -> b) -> Nonempty a -> Nonempty b
map f (Nonempty x xs) =
    Nonempty (f x) (List.map f xs)


{-| Map a function over two nonempty lists.
-}
map2 : (a -> b -> c) -> Nonempty a -> Nonempty b -> Nonempty c
map2 f (Nonempty x xs) (Nonempty y ys) =
    Nonempty (f x y) (List.map2 f xs ys)


{-| Map over an arbitrary number of nonempty lists.

    map2 (,) xs ys == map (,) xs |> andMap ys

    head (map (,,) xs |> andMap ys |> andMap zs) == ( head xs, head ys, head zs )

-}
andMap : Nonempty a -> Nonempty (a -> b) -> Nonempty b
andMap =
    map2 (|>)


{-| Map a given function onto a nonempty list and flatten the resulting nonempty lists. If you're chaining, you can
define `andThen = flip concatMap`.
-}
concatMap : (a -> Nonempty b) -> Nonempty a -> Nonempty b
concatMap f xs =
    concat (map f xs)


{-| Same as `map` but the function is also applied to the index of each element (starting at zero).
-}
indexedMap : (Int -> a -> b) -> Nonempty a -> Nonempty b
indexedMap f (Nonempty x xs) =
    let
        wrapped i d =
            f (i + 1) d
    in
    Nonempty (f 0 x) (List.indexedMap wrapped xs)


{-| Determine if the nonempty list has exactly one element.
-}
isSingleton : Nonempty a -> Bool
isSingleton (Nonempty x xs) =
    List.isEmpty xs


{-| Find the length of the nonempty list.
-}
length : Nonempty a -> Int
length (Nonempty x xs) =
    List.length xs + 1


{-| Determine if an element is present in the nonempty list.
-}
member : a -> Nonempty a -> Bool
member y (Nonempty x xs) =
    x == y || List.member y xs


{-| Determine if all elements satisfy the predicate.
-}
all : (a -> Bool) -> Nonempty a -> Bool
all f (Nonempty x xs) =
    f x && List.all f xs


{-| Determine if any elements satisfy the predicate.
-}
any : (a -> Bool) -> Nonempty a -> Bool
any f (Nonempty x xs) =
    f x || List.any f xs


{-| Filter a nonempty list. If all values are filtered out, return the singleton list containing the default value
provided. If any value is retained, the default value is not used. If you want to deal with a Maybe instead, use
`toList >> List.filter yourPredicate >> fromList`.

    isEven : Int -> Bool
    isEven n = modBy 2 n == 0

    filter isEven 0 (Nonempty 7 [2, 5]) --> fromElement 2

    filter isEven 0 (Nonempty 7 []) --> fromElement 0

-}
filter : (a -> Bool) -> a -> Nonempty a -> Nonempty a
filter p d (Nonempty x xs) =
    if p x then
        Nonempty x (List.filter p xs)

    else
        case List.filter p xs of
            [] ->
                Nonempty d []

            y :: ys ->
                Nonempty y ys


insertWith : (a -> a -> Order) -> a -> List a -> Nonempty a
insertWith cmp hd aList =
    case aList of
        x :: xs ->
            if cmp x hd == LT then
                Nonempty x (List.sortWith cmp (hd :: xs))

            else
                Nonempty hd aList

        [] ->
            Nonempty hd []


{-| Sort a nonempty list of comparable things, lowest to highest.
-}
sort : Nonempty comparable -> Nonempty comparable
sort (Nonempty x xs) =
    insertWith compare x (List.sort xs)


{-| Sort a nonempty list of things by a derived property.
-}
sortBy : (a -> comparable) -> Nonempty a -> Nonempty a
sortBy f (Nonempty x xs) =
    insertWith (\a b -> compare (f a) (f b)) x (List.sortBy f xs)


{-| Sort a nonempty list of things by a custom comparison function.
-}
sortWith : (a -> a -> Order) -> Nonempty a -> Nonempty a
sortWith f (Nonempty x xs) =
    insertWith f x <| List.sortWith f xs


{-| Remove _adjacent_ duplicate elements from the nonempty list.

    dedup (Nonempty 1 [ 2, 2, 1 ]) --> Nonempty 1 [2, 1]

-}
dedup : Nonempty a -> Nonempty a
dedup (Nonempty x xs) =
    let
        dedupe : a -> List a -> List a -> Nonempty a
        dedupe prev done next =
            case next of
                [] ->
                    Nonempty prev done

                y :: ys ->
                    if y == prev then
                        dedupe prev done ys

                    else
                        dedupe y (prev :: done) ys
    in
    reverse <| dedupe x [] xs


{-| Remove _all_ duplicate elements from the nonempty list, with the remaining elements ordered by first occurrence.

    uniq (Nonempty 1 [ 2, 2, 1 ]) --> Nonempty 1 [2]

-}
uniq : Nonempty a -> Nonempty a
uniq (Nonempty x xs) =
    let
        unique : List a -> Nonempty a -> List a -> Nonempty a
        unique seen done next =
            case next of
                [] ->
                    done

                y :: ys ->
                    if List.member y seen then
                        unique seen done ys

                    else
                        unique (y :: seen) (cons y done) ys
    in
    reverse <| unique [ x ] (Nonempty x []) xs


{-| Reduce a nonempty list from the left with a base case.

    foldl (++) "" (Nonempty "a" [ "b", "c" ]) --> "cba"

-}
foldl : (a -> b -> b) -> b -> Nonempty a -> b
foldl f b (Nonempty x xs) =
    List.foldl f b (x :: xs)


{-| Reduce a nonempty list from the left _without_ a base case. As per Elm convention, the first argument is the current
element and the second argument is the accumulated value. The function is first invoked on the _second_ element, using
the first element as the accumulated value, except for singleton lists in which has the head is returned.

    foldl1 (++) (Nonempty "a" ["b", "c"]) --> "cba"

    foldl1 (++) (fromElement "a") --> "a"

    findMe : Int
    findMe = 42

    minimizeMe : Int -> Int
    minimizeMe n = abs (n-findMe)

    nearest : Int
    nearest = foldl1 (\a b -> if minimizeMe a < minimizeMe b then a else b) (Nonempty 10 [20,30,40,50,60])

    nearest --> 40

-}
foldl1 : (a -> a -> a) -> Nonempty a -> a
foldl1 f (Nonempty x xs) =
    List.foldl f x xs


{-| Take two nonempty lists and compose them into a nonempty list of corresponding pairs.

The length of the new list equals the length of the smallest list given.

-}
zip : Nonempty a -> Nonempty b -> Nonempty ( a, b )
zip (Nonempty x xs) (Nonempty y ys) =
    Nonempty ( x, y ) (List.map2 Tuple.pair xs ys)


{-| Decompose a nonempty list of tuples into a tuple of nonempty lists.

    batters : Nonempty (String, Int)
    batters = Nonempty ("Barry Bonds", 762) [("Hank Aaron", 755), ("Babe Ruth", 714)]

    unzip batters --> (Nonempty "Barry Bonds" ["Hank Aaron", "Babe Ruth"], Nonempty 762 [755, 714])

-}
unzip : Nonempty ( a, b ) -> ( Nonempty a, Nonempty b )
unzip (Nonempty ( x, y ) rest) =
    let
        ( xs, ys ) =
            List.unzip rest
    in
    ( Nonempty x xs, Nonempty y ys )


{-| Use a non-empty list as arguments for functions with the type signature `a -> List a -> Something`, such as `Random.weighted`. This is designed to be used in a pipe style.

    import Random

    type PrizeMoneyInDollars
        = PrizeMoneyInDollars Int

    lotteryGenerator : Random.Generator Int
    lotteryGenerator =
        Nonempty 1 [ 2, 3, 4, 5 ]
            |> Nonempty.map
                (\i ->
                    ( toFloat (10 ^ i)
                    , PrizeMoneyInDollars (10 ^ (6 - i))
                    )
                )
            |> Nonempty.consumeWith Random.weighted

-}
consumeWith : (a -> List a -> b) -> Nonempty a -> b
consumeWith func (Nonempty x xs) =
    func x xs
