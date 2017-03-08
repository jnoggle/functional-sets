module Main exposing (..)


type Set comparable
    = Tree Int comparable (Set comparable) (Set comparable)
    | Empty


empty : Set comparable
empty =
    Empty


singleton : comparable -> Set comparable
singleton item =
    tree item empty empty


insert : comparable -> Set comparable -> Set comparable
insert item set =
    case set of
        Empty ->
            singleton item

        Tree _ head left right ->
            if item < head then
                tree head (insert item left) right |> balance
            else if item > head then
                tree head left (insert item right) |> balance
            else
                set


fromList : List comparable -> Set comparable
fromList items =
    List.foldl insert empty items


height : Set comparable -> Int
height set =
    case set of
        Empty ->
            0

        Tree height _ _ _ ->
            height


tree : comparable -> Set comparable -> Set comparable -> Set comparable
tree head left right =
    Tree
        (max (height left) (height right) |> (+) 1)
        head
        left
        right


rotl : Set comparable -> Set comparable
rotl set =
    case set of
        Tree _ head lessThans (Tree _ subHead betweens greaterThans) ->
            tree subHead (tree head lessThans betweens) greaterThans

        _ ->
            set


rotr : Set comparable -> Set comparable
rotr set =
    case set of
        Tree _ head (Tree _ subHead lessThans betweens) greaterThans ->
            tree subHead lessThans (tree head betweens greaterThans)

        _ ->
            set


diff : Set comparable -> Int
diff set =
    case set of
        Empty ->
            0

        Tree _ _ left right ->
            height right - height left


balance : Set comparable -> Set comparable
balance set =
    case set of
        Empty ->
            set

        Tree _ head left right ->
            if diff set == -2 && diff left == 1 then
                -- left leaning tree with right-leaning left subtree. Rotate left, then right.
                tree head (rotl left) right |> rotr
            else if diff set < -1 then
                -- left leaning tree, generally. Rotate right.
                rotr set
            else if diff set == 2 && diff right == -1 then
                -- right leaning tree with left-leaning right subtree. Rotate right, then left.
                tree head left (rotr right) |> rotl
            else if diff set > 1 then
                -- right leaning tree, generally. Rotate left.
                rotl set
            else
                -- diff is -1, 0, or 1. Already balanced, no operation required.
                set


member : comparable -> Set comparable -> Bool
member item set =
    case set of
        Empty ->
            False

        Tree _ head left right ->
            if item < head then
                member item left
            else if item > head then
                member item right
            else
                True
