
|||  An intermediate node in a 2-3 tree, parameterized
||| based on the type of its child.
data Node a = Branch3 a a a
            | Branch2 a a

||| Parameteraize the affix by the type fo data it stores.
||| THis is equivalent to lists of length 1 to 4.
data Affix a = One a
             | Two a a
             | Three a a a
             | Four a a a a

||| As usual, the type parameter represents what type
||| of data is stored in this finger tree.
data FingerTree : a -> Type where
  Empty : FingerTree a
  Single : a -> FingerTree a
  Deep : (prefix : Affix a) ->
         (deeper : FingerTree (Node a)) ->
         (suffix : Affix a) ->
         FingerTree a

implementation Show a => Show (Node a) where
  show (Branch3 x y z) = show (x, y, z)
  show (Branch2 x y) = show (x, y)

implementation Show a => Show (Affix a) where
  show (One x) = show (x)
  show (Two x y) = show (x, y)
  show (Three x y z) = show (x, y, z)
  show (Four x y z w) = show (x, y, z, w)

implementation Show a => Show (FingerTree a) where
  show Empty = "empty"
  show (Single x) = "single: " ++ show x
  show (Deep prefix deeper suffix) = "prefix: " ++ show prefix ++
                                     " deep: " ++ show deeper ++
                                     " suffix: " ++ show suffix


affixPrepend : a -> Affix a -> Affix a
affixPrepend x (One y) = Two x y
affixPrepend x (Two y z) = Three x y z
affixPrepend x (Three y z w) = Four x y z w
affixPrepend x (Four y z w s) = ?appendPrefix_rhs_4

affixAppend : a -> Affix a -> Affix a
affixAppend x (One y) = Two y x
affixAppend x (Two y z) = Three y z x
affixAppend x (Three y z w) = Four y z w x

affixToList : Affix a -> List a
affixToList (One x) = [x]
affixToList (Two x y) = [x, y]
affixToList (Three x y z) = [x, y, z]
affixToList (Four x y z w) = [x, y, z, w]


-- User <| to prepend. It's the finger tree analogue of : for lists.
infixr 5 <|
(<|) : a -> FingerTree a -> FingerTree a

x <| Empty = Single x
x <| (Single y) = Deep (One x) Empty (One y)
x <| (Deep (Four y z w s) deeper suffix) = let node = Branch3 z w s
                                            in Deep (Two x y) (node <| deeper) suffix
x <| (Deep prefix deeper suffix) = Deep (affixPrepend x prefix) deeper suffix


infixl 5 |>
(|>) : FingerTree a -> a -> FingerTree a

Empty |> y = Single y
(Single x) |> y = Deep (One x) Empty (One y)
(Deep prefix deeper (Four e d c b)) |> a = let node = Branch3 e d c
                                            in Deep prefix (deeper |> node) (Two b a)
(Deep prefix deeper suffix) |> y = Deep prefix deeper (affixAppend y suffix)



affixFirst : Affix a -> (a, Affix a)
affixFirst (One x) = ?rhs
affixFirst (Two x y) = (x, One y)
affixFirst (Three x y z) = (x, Two y z)
affixFirst (Four x y z w) = (x, Three y z w)

affixLast : Affix a -> (a, Affix a)
affixLast (One x) = ?rhs
affixLast (Two x y) = (y, One x)
affixLast (Three x y z) = (z, Two x y)
affixLast (Four x y z w) = (w, Three x y z)


data View a = Nil | Cons a (FingerTree a)

nodeToAffix : Node a -> Affix a
nodeToAffix (Branch3 x y z) = Three x y z
nodeToAffix (Branch2 x y) = Two x y


viewl : FingerTree a -> View a
viewl Empty = Nil
viewl (Single x) = Cons x Empty
viewl (Deep (One x) deeper suffix)
  = let rest = case viewl deeper of
                    Cons node rest' => Deep (nodeToAffix node) rest' suffix
                    Nil => case suffix of
                                One x => Single x
                                Two x y => Deep (One x) Empty (One y)
                                Three x y z => Deep (Two x y) Empty (One z)
                                Four x y z w => Deep (Three x y z) Empty (One w)
     in Cons x rest
viewl (Deep prefix deeper suffix)
  = let (first, rest) = affixFirst prefix
     in Cons first (Deep rest deeper suffix)


viewr : FingerTree a -> View a
viewr Empty = Nil
viewr (Single x) = Cons x Empty
viewr (Deep prefix deeper (One x))
  = let rest = case viewr deeper of
                    Cons node rest' => Deep prefix rest' (nodeToAffix node)
                    Nil => case prefix of
                                One x => Single x
                                Two x y => Deep (One x) Empty (One y)
                                Three x y z => Deep (One x) Empty (Two y z)
                                Four x y z w => Deep (One x) Empty (Three y z w)
     in Cons x rest
viewr (Deep prefix deeper suffix)
  = let (last, rest) = affixLast suffix
     in Cons last (Deep prefix deeper rest)


fromList : List a -> FingerTree a
fromList = foldr (<|) Empty

toList : FingerTree a -> List a
toList tree = case viewl tree of
                   Nil => []
                   Cons x xs => x :: toList xs


---
-- Concatenation

nodes : List a -> List (Node a)
nodes xs = case xs of
                [] => ?nodes_rhs_1
                [x] => ?nodes_rhs_2
                [x, y] => [Branch2 x y]
                [x, y, z] => [Branch3 x y z]
                (x :: y :: rest) => Branch2 x y :: nodes rest


concatWithMiddle : FingerTree a -> List a -> FingerTree a -> FingerTree a
concatWithMiddle Empty [] right = right
concatWithMiddle Empty (x :: xs) right = x <| concatWithMiddle Empty xs right
concatWithMiddle (Single y) xs right = y <| concatWithMiddle Empty xs right

concatWithMiddle left [] Empty = left
concatWithMiddle left xs@(_::_) Empty = concatWithMiddle left (init xs) Empty |> last xs
concatWithMiddle left xs (Single y) = concatWithMiddle left xs Empty |> y

concatWithMiddle (Deep prefixl deeperl suffixl) mid (Deep prefixr deeperr suffixr)
  = let mid' = nodes $ (affixToList suffixl) ++ mid ++ (affixToList prefixr)
        deeper' = concatWithMiddle deeperl mid' deeperr
     in Deep prefixl deeper' suffixr


infix 5 ><
(><) : {a : Type} -> FingerTree a -> FingerTree a -> FingerTree a
left >< right = concatWithMiddle left [] right


{-

foo : Node Char
foo = Branch3 'n' 'o' 't'

layer3 : FingerTree a
layer3 = Empty

layer2 : FingerTree (Node Char)
layer2 = let prefix = Two (Branch2 'i' 's') (Branch2 'i' 's')
             suffix = Two (Branch3 'n' 'o' 't') (Branch2 'a' 't')
          in Deep prefix layer3 suffix

layer1 : FingerTree Char
layer1 = let prefix = Two 't' 'h'
             suffix = Three 'r' 'e' 'e'
          in Deep prefix layer2 suffix

exampleTree : FingerTree Char
exampleTree = layer1

empty : FingerTree a
empty = Empty

bar : View Char
bar = case viewl exampleTree of
           Cons a rest => viewl rest

-}
