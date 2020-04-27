module Data.FingerTree

--%default total

|||  An intermediate node in a 2-3 tree, parameterized
||| based on the type of its child.
data Node v a = Node2 v a a
              | Node3 v a a a

||| Parameteraize the digit by the type fo data it stores.
||| This is equivalent to lists of length 1 to 4.
data Digit a = One a
             | Two a a
             | Three a a a
             | Four a a a a

implementation Foldable Digit where
  foldr f acc (One x) = f x acc
  foldr f acc (Two x y) = f x (f y acc)
  foldr f acc (Three x y z) = f x (f y (f z acc))
  foldr f acc (Four x y z w) = f x (f y (f z (f w acc)))

  foldl f acc (One x) = f acc x
  foldl f acc (Two x y) = f (f acc x) y
  foldl f acc (Three x y z) = f (f (f acc x) y) z
  foldl f acc (Four x y z w) = f (f (f (f acc x) y) z) w


digitToList : Digit a -> List a
digitToList (One x) = [x]
digitToList (Two x y) = [x, y]
digitToList (Three x y z) = [x, y, z]
digitToList (Four x y z w) = [x, y, z, w]

digitFirst : Digit a -> (a, Digit a)
--digitFirst (One x) = ?no
digitFirst (Two x y) = (x, One y)
digitFirst (Three x y z) = (x, Two y z)
digitFirst (Four x y z w) = (x, Three y z w)

digitLast : Digit a -> (a, Digit a)
--digitLast (One x) = ?rhs
digitLast (Two x y) = (y, One x)
digitLast (Three x y z) = (z, Two x y)
digitLast (Four x y z w) = (w, Three x y z)

nodeToDigit : Node v a -> Digit a
nodeToDigit (Node2 _ x y) = Two x y
nodeToDigit (Node3 _ x y z) = Three x y z


--
-- fromList : List a -> FingerTree a
-- fromList = foldr (<|) Empty
--
-- toList : FingerTree a -> List a
-- toList tree = case viewl tree of
--                    Nil => []
--                    Cons x xs => x :: toList xs


-- Measurements
interface Monoid v => Measured a v where
  measure : a -> v


implementation Measured a v => Measured (Digit a) v where
  measure = concatMap measure


-- Caching measurements
implementation Measured a v => Measured (Node v a) v where
  measure (Node2 v _ _) = v
  measure (Node3 v _ _ _) = v

node2 : Measured a v => a -> a -> Node v a
node2 x y = Node2 (measure x <+> measure y) x y

node3 : Measured a v => a -> a -> a -> Node v a
node3 x y z = Node3 (measure x <+> measure y <+> measure z) x y z



data FingerTree v a = Empty
                    | Single a
                    | Deep v (Digit a) (FingerTree v (Node v a)) (Digit a)

implementation Measured a v => Measured (FingerTree v a) v where
  measure Empty = neutral
  measure (Single x) = measure x
  measure (Deep v _ _ _) = v

deep : Measured a v => Digit a -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deep pr m sf = Deep (measure pr <+> measure m <+> measure sf) pr m sf


digitToTree : Measured a v => Digit a -> FingerTree v a
digitToTree (One x) = Single x
digitToTree (Two x y) = deep (One x) Empty (One y)
digitToTree (Three x y z) = deep (Two x y) Empty (One z)
digitToTree (Four x y z w) = deep (Two x y) Empty (Two z w)
-- digitToTree = foldl <: Empty


----------
-- 4.3 Construction, deconstruction and concatenation
----------

-- User <| to prepend. It's the finger tree analogue of : for lists.
infixr 5 <|
(<|) : Measured a v => a -> FingerTree v a -> FingerTree v a
a <| Empty = Single a
a <| (Single b) = deep (One a) Empty (One b)
a <| (Deep v pr m sf) =
  case pr of
       One b        => Deep (measure a <+> v) (Two a b) m sf
       Two b c      => Deep (measure a <+> v) (Three a b c) m sf
       Three b c d  => Deep (measure a <+> v) (Four a b c d) m sf
       Four b c d e => Deep (measure a <+> v) (Two a b) (node3 c d e <| m) sf


infixl 5 |>
(|>) : Measured a v => FingerTree v a -> a -> FingerTree v a
Empty |> a = Single a
(Single b) |> a = deep (One b) Empty (One a)
(Deep v pr m sf) |> a =
  case pr of
       One b        => Deep (v <+> measure a) pr m (Two b a)
       Two c b      => Deep (v <+> measure a) pr m (Three c b a)
       Three d c b  => Deep (v <+> measure a) pr m (Four d c b a)
       Four e d c b => Deep (v <+> measure a) pr (m |> node3 e d c) (Two b a)


data View : (Type -> Type) -> Type -> Type where
  Nil : View s a
  Cons : a -> s a -> View s a

viewl : Measured a v => FingerTree v a -> View (FingerTree v) a
viewl Empty = Nil
viewl (Single x) = Cons x Empty
viewl (Deep _ (One x) m sf)
  = let rest = case viewl m of
                    Nil => digitToTree sf
                    Cons node m' => deep (nodeToDigit node) m' sf
     in Cons x rest
viewl (Deep _ pr m sf)
  = let (head, tail) = digitFirst pr
     in Cons head (deep tail m sf)

viewr : Measured a v => FingerTree v a -> View (FingerTree v) a
viewr Empty = Nil
viewr (Single x) = Cons x Empty
viewr (Deep _ pr m (One x))
  = let rest = case viewr m of
                    Nil => digitToTree pr
                    Cons node m' => deep pr m' (nodeToDigit node)
     in Cons x rest
viewr (Deep _ prefix deeper suffix)
  = let (last, rest) = digitLast suffix
     in Cons last (deep prefix deeper rest)



nodes : Measured a v => List a -> List (Node v a)
nodes [a, b] = [node2 a b]
nodes [a, b, c] = [node3 a b c]
nodes [a, b, c, d] = [node2 a b, node2 c d]
nodes (a :: b :: c :: xs') = node3 a b c :: nodes xs'


app3 : Measured a v => FingerTree v a -> List a -> FingerTree v a -> FingerTree v a
app3 Empty [] right = right
app3 Empty (x :: xs) right = x <| app3 Empty xs right
app3 (Single y) xs right = y <| app3 Empty xs right
app3 left [] Empty = left
app3 left xs@(_::_) Empty = app3 left (init xs) Empty |> last xs
app3 left xs (Single y) = app3 left xs Empty |> y
app3 (Deep v1 pr1 m1 sf1) ts (Deep v2 pr2 m2 sf2)
  = let ts' = nodes $ (digitToList sf1) ++ ts ++ (digitToList pr2)
     in deep pr1 (app3 m1 ts' m2) sf2

infix 5 ><
(><) : Measured a v => FingerTree v a -> FingerTree v a -> FingerTree v a
left >< right = app3 left [] right

implementation Measured a v => Semigroup (FingerTree v a) where
  (<+>) = (><)

implementation Measured a v => Monoid (FingerTree v a) where
  neutral = Empty

{-

foo : Node Char
foo = Node3 'n' 'o' 't'

layer3 : FingerTree a
layer3 = Empty

layer2 : FingerTree (Node Char)
layer2 = let prefix = Two (Node2 'i' 's') (Node2 'i' 's')
             suffix = Two (Node3 'n' 'o' 't') (Node2 'a' 't')
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
