module Internal.Name where

data Name = Raw String
          | Renamed String Int
          | Erased
          deriving (Eq, Ord)

instance Show Name where
 show (Raw name) = name
 show (Renamed name i) = name ++ ('$' : show i)
 show Erased = "_"

rename :: Int -> Name -> Name
rename i (Raw name) = Renamed name i
rename i (Renamed name _) = Renamed name i
rename _ Erased = Erased

isErased :: Name -> Bool
isErased Erased = True
isErased _ = False
