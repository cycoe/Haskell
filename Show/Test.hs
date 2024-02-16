{-#LANGUAGE TemplateHaskell#-}
import ShowTH

data Name = Name { first :: String
                 , last :: String
                 }

derivingShow ''Name

data People = People { name :: Name
                     , age :: Int
                     }

derivingShow ''People
