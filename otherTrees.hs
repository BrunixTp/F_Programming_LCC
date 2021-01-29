module MoreTrees where

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

-- ExpInt's terms can be seen as trees whose leafs are 
-- integers and whose nodes (non leaves) are operators
