{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverlappingInstances #-}

module ShowConcat((@@)) where

class ShowConcat a b where
   (@@) :: Show a => Show b => a -> b -> String

instance ShowConcat String String where
   (@@) a b = a ++ b

instance ShowConcat String a where
   (@@) a b = a ++ show b

instance ShowConcat a String where
   (@@) a b = show a ++ b

instance ShowConcat a b where
   (@@) a b = show a ++ show b

