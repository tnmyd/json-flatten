module AesonFun(collapse) where

import qualified Data.Map as M (empty, Map, insert, lookup)
-- import qualified Data.HashMap.Strict as HM (toList)
import qualified Data.List as L (foldl')
import qualified Data.Aeson as Ae (decode, )
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T (unpack, Text)
import Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy.Char8 as BS ( pack )

collapse :: String -> StringMap
collapse s = maybe M.empty (collapser0 M.empty) $ toValue s

toValue :: String -> Maybe Value
toValue = Ae.decode . BS.pack

type StringMap = M.Map String String

delim = "."

type Collapser = StringMap -> Value -> StringMap

collapser0 = collapser Nothing

collapser :: (Maybe String) -> Collapser
collapser s m v = case v of
                    AT.Object ob  -> L.foldl' (\m' (c,v') -> c m' v')  m pairs where
                      pairs :: [(Collapser, Value)]
                      pairs = map toPair $ KM.toList ob
                      toPair :: (Key, Value) -> (Collapser, Value)
                      toPair (t, v') = (collapser s', v') where
                        s' = case s of
                               Just p  -> Just $ p ++ delim ++ ( show t)
                               Nothing -> Just $ ( show t)
                    AT.String t  -> maybe m (\str -> M.insert str ( show t) m) s
                    otherwise    -> m