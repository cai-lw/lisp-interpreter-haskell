{-# LANGUAGE ScopedTypeVariables #-}

module Klang.Lambda where
import Klang.Type
import Control.Applicative

lit :: Data -> KTerm
lit = pure . Right

slot :: String -> KTerm
slot s = K $ \h -> case lookup s h of
    Just x -> x
    Nothing -> Left ("Cannot find variable \"" ++ s ++"\"")

lambda :: String -> KTerm -> KTerm
lambda s t = K $ \h -> (Right . KLam) $ \x -> run t ((s, x) : h)

apply :: KFunc2
apply = liftA2 $ \kf ->
    case kf >>= unpack  of
        Right (f :: (Res -> Res)) -> f
        Left e -> const $ Left e
