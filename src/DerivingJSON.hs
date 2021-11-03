{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module DerivingJSON where

import qualified Data.Aeson.Types as Ae
import Data.Char (isLower)
import GHC.Generics
import qualified GenericPretty as GP

prefixCamel = Ae.camelTo2 '_' . dropWhile isLower
prefixCamelOptions = Ae.defaultOptions {
    Ae.fieldLabelModifier = prefixCamel
    }



newtype PrefixCamel a =
    PrefixCamel
        { unPrefixCamel :: a
        }
    deriving (Generic)

instance (Generic a, Ae.GFromJSON Ae.Zero (Rep a)) =>
         Ae.FromJSON (PrefixCamel a) where
    parseJSON =
        fmap PrefixCamel .
        Ae.genericParseJSON prefixCamelOptions

instance (Generic a, Ae.GToJSON' Ae.Value Ae.Zero (Rep a)) =>
         Ae.ToJSON (PrefixCamel a) where
    toJSON =
        Ae.genericToJSON prefixCamelOptions .
        unPrefixCamel


instance (Generic a, GP.GPrettyShow (Rep a)) => GP.PrettyShow (PrefixCamel a) where
    prettyShow = GP.genericPrettyShow GP.defaultOptionsL { GP.labelModifier = prefixCamel }
                 . unPrefixCamel
