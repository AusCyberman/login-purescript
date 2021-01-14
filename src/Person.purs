module Data.Person (editable_,value_,name_,age_,Person,PersonRow,PersonArg,Pronouns) where

import Data.List
import Data.Maybe
import Data.Tuple

import Data.Symbol (SProxy(..))

name_ = SProxy :: SProxy "name"
age_ = SProxy :: SProxy "age" 
editable_ = SProxy :: SProxy "editable"
value_ = SProxy :: SProxy "value"
pronouns = SProxy :: SProxy "pronouns"

type PersonArg a =  {editable :: Maybe String, value :: a }
type PersonRow = (name :: PersonArg String,  age :: PersonArg Int, gender :: {-, pronouns :: PersonArg (List (PersonArg Pronouns)) -})
type Pronouns = {personal :: String, possessive :: String, intensive :: String}
type Person = Record PersonRow 