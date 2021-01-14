module Main where

import Data.List
import Data.NonEmpty
import Data.Person
import Data.Symbol
import Data.Tuple
import Debug.Trace
import Prelude
import Prim.Row

import Data.Array as A
import Data.Array.NonEmpty as An
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Record as Record
import Record.Builder as Builder
import Spork.Html (Html, IProp)
import Spork.Html as H
import Spork.PureApp (PureApp)
import Spork.PureApp as PureApp
import Web.HTML.Event.EventTypes (offline)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key) as Event

people :: An.NonEmptyArray Person
people = map (flip ($) {}) $ An.fromNonEmpty $ NonEmpty (Builder.build $ s name_ "bob" <<< s age_ 1000) [
  Builder.build $ s name_ "jim" <<< s age_ 10
  ,Builder.build $ s name_ "bnot" <<< s age_ 1000
  ,Builder.build $ s name_ "homesexual" <<< s age_ 324324
]

s pro y = Builder.insert pro {editable: Nothing,value: y}

onEnter ∷ forall i r. i → H.IProp (onKeyDown ∷ Event.KeyboardEvent | r) i
onEnter a = H.onKeyDown \ev →
  if Event.key ev == "Enter"
    then Just a
    else Nothing




renderField :: forall s b r1. 
    IsSymbol s =>
    Eq b =>
    Show b => 
    Lacks s r1 =>
    Cons s (PersonArg b) r1 PersonRow => 
    SProxy s -> 
    String -> 
    (String -> Maybe b) -> 
    (b -> String) ->
    Person ->
    Html Action
renderField pr text r s p
    | isJust $ (Record.get pr p).editable  =
        H.div [H.classes [text,"editable"]] [
            H.text (text <> ": ")
            ,H.input [
              H.onValueInput (H.always (\x -> EditPerson $ Record.modify pr (\v -> v {editable = Just x})))
            ,onEnter $ (submit text pr r p)
            ,H.value (s $ (Record.get pr p).value)]
        ]
    | otherwise = 
        H.div [
          H.classes [text]
          ,H.onClick (H.always_ (EditPerson $ Record.modify pr (\v -> v {editable = Just mempty}) ))] 
            [
            H.text (text <> ": " <> (s $ (Record.get pr p).value))]
    
submit :: forall s b r1. Eq b => IsSymbol s =>  Cons s (PersonArg b) r1 PersonRow => String -> SProxy s -> (String -> Maybe b) -> Person -> Action
submit name pr r p =  case (Record.get pr p).editable of
                            Just x -> if x == mempty then EditPerson $ Record.modify pr (\v -> v {editable = Nothing}) else
                                case r x of
                                  Just y -> if y == (Record.get pr p).value then EditPerson $ Record.modify pr (\v -> v {editable = Nothing}) else EditPerson $ Record.modify pr (\_ -> {editable: Nothing, value: y })
                                  _ -> ShowError $ "Incorrect Input: " <> name
                            _ -> ShowError $ "Incorrect Input" <> name

renderPerson :: Person -> Html Action 
renderPerson p = H.ol [H.classes ["person"]]  $ map (flip ($) p)
              [
                renderField name_ "Name" (Just) identity,
                renderField age_ "Age" fromString show
              ]
          
type ModelRow = (
    previous :: List Person
    ,next :: List Person
    ,current :: Person
    ,error :: List String)
type Model = Record ModelRow
constructModel :: An.NonEmptyArray Person -> Model
constructModel xs = {
    previous: Nil,
    next: fromFoldable (An.tail xs),
    current: An.head xs,
     error: Nil
} 
data Action =
      ShowError String
      | HideError String
      | Inc 
      | Dec 
      | EditPerson (Person -> Person)
update :: Model -> Action -> Model
update m = 
    case _ of
        Inc | length m.next > 0 -> 
            case m.next of
                  (x:xs) -> m {current =  x,previous = m.current:m.previous, next = xs}
                  _     -> addError "End of People" m
        Dec | length m.previous > 0 -> 
            case m.previous of 
                  (x:xs) -> m {current = x, previous = xs, next = m.current:m.next}
                  _      -> m
        EditPerson f -> m {current = f m.current}
        ShowError s -> addError s m  
        HideError s -> m {error = filter (\x -> x /= s) m.error}
        _ -> m

addError :: String -> Model -> Model
addError x m = m {error = nub $ x:m.error}

render :: Model -> H.Html Action
render m = H.div [] $ [
  
  H.button 
    [H.onClick (H.always_ Inc)] 
    [H.text "+"]
  ,H.button 
    [H.onClick (H.always_ Dec)] 
    [H.text "-"],
  renderPerson m.current] <> if not $ null m.error then A.fromFoldable $ map renderError m.error else [] 

renderError :: String -> Html Action
renderError x = H.li [H.onClick (H.always_ (HideError x))] [H.text x]

app ∷ PureApp Model Action
app = { update, render, init: constructModel people }

main ∷ Effect Unit
main = void $ PureApp.makeWithSelector app "#app"