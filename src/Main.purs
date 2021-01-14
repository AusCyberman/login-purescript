module Main where

import Data.List
import Data.NonEmpty
import Data.Symbol
import Data.Tuple
import Person
import Prelude
import Prim.Row
import Data.Array as A
import Data.Array.NonEmpty as An
import Effect (Effect)
import Record as Record
import Spork.Html (Html)
import Spork.Html as H
import Spork.PureApp (PureApp)
import Spork.PureApp as PureApp
import Web.HTML.Event.EventTypes (offline)
people :: An.NonEmptyArray Person
people = An.fromNonEmpty $ NonEmpty ({name: "bob",age:1}) [
  {name: "hello", age: 10},
  {name: "bob", age: 20},
  {name: "wow", age: 50}
]
renderPerson :: Person -> Html Action 
renderPerson p = H.ol [H.classes ["person"]]   
              [
                H.li [H.classes ["name"],H.onClick (H.always_ ChangeCurrent )] [ H.text ("Name: " <> p.name)],
                H.li [H.classes ["age"]] [H.text ("Age: " <> show p.age)]
              ]
          
type ModelRow = (
    previous :: List Person,
    next :: List Person,
    current :: Person,
    editable :: PersonEditable)
type Model = Record ModelRow
constructModel :: An.NonEmptyArray Person -> Model
constructModel xs = {
    previous: Nil,
    next: fromFoldable (An.tail xs),
    current: An.head xs,
    editable: {
      name: false,
      age: false
    }
} 

data Action = 
      Inc 
      | Dec 
      | MakeEditable (forall a b. Cons a b PersonRow PersonRow => SProxy a) 
      | ChangeCurrent (forall l b r1. IsSymbol l => Lacks l r1 => Cons l b r1 PersonRow => { l :: b}) 

update :: Model -> Action -> Model
update m = 
    case _ of
        Inc | length m.next > 0 -> 
            case m.next of
                  (x:xs) -> m {current =  x,previous = m.current:m.previous, next = xs}
                  _     -> m
        Dec | length m.previous > 0 -> 
            case m.previous of 
                  (x:xs) -> m {current = x, previous = xs, next = m.current:m.next}
                  _      -> m
        ChangeCurrent l -> m {current = Record.merge l m.current }
        MakeEditable s -> m {editable =  Record.set s true m.editable}
        _ -> m
verify ::
  forall l t _1
   . IsSymbol l
  => Cons l t _1 PersonRow
  => Tuple (SProxy l) t
  -> Boolean
verify _ = true
render :: Model -> H.Html Action
render m = H.div [] [
  H.button 
    [H.onClick (H.always_ Inc)] 
    [H.text "+"]
  ,H.button 
    [H.onClick (H.always_ Dec)] 
    [H.text "-"],
  renderPerson m.current
]

app ∷ PureApp Model Action
app = { update, render, init: constructModel people }

main ∷ Effect Unit
main = void $ PureApp.makeWithSelector app "#app"