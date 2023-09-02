module Chameleon.Styled.Elems
  ( class StyledElems
  , styledElems
  , class StyledElemsRL
  , styledElemsRL
  , class StyledElemsOne
  , styledElemsOne
  ) where

import Prelude

import Chameleon (Key, Prop)
import Chameleon.Styled.Core (class HtmlStyled, class IsStyle, ElemName(..), ElemScope(..), styleKeyedNodeNamed, styleLeafNamed, styleNodeNamed)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.Symbol as Sym
import Record as Record
import Type.Proxy (Proxy(..))

class
  StyledElems
    (rowIn :: Row Type)
    (rowOut :: Row Type)
  | rowIn -> rowOut
  where
  styledElems :: String -> Record rowIn -> Record rowOut

instance
  ( RowToList rowIn rowlist
  , StyledElemsRL rowlist rowIn rowOut
  ) =>
  StyledElems rowIn rowOut
  where
  styledElems elemScope = styledElemsRL
    (ElemScope elemScope)
    (Proxy :: Proxy rowlist)

--------------------------------------------------------------------------------

class
  StyledElemsRL
    (rowlist :: RowList Type)
    (rowIn :: Row Type)
    (rowOut :: Row Type)
  | rowlist rowIn -> rowOut
  where
  styledElemsRL :: ElemScope -> Proxy rowlist -> Record rowIn -> Record rowOut

instance StyledElemsRL RL.Nil row () where
  styledElemsRL _ _ _ = {}

instance
  ( StyledElemsRL rowlistPrev rowIn rowOutPrev
  , Row.Cons sym typ rowTrash rowIn
  , Row.Cons sym (Array (Prop a) -> styledElem_) rowOutPrev rowOutTmp
  , Row.Cons sym_ styledElem_ rowOutTmp rowOut
  , Sym.Append sym "_" sym_
  , Row.Lacks sym rowOutPrev
  , Row.Lacks sym_ rowOutTmp
  , StyledElemsOne typ (Array (Prop a) -> styledElem_)
  , IsSymbol sym
  , IsSymbol sym_
  ) =>
  StyledElemsRL
    (RL.Cons sym typ rowlistPrev)
    rowIn
    rowOut
  where
  styledElemsRL
    :: ElemScope
    -> Proxy (RL.Cons sym typ rowlistPrev)
    -> Record rowIn
    -> Record rowOut
  styledElemsRL elemScope _ recordIn =
    let
      recordPrev :: Record rowOutPrev
      recordPrev = styledElemsRL elemScope (Proxy :: Proxy rowlistPrev) recordIn

      spec :: typ
      spec = Record.get proxySym recordIn

      elemName :: ElemName
      elemName = ElemName (reflectSymbol proxySym)

      styledElem :: Array (Prop a) -> styledElem_
      styledElem = styledElemsOne elemName elemScope spec

      styledElem_ :: styledElem_
      styledElem_ = styledElem []

      proxySym :: Proxy sym
      proxySym = Proxy

      proxySym_ :: Proxy sym_
      proxySym_ = Proxy
    in
      recordPrev
        # Record.insert proxySym styledElem
        # Record.insert proxySym_ styledElem_

--------------------------------------------------------------------------------

class
  StyledElemsOne
    (typ :: Type)
    (styledElem :: Type)
  | typ -> styledElem
  where
  styledElemsOne :: ElemName -> ElemScope -> typ -> styledElem

instance
  ( HtmlStyled html
  ) =>
  StyledElemsOne
    (Array (Prop a) -> html a)
    (Array (Prop a) -> html a)
  where
  styledElemsOne elemName elemScope elem =
    styleLeafNamed (Just elemName) (Just elemScope) elem unit

else instance
  ( HtmlStyled html
  ) =>
  StyledElemsOne
    (Array (Prop a) -> Array (html a) -> html a)
    (Array (Prop a) -> Array (html a) -> html a)
  where
  styledElemsOne elemName elemScope elem =
    styleNodeNamed (Just elemName) (Just elemScope) elem unit

else instance
  ( HtmlStyled html
  ) =>
  StyledElemsOne
    (Array (Prop a) -> Array (Key /\ html a) -> html a)
    (Array (Prop a) -> Array (Key /\ html a) -> html a)
  where
  styledElemsOne elemName elemScope elem =
    styleKeyedNodeNamed (Just elemName) (Just elemScope) elem unit

instance
  ( IsStyle style
  , HtmlStyled html
  ) =>
  StyledElemsOne
    ((Array (Prop a) -> html a) /\ style)
    (Array (Prop a) -> html a)
  where
  styledElemsOne elemName elemScope (elem /\ style) =
    styleLeafNamed (Just elemName) (Just elemScope) elem style

else instance
  ( IsStyle style
  , HtmlStyled html
  ) =>
  StyledElemsOne
    ((Array (Prop a) -> Array (html a) -> html a) /\ style)
    (Array (Prop a) -> Array (html a) -> html a)
  where
  styledElemsOne elemName elemScope (elem /\ style) =
    styleNodeNamed (Just elemName) (Just elemScope) elem style

else instance
  ( IsStyle style
  , HtmlStyled html
  ) =>
  StyledElemsOne
    ((Array (Prop a) -> Array (Key /\ html a) -> html a) /\ style)
    (Array (Prop a) -> Array (Key /\ html a) -> html a)
  where
  styledElemsOne elemName elemScope (elem /\ style) =
    styleKeyedNodeNamed (Just elemName) (Just elemScope) elem style

else instance
  ( IsStyle style
  , HtmlStyled html
  ) =>
  StyledElemsOne
    ((Array (Prop a) -> Array (Key /\ html a) -> html a) /\ style)
    (Array (Prop a) -> Array (Key /\ html a) -> html a)
  where
  styledElemsOne elemName elemScope (elem /\ style) =
    styleKeyedNodeNamed (Just elemName) (Just elemScope) elem style