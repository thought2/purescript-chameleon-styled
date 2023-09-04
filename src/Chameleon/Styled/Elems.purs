module Chameleon.Styled.Elems
  ( class StyleElems
  , styleElems
  , class StyleElemsRL
  , styleElemsRL
  , class StyleElemsOne
  , styleElemsOne
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
  StyleElems
    (rowIn :: Row Type)
    (rowOut :: Row Type)
  | rowIn -> rowOut
  where
  styleElems :: String -> Record rowIn -> Record rowOut

instance
  ( RowToList rowIn rowlist
  , StyleElemsRL rowlist rowIn rowOut
  ) =>
  StyleElems rowIn rowOut
  where
  styleElems elemScope = styleElemsRL
    (ElemScope elemScope)
    (Proxy :: Proxy rowlist)

--------------------------------------------------------------------------------

class
  StyleElemsRL
    (rowlist :: RowList Type)
    (rowIn :: Row Type)
    (rowOut :: Row Type)
  | rowlist rowIn -> rowOut
  where
  styleElemsRL :: ElemScope -> Proxy rowlist -> Record rowIn -> Record rowOut

instance StyleElemsRL RL.Nil row () where
  styleElemsRL _ _ _ = {}

instance
  ( StyleElemsRL rowlistPrev rowIn rowOutPrev
  , Row.Cons sym typ rowTrash rowIn
  , Row.Cons sym (Array (Prop a) -> styledElem_) rowOutPrev rowOutTmp
  , Row.Cons sym_ styledElem_ rowOutTmp rowOut
  , Sym.Append sym "_" sym_
  , Row.Lacks sym rowOutPrev
  , Row.Lacks sym_ rowOutTmp
  , StyleElemsOne typ (Array (Prop a) -> styledElem_)
  , IsSymbol sym
  , IsSymbol sym_
  ) =>
  StyleElemsRL
    (RL.Cons sym typ rowlistPrev)
    rowIn
    rowOut
  where
  styleElemsRL
    :: ElemScope
    -> Proxy (RL.Cons sym typ rowlistPrev)
    -> Record rowIn
    -> Record rowOut
  styleElemsRL elemScope _ recordIn =
    let
      recordPrev :: Record rowOutPrev
      recordPrev = styleElemsRL elemScope (Proxy :: Proxy rowlistPrev) recordIn

      spec :: typ
      spec = Record.get proxySym recordIn

      elemName :: ElemName
      elemName = ElemName (reflectSymbol proxySym)

      styledElem :: Array (Prop a) -> styledElem_
      styledElem = styleElemsOne elemName elemScope spec

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
  StyleElemsOne
    (typ :: Type)
    (styledElem :: Type)
  | typ -> styledElem
  where
  styleElemsOne :: ElemName -> ElemScope -> typ -> styledElem

instance leaf ::
  ( HtmlStyled html
  ) =>
  StyleElemsOne
    (Array (Prop a) -> html a)
    (Array (Prop a) -> html a)
  where
  styleElemsOne elemName elemScope elem =
    styleLeafNamed (Just elemName) (Just elemScope) elem unit

else instance node ::
  ( HtmlStyled html
  ) =>
  StyleElemsOne
    (Array (Prop a) -> Array (html a) -> html a)
    (Array (Prop a) -> Array (html a) -> html a)
  where
  styleElemsOne elemName elemScope elem =
    styleNodeNamed (Just elemName) (Just elemScope) elem unit

else instance keyedNode ::
  ( HtmlStyled html
  ) =>
  StyleElemsOne
    (Array (Prop a) -> Array (Key /\ html a) -> html a)
    (Array (Prop a) -> Array (Key /\ html a) -> html a)
  where
  styleElemsOne elemName elemScope elem =
    styleKeyedNodeNamed (Just elemName) (Just elemScope) elem unit

instance leafStyled ::
  ( IsStyle style
  , HtmlStyled html
  ) =>
  StyleElemsOne
    ((Array (Prop a) -> html a) /\ style)
    (Array (Prop a) -> html a)
  where
  styleElemsOne elemName elemScope (elem /\ style) =
    styleLeafNamed (Just elemName) (Just elemScope) elem style

else instance leafStyledOpt ::
  ( IsStyle style
  , HtmlStyled html
  ) =>
  StyleElemsOne
    (Record opt -> (Array (Prop a) -> html a) /\ style)
    (Record opt -> Array (Prop a) -> html a)
  where
  styleElemsOne elemName elemScope f opt =
    let
      elem /\ style = f opt
    in
      styleLeafNamed (Just elemName) (Just elemScope) elem style

else instance nodeStyled ::
  ( IsStyle style
  , HtmlStyled html
  ) =>
  StyleElemsOne
    ((Array (Prop a) -> Array (html a) -> html a) /\ style)
    (Array (Prop a) -> Array (html a) -> html a)
  where
  styleElemsOne elemName elemScope (elem /\ style) =
    styleNodeNamed (Just elemName) (Just elemScope) elem style

else instance nodeStyledOpt ::
  ( IsStyle style
  , HtmlStyled html
  ) =>
  StyleElemsOne
    (Record opt -> (Array (Prop a) -> Array (html a) -> html a) /\ style)
    (Record opt -> Array (Prop a) -> Array (html a) -> html a)
  where
  styleElemsOne elemName elemScope f opt =
    let
      elem /\ style = f opt
    in
      styleNodeNamed (Just elemName) (Just elemScope) elem style

else instance keyedNodeStyledOpt ::
  ( IsStyle style
  , HtmlStyled html
  ) =>
  StyleElemsOne
    (Record opt -> (Array (Prop a) -> Array (Key /\ html a) -> html a) /\ style)
    (Record opt -> Array (Prop a) -> Array (Key /\ html a) -> html a)
  where
  styleElemsOne elemName elemScope f opt =
    let
      elem /\ style = f opt
    in
      styleKeyedNodeNamed (Just elemName) (Just elemScope) elem style
