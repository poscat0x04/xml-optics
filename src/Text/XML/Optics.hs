module Text.XML.Optics
  ( -- * Document
    Document (..),

    -- ** Lenses
    root,
    prologue,
    epilogue,

    -- * Node
    Node (..),

    -- ** Prisms
    _Element,
    _Content,
    CanbeInstruction (..),
    CanbeComment (..),

    -- * Element
    Element (..),

    -- ** Lenses
    attrs,
    nodes,
    name,

    -- ** Traversals

    -- *** Filtering elements
    el,
    named,
    attr,
    attributeSatisfies,
    withoutAttribute,
    attributeIs,

    -- *** Traversing subnodes
    lower,
    plate,
    text,
    comment,

    -- * Composing traversals
    (./),
    (.//),

    -- * Name
    Name (..),

    -- ** Lenses
    localName,
    namespace,
    prefix,
  )
where

import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import Optics.Core
import Text.XML

root :: Lens' Document Element
root = lens documentRoot (\d r -> d {documentRoot = r})
{-# INLINE root #-}

prologue :: Lens' Document Prologue
prologue = lens documentPrologue (\d p -> d {documentPrologue = p})
{-# INLINE prologue #-}

epilogue :: Lens' Document [Miscellaneous]
epilogue = lens documentEpilogue (\d e -> d {documentEpilogue = e})
{-# INLINE epilogue #-}

name :: Lens' Element Name
name = lens elementName (\e n -> e {elementName = n})
{-# INLINE name #-}

attrs :: Lens' Element (Map Name Text)
attrs = lens elementAttributes (\e a -> e {elementAttributes = a})
{-# INLINE attrs #-}

nodes :: Lens' Element [Node]
nodes = lens elementNodes (\e ns -> e {elementNodes = ns})
{-# INLINE nodes #-}

localName :: Lens' Name Text
localName = lens nameLocalName (\n lc -> n {nameLocalName = lc})
{-# INLINE localName #-}

namespace :: Lens' Name (Maybe Text)
namespace = lens nameNamespace (\n ns -> n {nameNamespace = ns})
{-# INLINE namespace #-}

prefix :: Lens' Name (Maybe Text)
prefix = lens namePrefix (\n pfx -> n {namePrefix = pfx})
{-# INLINE prefix #-}

_Element :: Prism' Node Element
_Element = prism' NodeElement (\case NodeElement e -> Just e; _ -> Nothing)
{-# INLINE _Element #-}

_Content :: Prism' Node Text
_Content = prism' NodeContent (\case NodeContent c -> Just c; _ -> Nothing)
{-# INLINE _Content #-}

class CanbeInstruction t where
  _Instruction :: Prism' t Instruction

class CanbeComment t where
  _Comment :: Prism' t Text

instance CanbeInstruction Node where
  _Instruction = prism' NodeInstruction (\case NodeInstruction i -> Just i; _ -> Nothing)
  {-# INLINE _Instruction #-}

instance CanbeInstruction Miscellaneous where
  _Instruction = prism' MiscInstruction (\case MiscInstruction i -> Just i; _ -> Nothing)
  {-# INLINE _Instruction #-}

instance CanbeComment Node where
  _Comment = prism' NodeComment (\case NodeComment c -> Just c; _ -> Nothing)
  {-# INLINE _Comment #-}

instance CanbeComment Miscellaneous where
  _Comment = prism' MiscComment (\case MiscComment c -> Just c; _ -> Nothing)
  {-# INLINE _Comment #-}

el :: Name -> AffineTraversal' Element Element
el n =
  atraversal
    (\e -> if e ^. name == n then Right e else Left e)
    (\e e' -> if e ^. name == n then e' else e)
{-# INLINE el #-}

named :: Text -> AffineTraversal' Element Element
named n =
  atraversal
    (\e -> if e ^. name % localName == n then Right e else Left e)
    (\e e' -> if e ^. name % localName == n then e' else e)
{-# INLINE named #-}

attr :: Name -> AffineTraversal' Element Text
attr n = attrs % ix n
{-# INLINE attr #-}

attributeSatisfies :: Name -> (Maybe Text -> Bool) -> AffineTraversal' Element Element
attributeSatisfies n f =
  atraversal
    (\e -> if f (e ^? attr n) then Right e else Left e)
    (\e e' -> if f (e ^? attr n) then e' else e)
{-# INLINE attributeSatisfies #-}

withoutAttribute :: Name -> AffineTraversal' Element Element
withoutAttribute n = attributeSatisfies n isNothing
{-# INLINE withoutAttribute #-}

attributeIs :: Name -> Text -> AffineTraversal' Element Element
attributeIs n t = attributeSatisfies n (== Just t)
{-# INLINE attributeIs #-}

-- | Traverse all the subnodes of an 'Element'
lower :: IxTraversal' Int Element Node
lower = nodes % itraversed
{-# INLINE lower #-}

-- | 'lower' then select all the 'NodeElement's
plate :: IxTraversal' Int Element Element
plate = lower % _Element
{-# INLINE plate #-}

-- | 'lower' then select all the 'NodeContent's
text :: IxTraversal' Int Element Text
text = lower % _Content
{-# INLINE text #-}

-- | 'lower' then select all the 'NodeComment's
comment :: IxTraversal' Int Element Text
comment = lower % _Comment
{-# INLINE comment #-}

infixr 9 ./

-- | Compose two 'Traversal'' using 'plate'
--
-- @t1 './' t2 = t1 '%' 'plate' '%' t2@
(./) :: (Is (Join k A_Traversal) (Join (Join k A_Traversal) l), Is l (Join (Join k A_Traversal) l), Is k (Join k A_Traversal), Is A_Traversal (Join k A_Traversal)) => Optic k is s t Element Element -> Optic l js Element Element a b -> Optic (Join (Join k A_Traversal) l) (Append (Append is (WithIx Int)) js) s t a b
o1 ./ o2 = o1 % plate % o2
{-# INLINE (./) #-}

infixr 9 .//

-- | A version of './' that ignores the index from 'plate'
--
-- @t1 './/' t1 = t1 '<%' 'plate' '%' t2@
(.//) :: (Is (Join k A_Traversal) (Join (Join k A_Traversal) l), Is l (Join (Join k A_Traversal) l), Is k (Join k A_Traversal), Is A_Traversal (Join k A_Traversal)) => Optic k is s t Element Element -> Optic l js Element Element a b -> Optic (Join (Join k A_Traversal) l) (Append is js) s t a b
o1 .// o2 = o1 <% plate % o2
{-# INLINE (.//) #-}
