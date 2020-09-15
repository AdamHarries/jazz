{-# LANGUAGE OverloadedStrings #-}
module XMLUtils
    (
        searchDoc,
        maybeSearchDoc,
        searchDocOrElse,
        getTextOrElse,
        defaultNodeWithContent
    ) where

import qualified Data.Map        as DM
import qualified Data.Text       as TE
import           Text.XML
import           Text.XML.Cursor


-- Search a document for any of a specifically named node.
searchDoc :: Document -> Name -> [Node]
searchDoc d name = Prelude.map node $ descendant (fromDocument d) >>= element name

-- Search a document for a specifically named node
maybeSearchDoc :: Document -> Name -> Maybe Node
maybeSearchDoc d name = case (searchDoc d name) of
  (x:_) -> Just x
  []    -> Nothing

-- Search a document for a specifically named node, and if it doesn't exist, return another one
searchDocOrElse :: Document -> Name -> Node -> Node
searchDocOrElse d name el = case (searchDoc d name) of
  (x:_) -> x
  []    -> el

-- Get some text from a node, or return another default
getTextOrElse :: TE.Text -> Node -> TE.Text
getTextOrElse _  (NodeElement e)     = (nameLocalName . elementName) e
getTextOrElse el (NodeInstruction _) = el
getTextOrElse _  (NodeContent c)     = c
getTextOrElse _  (NodeComment c)     = c

-- Create a named node with some given content
defaultNodeWithContent :: TE.Text -> TE.Text -> Node
defaultNodeWithContent name content = NodeElement (
  Element {
    elementName = Name {
      nameLocalName = name,
      nameNamespace = Nothing,
      namePrefix = Nothing
    },
    elementAttributes = DM.fromList [],
    elementNodes = [NodeContent content]
  })

-- Produces this version string
-- <?xml version="1.0" encoding="UTF-8"?>
versionString :: Node
versionString = NodeElement (
  Element {
    elementName = Name {
      nameLocalName = "xml",
      nameNamespace = Nothing,
      namePrefix = Nothing
    },
    elementAttributes = DM.fromList [("version", "1.0"), ("encoding", "UTF-8")],
    elementNodes = []
  })

