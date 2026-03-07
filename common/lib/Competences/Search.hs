module Competences.Search
  ( -- * Query types
    Term (..)
  , Clause
  , Query

    -- * Parsing
  , parseQuery

    -- * Matching
  , matchItem
  , matchTerm
  )
where

import Data.Text (Text)
import Data.Text qualified as T

-- | A single search term: either a text substring match or a metadata filter.
data Term
  = TextTerm !Text
  | MetaTerm !Text
  deriving (Eq, Show)

-- | A clause is a conjunction (AND) of terms.
type Clause = [Term]

-- | A query is a disjunction (OR) of clauses — disjunctive normal form.
type Query = [Clause]

-- | Parse a search query in disjunctive normal form.
--
-- Splits on @,@ or @|@ (OR), then within each clause splits on @&@ (AND).
-- Terms starting with @\@@ are metadata filters; others are text searches.
-- Whitespace is trimmed. Empty terms and clauses are ignored.
--
-- Examples:
--
-- @
-- parseQuery ""             == []
-- parseQuery "Iso"          == [[TextTerm "Iso"]]
-- parseQuery "Iso, Trist"   == [[TextTerm "Iso"], [TextTerm "Trist"]]
-- parseQuery "\@06.03 \@hü" == [[MetaTerm "06.03", MetaTerm "hü"]]
-- parseQuery "Iso & \@m"    == [[TextTerm "Iso", MetaTerm "m"]]
-- @
parseQuery :: Text -> Query
parseQuery input =
  let orParts = T.splitOn "," input >>= T.splitOn "|"
   in filter (not . null) $ map parseClause orParts
  where
    parseClause :: Text -> Clause
    parseClause clauseText =
      let parts = T.splitOn "&" clauseText
       in concatMap parseTerms parts

    parseTerms :: Text -> [Term]
    parseTerms part =
      let stripped = T.strip part
       in if T.null stripped
            then []
            else case T.splitOn "@" stripped of
              [] -> []
              (x : xs) ->
                [TextTerm (T.strip x) | not (T.null (T.strip x))]
                  <> [MetaTerm (T.strip m) | m <- xs, not (T.null (T.strip m))]

-- | Check whether an item matches a query.
--
-- An empty query matches everything.
-- Otherwise, the item must match at least one clause (OR).
-- Within a clause, all terms must match (AND).
matchItem ::
  -- | Extract label text for 'TextTerm' matching
  (a -> Text) ->
  -- | Extract metadata values for 'MetaTerm' matching
  (a -> [Text]) ->
  -- | The parsed query
  Query ->
  -- | The item to test
  a ->
  Bool
matchItem _ _ [] _ = True
matchItem label metadata query item =
  any (all (matchTerm label metadata item)) query

-- | Check whether a single term matches an item.
matchTerm ::
  (a -> Text) ->
  (a -> [Text]) ->
  a ->
  Term ->
  Bool
matchTerm label _ item (TextTerm t) =
  T.toLower t `T.isInfixOf` T.toLower (label item)
matchTerm _ metadata item (MetaTerm t) =
  any (\v -> T.toLower t `T.isInfixOf` T.toLower v) (metadata item)
