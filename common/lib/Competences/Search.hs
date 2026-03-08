module Competences.Search
  ( -- * Query types
    Term (..)
  , Clause
  , Query

    -- * Parsing
  , parseQuery

    -- * Matching (metadata as text lists)
  , matchItem
  , matchTerm

    -- * Matching (typed meta filters)
  , resolveMetaTerm
  , isClauseValid
  , matchItemWithFilters
  , matchTermResolved
  , unresolvedTerms

    -- * Query segmentation (for inline highlighting)
  , QuerySegment (..)
  , segmentQuery
  )
where

import Data.Maybe (isJust, isNothing, listToMaybe, mapMaybe)
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

-- ============================================================================
-- Typed meta filters
-- ============================================================================

-- | Try filters in order, return first successful parse.
resolveMetaTerm :: [Text -> Maybe (a -> Bool)] -> Text -> Maybe (a -> Bool)
resolveMetaTerm filters t = listToMaybe $ mapMaybe (\f -> f t) filters

-- | A clause is valid if all its MetaTerms resolve.
isClauseValid :: [Text -> Maybe (a -> Bool)] -> Clause -> Bool
isClauseValid filters = all termValid
  where
    termValid (TextTerm _) = True
    termValid (MetaTerm t) = isJust (resolveMetaTerm filters t)

-- | Match with typed meta filters. Invalid clauses (containing
-- unresolvable @-terms) are dropped. If no valid clauses remain,
-- everything matches (like an empty query).
matchItemWithFilters :: (a -> Text) -> [Text -> Maybe (a -> Bool)] -> Query -> a -> Bool
matchItemWithFilters _ _ [] _ = True
matchItemWithFilters label filters query item =
  let valid = filter (isClauseValid filters) query
   in null valid || any (all (matchTermResolved label filters item)) valid

-- | Match a single term using typed filters for MetaTerms.
matchTermResolved :: (a -> Text) -> [Text -> Maybe (a -> Bool)] -> a -> Term -> Bool
matchTermResolved label _ item (TextTerm t) =
  T.toLower t `T.isInfixOf` T.toLower (label item)
matchTermResolved _ filters item (MetaTerm t) =
  case resolveMetaTerm filters t of
    Just p -> p item
    Nothing -> True -- shouldn't happen (already filtered by isClauseValid)

-- | Collect unresolved meta terms from a query (for UI feedback).
unresolvedTerms :: [Text -> Maybe (a -> Bool)] -> Query -> [Text]
unresolvedTerms filters query =
  [t | clause <- query, MetaTerm t <- clause, isNothing (resolveMetaTerm filters t)]

-- ============================================================================
-- Query segmentation (for inline highlighting)
-- ============================================================================

-- | A segment of the raw query text, preserving the original characters exactly.
data QuerySegment
  = PlainText !Text
  | ResolvedFilter !Text
  | UnresolvedFilter !Text
  deriving (Eq, Show)

-- | Split raw query text into typed segments for inline highlighting.
--
-- Preserves the exact original text (whitespace, @, separators).
-- Each @-filter segment includes the @ prefix.
segmentQuery :: [Text -> Maybe (a -> Bool)] -> Text -> [QuerySegment]
segmentQuery filters input =
  case T.splitOn "@" input of
    [] -> []
    (first : rest) ->
      [PlainText first | not (T.null first)]
        <> concatMap processAtPart rest
  where
    -- Characters that end a filter term
    isSeparator c = c == ' ' || c == '@' || c == '&' || c == ',' || c == '|'

    processAtPart part
      | T.null part = [PlainText "@"]
      | otherwise =
          let (term, remainder) = T.span (not . isSeparator) part
           in if T.null term
                then PlainText "@" : [PlainText remainder | not (T.null remainder)]
                else
                  let filterSeg =
                        if isJust (resolveMetaTerm filters term)
                          then ResolvedFilter ("@" <> term)
                          else UnresolvedFilter ("@" <> term)
                   in filterSeg : [PlainText remainder | not (T.null remainder)]
