module Tenpureto.TemplateLoader.Internal where

import           Data.Maybe
import           Data.List
import qualified Data.Text                     as T
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text.Prettyprint.Doc
import           Data.Yaml.Builder              ( ToYaml(..)
                                                , YamlBuilder
                                                , string
                                                , mapping
                                                , (.=)
                                                )
import           Data.Yaml.Parser               ( FromYaml(..)
                                                , YamlParser
                                                , YamlValue
                                                , withText
                                                , withMapping
                                                , (.:)
                                                )
import           Control.Applicative            ( (<|>) )

import           Tenpureto.Graph
import           Tenpureto.Effects.Git
import           Tenpureto.OrderedMap           ( OrderedMap )
import qualified Tenpureto.OrderedMap          as OrderedMap
import           Tenpureto.Orphanage            ( )

data FeatureStability = Deprecated | Experimental | Stable
        deriving (Show, Eq, Ord, Enum, Bounded)

data TemplateYamlFeature = TemplateYamlFeature
        { yamlFeatureName :: Text
        , yamlFeatureDescription :: Maybe Text
        , yamlFeatureHidden :: Bool
        , yamlFeatureStability :: FeatureStability
        }
        deriving (Show, Eq, Ord)

data TemplateYaml = TemplateYaml
        { yamlVariables :: OrderedMap Text Text
        , yamlFeatures :: Set TemplateYamlFeature
        , yamlExcludes :: Set Text
        , yamlConflicts :: Set Text
        }
        deriving (Show, Eq, Ord)

data TemplateInformation = TemplateInformation
    { branchesInformation :: [TemplateBranchInformation]
    , branchesGraph :: Graph TemplateBranchInformation
    }
    deriving Show

data TemplateBranchInformation = TemplateBranchInformation
    { branchName :: Text
    , branchCommit :: Committish
    , templateYaml :: TemplateYaml
    }
    deriving (Show, Eq, Ord)

instance Pretty TemplateBranchInformation where
    pretty cfg = (align . vsep)
        [ "Branch name:      " <+> (align . pretty) (branchName cfg)
        , "Required branches:" <+> (align . pretty) (requiredBranches cfg)
        , "Branch variables: " <+> (align . pretty) (branchVariables cfg)
        , "Description:      " <+> (align . pretty)
            (yamlFeatureDescription =<< templateYamlFeature cfg)
        , "Hidden:           "
            <+> (align . pretty) (yamlFeatureHidden <$> templateYamlFeature cfg)
        ]

instance Pretty TemplateInformation where
    pretty cfg = (align . vsep)
        ["Branches:" <+> (align . pretty) (branchesInformation cfg)]

instance Pretty TemplateYamlFeature where
    pretty feature =
        (align . vsep) ["Name:" <+> (align . pretty) (yamlFeatureName feature)]

instance Pretty TemplateYaml where
    pretty cfg = (align . vsep)
        [ "Variables:" <+> (align . pretty) (yamlVariables cfg)
        , "Features: " <+> (align . pretty) (yamlFeatures cfg)
        ]

instance FromYaml FeatureStability where
    fromYaml = withText "FeatureStability" $ \case
        "stable"       -> pure Stable
        "experimental" -> pure Experimental
        "deprecated"   -> pure Deprecated
        other -> fail ("Expected FeatureStability but got " ++ T.unpack other)

instance ToYaml FeatureStability where
    toYaml Stable       = string "stable"
    toYaml Experimental = string "experimental"
    toYaml Deprecated   = string "deprecated"

instance FromYaml TemplateYamlFeature where
    fromYaml yaml = simpleFeature yaml <|> complexFeature yaml
      where
        simpleFeature = withText "TemplateYamlFeatureName"
            $ \s -> pure (TemplateYamlFeature s Nothing False Stable)
        complexFeature = withMapping "TemplateYamlFeatureDefinition" $ \case
            [(k, v)] -> withMapping
                "TemplateYamlFeature"
                (\mm ->
                    TemplateYamlFeature k
                        <$> (mm .:? "description")
                        <*> (mm .:? "hidden" .!= False)
                        <*> (mm .:? "stability" .!= Stable)
                )
                v
            _ -> fail "Expected TemplateYamlFeatureDefinition"

instance FromYaml TemplateYaml where
    fromYaml = withMapping "TemplateYaml" $ \v ->
        TemplateYaml
            <$> (v .:? "variables" .!= OrderedMap.empty)
            <*> (v .:? "features" .!= mempty)
            <*> (v .:? "excludes" .!= mempty)
            <*> (v .:? "conflicts" .!= mempty)

instance ToYaml TemplateYamlFeature where
    toYaml TemplateYamlFeature { yamlFeatureName = n, yamlFeatureDescription = d, yamlFeatureHidden = h, yamlFeatureStability = s }
        = case
                catMaybes
                    [ "description" .?= d
                    , kvd "stability" Stable s
                    , kvd "hidden"    False  h
                    ]
            of
                []     -> string n
                fields -> mapping [(n, mapping fields)]

instance ToYaml TemplateYaml where
    toYaml TemplateYaml { yamlVariables = v, yamlFeatures = f, yamlExcludes = e, yamlConflicts = c }
        = mapping $ catMaybes
            [ kvd "variables" OrderedMap.empty v
            , "features" .?= Set.toList f
            , "excludes" .?= Set.toList e
            , "conflicts" .?= Set.toList c
            ]

(.?=) :: (ToYaml v, Eq v, Monoid v) => Text -> v -> Maybe (Text, YamlBuilder)
(.?=) a b = if b == mempty then Nothing else Just (a .= b)

(.:?) :: FromYaml a => [(Text, YamlValue)] -> Text -> YamlParser (Maybe a)
(.:?) m k = (Just <$> m .: k) <|> pure Nothing

(.!=) :: YamlParser (Maybe a) -> a -> YamlParser a
(.!=) parser def = fromMaybe def <$> parser

kvd :: (ToYaml v, Eq v) => Text -> v -> v -> Maybe (Text, YamlBuilder)
kvd a bd b = if b == bd then Nothing else Just (a .= b)

requiredBranches :: TemplateBranchInformation -> Set Text
requiredBranches = Set.map yamlFeatureName . yamlFeatures . templateYaml

branchVariables :: TemplateBranchInformation -> OrderedMap Text Text
branchVariables = yamlVariables . templateYaml

templateYamlFeature :: TemplateBranchInformation -> Maybe TemplateYamlFeature
templateYamlFeature bi = find ((==) (branchName bi) . yamlFeatureName)
    $ yamlFeatures (templateYaml bi)

branchConflicts :: TemplateBranchInformation -> Set Text
branchConflicts = yamlConflicts . templateYaml

buildGraph :: [TemplateBranchInformation] -> Graph TemplateBranchInformation
buildGraph bis =
    let
        branchNames = fmap branchName bis
        branchEdges =
            [ (branchName a, branchName b)
            | a <- bis
            , b <- bis
            , Set.member (branchName a) (requiredBranches b)
                || (  Set.isProperSubsetOf (requiredBranches a)
                                           (requiredBranches b)
                   && not (Set.member (branchName b) (requiredBranches b))
                   )
            ]
        branchNameGraph = overlay (vertices branchNames) (edges branchEdges)
        findBranchInformation name = find ((==) name . branchName) bis
    in
        filterMapVertices findBranchInformation branchNameGraph

transposeMapSet :: (Ord k, Ord v) => Map k (Set v) -> Map v (Set k)
transposeMapSet = Map.foldrWithKey combine Map.empty
  where
    combine k v acc = Set.foldr (combine' k) acc v
    combine' k v = Map.insertWith Set.union v (Set.singleton k)

commutativeConflicts
    :: [TemplateBranchInformation] -> [TemplateBranchInformation]
commutativeConflicts bis =
    let conflictsMap =
                Map.fromList [ (branchName b, branchConflicts b) | b <- bis ]
        transposedMap = transposeMapSet conflictsMap
    in  [ TemplateBranchInformation
              { branchName   = branchName b
              , branchCommit = branchCommit b
              , templateYaml = TemplateYaml
                  { yamlVariables = (yamlVariables . templateYaml) b
                  , yamlFeatures  = (yamlFeatures . templateYaml) b
                  , yamlExcludes  = (yamlExcludes . templateYaml) b
                  , yamlConflicts = (yamlConflicts . templateYaml) b
                                        <> Map.findWithDefault
                                               Set.empty
                                               (branchName b)
                                               transposedMap
                  }
              }
        | b <- bis
        ]

isFeatureBranch :: TemplateBranchInformation -> Bool
isFeatureBranch b = branchName b `Set.member` requiredBranches b

isHiddenBranch :: TemplateBranchInformation -> Bool
isHiddenBranch = maybe False yamlFeatureHidden . templateYamlFeature

isParentOf :: TemplateBranchInformation -> TemplateBranchInformation -> Bool
isParentOf child parent = branchName parent `Set.member` requiredBranches child

isMergeOf :: TemplateBranchInformation -> [TemplateBranchInformation] -> Bool
isMergeOf bi bis =
    foldMap requiredBranches bis
        == requiredBranches bi
        && all ((/=) (requiredBranches bi) . requiredBranches) bis

isMergeBranch'
    :: [TemplateBranchInformation] -> TemplateBranchInformation -> Bool
isMergeBranch' bis b = any (isMergeOf b) mergeOptions
  where
    fb           = filter (isParentOf b) $ filter isFeatureBranch bis
    mergeOptions = filter ((<) 1 . length) (subsequences fb)

isMergeBranch :: TemplateInformation -> TemplateBranchInformation -> Bool
isMergeBranch t = isMergeBranch' (branchesInformation t)

managedBranches :: TemplateInformation -> [TemplateBranchInformation]
managedBranches t = filter (\b -> isFeatureBranch b || isMergeBranch t b)
                           (branchesInformation t)

templateInformation :: [TemplateBranchInformation] -> TemplateInformation
templateInformation branches =
    let isManagedBranch b = isFeatureBranch b || isMergeBranch' branches b
        enrichedBranches =
                (commutativeConflicts . filter isManagedBranch) branches
    in  TemplateInformation enrichedBranches (buildGraph enrichedBranches)
