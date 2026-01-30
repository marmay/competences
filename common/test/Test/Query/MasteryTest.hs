-- | Property-based and example tests for the Mastery classification system.
--
-- All tests operate at the ObservationTimeline level:
-- - Input:  [[LevelObservation]] (timeline of evidences, newest first)
-- - Output: Map Level MasteryStatus  (via classifyAllLevels)
--
-- This is the right abstraction for expressing cross-level properties like
-- "failing at Basic doesn't affect Advanced mastery."
module Test.Query.MasteryTest (tests) where

import Competences.Document.Competence (Level (..), allLevels)
import Competences.Document.Evidence (Ability (..), SocialForm (..))
import Competences.Query.Mastery
import Data.Map.Strict qualified as Map
import Test.Generators ()
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Mastery"
    [ classifierExamples
    , classifierProperties
    ]

-- | Helper: lookup mastery at a level, defaulting to NotTried.
masteryAt :: Level -> ObservationTimeline -> MasteryStatus
masteryAt lvl timeline = Map.findWithDefault NotTried lvl (classifyAllLevels timeline)

-- | Helper: construct a single observation.
obs :: Level -> Ability -> SocialForm -> LevelObservation
obs = LevelObservation

-- | Generate a timeline where all observations use Group social form.
genAllGroupTimeline :: Gen ObservationTimeline
genAllGroupTimeline = listOf (listOf genGroupObs)
  where
    genGroupObs = LevelObservation <$> arbitrary <*> arbitrary <*> pure Group

-- | Generate a timeline with no observations at the given level.
genTimelineWithout :: Level -> Gen ObservationTimeline
genTimelineWithout targetLevel = listOf (listOf genObs)
  where
    otherLevels = filter (/= targetLevel) allLevels
    genObs = LevelObservation <$> elements otherLevels <*> arbitrary <*> arbitrary

-- ============================================================================
-- Part A: Concrete Examples
-- ============================================================================

classifierExamples :: TestTree
classifierExamples =
  testGroup
    "examples"
    [ testCase "no observations -> all NotTried" $
        classifyAllLevels []
          @?= Map.fromList
            [ (BasicLevel, NotTried)
            , (IntermediateLevel, NotTried)
            , (AdvancedLevel, NotTried)
            ]
    , testCase "single SelfReliant Individual at Basic -> Basic=OneSuccess, others NotTried" $
        let timeline = [[obs BasicLevel SelfReliant Individual]]
         in do
              masteryAt BasicLevel timeline @?= OneSuccess
              masteryAt IntermediateLevel timeline @?= NotTried
              masteryAt AdvancedLevel timeline @?= NotTried
    , testCase "two SelfReliant at Basic (one Individual) -> StreakTwoPlus" $
        let timeline =
              [ [obs BasicLevel SelfReliant Individual]
              , [obs BasicLevel SelfReliant Group]
              ]
         in masteryAt BasicLevel timeline @?= StreakTwoPlus
    , testCase "two SelfReliant at Basic (all Group) -> OneSuccess (Individual required for +2)" $
        let timeline =
              [ [obs BasicLevel SelfReliant Group]
              , [obs BasicLevel SelfReliant Group]
              ]
         in masteryAt BasicLevel timeline @?= OneSuccess
    , testCase "fail at Basic -> Basic=MasteryNotYet" $
        let timeline = [[obs BasicLevel WithSupport Group]]
         in masteryAt BasicLevel timeline @?= MasteryNotYet
    , testCase "fail at Basic -> Intermediate also affected (cross-level ceiling)" $
        let timeline =
              [ [obs BasicLevel WithSupport Group, obs IntermediateLevel SelfReliant Individual]
              ]
         in do
              masteryAt BasicLevel timeline @?= MasteryNotYet
              -- Intermediate ceiling includes Basic's WithSupport (below target)
              masteryAt IntermediateLevel timeline @?= MasteryNotYet
    , testCase "fail at Advanced only -> Basic unaffected" $
        let timeline = [[obs AdvancedLevel NotYet Group]]
         in do
              masteryAt BasicLevel timeline @?= NotTried
              masteryAt AdvancedLevel timeline @?= MasteryNotYet
    , testCase "succeed at Basic -> Advanced unaffected" $
        let timeline = [[obs BasicLevel SelfReliant Individual]]
         in masteryAt AdvancedLevel timeline @?= NotTried
    , testCase "only SillyMistakes -> OnlySillyMistakes" $
        let timeline = [[obs BasicLevel SelfReliantWithSillyMistakes Group]]
         in masteryAt BasicLevel timeline @?= OnlySillyMistakes
    , testCase "succeed at Intermediate clears older Basic failure" $
        let timeline =
              [ [obs IntermediateLevel SelfReliant Individual]
              , [obs BasicLevel WithSupport Group]
              ]
         in masteryAt BasicLevel timeline @?= OneSuccess
    , testCase "SillyMistakes between two SelfReliant -> streak not broken" $
        let timeline =
              [ [obs BasicLevel SelfReliant Individual]
              , [obs BasicLevel SelfReliantWithSillyMistakes Group]
              , [obs BasicLevel SelfReliant Group]
              ]
         in masteryAt BasicLevel timeline @?= StreakTwoPlus
    ]

-- ============================================================================
-- Part B: QuickCheck Properties
-- ============================================================================

classifierProperties :: TestTree
classifierProperties =
  testGroup
    "properties"
    [ -- P1: Failing at target level sets it to MasteryNotYet
      testProperty "P1: fail at target level -> MasteryNotYet" $
        \(timeline :: ObservationTimeline) ->
          forAll (elements [WithSupport, NotYet]) $ \failAbility ->
            forAll arbitrary $ \targetLevel ->
              let newEvidence = [obs targetLevel failAbility Group]
                  result = masteryAt targetLevel (newEvidence : timeline)
               in result === MasteryNotYet
    , -- P2: Within-evidence ceiling — failure at a lower level implies failure
      -- at higher levels observed in the same evidence.
      testProperty "P2: within-evidence fail low -> higher level also MasteryNotYet" $
        \(timeline :: ObservationTimeline) ->
          forAll (elements [WithSupport, NotYet]) $ \failAbility ->
            forAll arbitrary $ \highAbility ->
              forAll (elements [BasicLevel, IntermediateLevel]) $ \lowLevel ->
                let higherLevels = filter (> lowLevel) allLevels
                 in forAll (elements higherLevels) $ \highLevel ->
                      forAll arbitrary $ \sf1 ->
                        forAll arbitrary $ \sf2 ->
                          let newEvidence = [obs lowLevel failAbility sf1, obs highLevel highAbility sf2]
                              result = classifyAllLevels (newEvidence : timeline)
                           in counterexample ("low=" ++ show (Map.lookup lowLevel result) ++ " high=" ++ show (Map.lookup highLevel result)) $
                                Map.findWithDefault NotTried highLevel result === MasteryNotYet
    , -- P3: Failing at a higher level does not affect lower levels
      testProperty "P3: fail high -> no effect on lower levels" $
        \(timeline :: ObservationTimeline) ->
          forAll (elements [WithSupport, NotYet]) $ \failAbility ->
            forAll (elements [IntermediateLevel, AdvancedLevel]) $ \highLevel ->
              let before = classifyAllLevels timeline
                  newEvidence = [obs highLevel failAbility Group]
                  now = classifyAllLevels (newEvidence : timeline)
                  lowerLevels = filter (< highLevel) allLevels
               in conjoin
                    [ counterexample ("level " ++ show lvl ++ ": before=" ++ show (Map.lookup lvl before) ++ " after=" ++ show (Map.lookup lvl now)) $
                        Map.findWithDefault NotTried lvl now === Map.findWithDefault NotTried lvl before
                    | lvl <- lowerLevels
                    ]
    , -- P4: Succeeding at a lower level does not affect higher levels
      testProperty "P4: succeed low -> no effect on higher levels" $
        \(timeline :: ObservationTimeline) ->
          forAll arbitrary $ \socialForm ->
            forAll (elements [BasicLevel, IntermediateLevel]) $ \lowLevel ->
              let before = classifyAllLevels timeline
                  newEvidence = [obs lowLevel SelfReliant socialForm]
                  now = classifyAllLevels (newEvidence : timeline)
                  higherLevels = filter (> lowLevel) allLevels
               in conjoin
                    [ counterexample ("level " ++ show lvl ++ ": before=" ++ show (Map.lookup lvl before) ++ " after=" ++ show (Map.lookup lvl now)) $
                        Map.findWithDefault NotTried lvl now === Map.findWithDefault NotTried lvl before
                    | lvl <- higherLevels
                    ]
    , -- P5: Success never lowers mastery at the same level
      -- MasteryStatus Ord: StreakTwoPlus < OneSuccess < OnlySillyMistakes < MasteryNotYet < NotTried
      testProperty "P5: success never lowers mastery at target level" $
        \(timeline :: ObservationTimeline) ->
          forAll arbitrary $ \targetLevel ->
            forAll (elements [SelfReliant, SelfReliantWithSillyMistakes]) $ \ability ->
              forAll arbitrary $ \socialForm ->
                let before = masteryAt targetLevel timeline
                    newEvidence = [obs targetLevel ability socialForm]
                    now = masteryAt targetLevel (newEvidence : timeline)
                 in counterexample ("before=" ++ show before ++ " after=" ++ show now) $
                      now <= before
    , -- P6: SelfReliant at target level gives at least OneSuccess
      testProperty "P6: SelfReliant at target -> at least OneSuccess" $
        \(timeline :: ObservationTimeline) ->
          forAll arbitrary $ \targetLevel ->
            forAll arbitrary $ \socialForm ->
              let newEvidence = [obs targetLevel SelfReliant socialForm]
                  result = masteryAt targetLevel (newEvidence : timeline)
               in counterexample ("result=" ++ show result) $
                    result <= OneSuccess
    , -- P7: Success at a higher level implies success at all lower levels.
      -- SelfReliant at a level above target gives at least OneSuccess at target.
      testProperty "P7: SelfReliant above target -> at least OneSuccess at target" $
        \(timeline :: ObservationTimeline) ->
          forAll (elements [IntermediateLevel, AdvancedLevel]) $ \highLevel ->
            forAll arbitrary $ \socialForm ->
              let lowerLevels = filter (< highLevel) allLevels
                  newEvidence = [obs highLevel SelfReliant socialForm]
                  result = classifyAllLevels (newEvidence : timeline)
               in conjoin
                    [ counterexample ("level " ++ show lvl ++ " = " ++ show (Map.findWithDefault NotTried lvl result)) $
                        Map.findWithDefault NotTried lvl result <= OneSuccess
                    | lvl <- lowerLevels
                    ]
    , -- P8: MasteryNotYet and OnlySillyMistakes require direct observation.
      -- Without any observation at the target level, mastery is never negative.
      testProperty "P8: no direct observation -> never MasteryNotYet or OnlySillyMistakes" $
        forAll arbitrary $ \targetLevel ->
          forAll (genTimelineWithout targetLevel) $ \timeline ->
            let result = masteryAt targetLevel timeline
             in counterexample ("result=" ++ show result) $
                  result =/= MasteryNotYet .&&. result =/= OnlySillyMistakes
    , -- P9: SillyMistakes don't move mastery from OneSuccess to MasteryNotYet.
      -- Generate a timeline that starts at OneSuccess (single SelfReliant at target).
      testProperty "P9: SillyMistakes never causes MasteryNotYet from OneSuccess" $
        forAll arbitrary $ \targetLevel ->
          forAll arbitrary $ \socialForm1 ->
            forAll arbitrary $ \socialForm2 ->
              let baseline = [[obs targetLevel SelfReliant socialForm1]]
                  newEvidence = [obs targetLevel SelfReliantWithSillyMistakes socialForm2]
                  before = masteryAt targetLevel baseline
                  now = masteryAt targetLevel (newEvidence : baseline)
               in counterexample ("before=" ++ show before ++ " after=" ++ show now) $
                    before === OneSuccess .&&. now =/= MasteryNotYet
    , -- P10: All Group observations -> no level can be StreakTwoPlus.
      -- Generate timelines where all observations use Group social form.
      testProperty "P10: all-Group observations -> no StreakTwoPlus" $
        forAll genAllGroupTimeline $ \timeline ->
          let result = classifyAllLevels timeline
           in conjoin
                [ counterexample ("level " ++ show lvl ++ " = " ++ show status) $
                    status =/= StreakTwoPlus
                | (lvl, status) <- Map.toList result
                ]
    , -- Smoke test: classifyAllLevels never crashes
      testProperty "smoke: classifyAllLevels terminates for any input" $
        \(timeline :: ObservationTimeline) ->
          let result = classifyAllLevels timeline
           in Map.size result `seq` True
    ]
