module NoLongImportLines exposing (rule)

{-| Some Elm tooling will always change `import` lines from multi-line to
single-line when updating an `import` statement, even when the single line
would be extremely long and prone to merge conflicts.

This rule enforces that no single-line `import` statements longer then 120
characters should exist.


# Usage

After adding [`elm-review`][elm-review] to your project, import this rule from
your `ReviewConfig.elm` file and add it to the config. E.g.:

    import NoLongImportLines
    import Review.Rule exposing (Rule)

    config : List Rule
    config =
        [ NoLongImportLines.rule ]

[elm-review]: https://package.elm-lang.org/packages/jfmengels/elm-review/latest/

@docs rule

-}

import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Maybe.Extra as ME
import Review.Fix exposing (Fix, replaceRangeBy)
import Review.Rule as Rule exposing (Error, Rule, errorWithFix)



-- Create a new rule


{-| A rule for elm-review that discourages the use of very long `import`
statements, which are prone to merge conflicts.

    import NoLongImportLines
    import Review.Rule exposing (Rule)

    config : List Rule
    config =
        [ NoLongImportLines.rule
        ]

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoLongImportLines" ()
        |> Rule.withSimpleImportVisitor importVisitor
        |> Rule.fromModuleRuleSchema


details =
    { message =
        "Do not keep `import` lines longer than 120 characters."
    , details =
        [ "Even though `elm-format` keeps the items sorted, having very long"
            ++ " lines like this invites merge conflicts, and reduces"
            ++ " readability."
        ]
    }


importVisitor : Node Import -> List (Error {})
importVisitor node =
    let
        range : Range
        range =
            node |> Node.range

        isSingleLine : Bool
        isSingleLine =
            range.start.row == range.end.row

        exceedsMaxLength : Bool
        exceedsMaxLength =
            range.end.column > 120
    in
    if isSingleLine && exceedsMaxLength && canBeChopped node then
        [ errorWithFix details (Node.range node) (chopDownLine node)
        ]

    else
        []


canBeChopped : Node Import -> Bool
canBeChopped node =
    case node |> Node.value |> .exposingList |> Maybe.map Node.value of
        Just (Exposing.All _) ->
            False

        Just (Exposing.Explicit _) ->
            True

        Nothing ->
            False


chopDownLine : Node Import -> List Fix
chopDownLine node =
    let
        importNode : Import
        importNode =
            Node.value node

        exposingNode : Maybe (Node Exposing)
        exposingNode =
            importNode.exposingList
    in
    case exposingNode of
        Just (Node.Node _ (Exposing.All _)) ->
            -- elm-format doesn't permit multiline `import _ exposing (..)`
            []

        Just (Node.Node exposingRange (Exposing.Explicit nodes)) ->
            let
                nodeTexts : List String
                nodeTexts =
                    nodes |> List.map (Node.value >> topLevelExposeToLiteral)
            in
            [ replaceRangeBy exposingRange ("""
    exposing
        ( """ ++ (nodeTexts |> String.join "\n        , ") ++ "\n        )") ]

        Nothing ->
            []


topLevelExposeToLiteral : Exposing.TopLevelExpose -> String
topLevelExposeToLiteral topLevelExpose =
    case topLevelExpose of
        Exposing.InfixExpose string ->
            string

        Exposing.FunctionExpose string ->
            string

        Exposing.TypeOrAliasExpose string ->
            string

        Exposing.TypeExpose exposedType ->
            exposedType.name ++ (exposedType.open |> ME.unwrap "" (always "(..)"))
