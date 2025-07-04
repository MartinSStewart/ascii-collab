module NoUselessLeftPizza exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node exposing (Node(..))
import Review.Rule as Rule exposing (Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoUselessLeftPizza" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    {}


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator <| \() -> {}


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor (Node range node) context =
    case node of
        OperatorApplication "<|" _ _ (Node _ right) ->
            case right of
                Application (_ :: _ :: _) ->
                    ( [], context )

                OperatorApplication _ _ _ _ ->
                    ( [], context )

                IfBlock _ _ _ ->
                    ( [], context )

                LambdaExpression _ ->
                    ( [], context )

                LetExpression _ ->
                    ( [], context )

                CaseExpression _ ->
                    ( [], context )

                _ ->
                    ( [ Rule.error
                            { message = "Forbidden pizza!"
                            , details = [ "This left pizza is redundant" ]
                            }
                            range
                      ]
                    , context
                    )

        _ ->
            ( [], context )
