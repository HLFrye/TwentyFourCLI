type expr = 
| Constant of int
| Sum of expr * expr
| Diff of expr * expr
| Prod of expr * expr
| Quot of expr * expr
| Power of expr * expr

let reverseDiff (x, y) = Diff (y, x)
let reverseQuot (x, y) = Quot (y, x)
let reversePow (x, y) = Power (y, x)

let operators = [Sum; Diff; Prod; Quot; Power; reverseDiff; reverseQuot; reversePow]

let rec calculateExpression expr =
    match expr with
    | Constant x -> float x 
    | Sum (x, y) -> (calculateExpression x) + (calculateExpression y)
    | Diff (x, y) -> (calculateExpression x) - (calculateExpression y)
    | Prod (x, y) -> (calculateExpression x) * (calculateExpression y)
    | Quot (x, y) -> (calculateExpression x) / (calculateExpression y)
    | Power (x, y) -> System.Math.Pow((calculateExpression x), (calculateExpression y))
    
let split choices = 
    let indexed = Seq.indexed choices
    indexed 
    |> Seq.map (fun (idx, choice) -> 
        choice, indexed 
        |> Seq.filter (fun (idx2, choice2) ->
            idx2 <> idx)
        |> Seq.map (fun (idx, choice) -> choice))

let getNext (accumulation, choices) =
    choices
    |> split
    |> Seq.map (fun (nextItem, nextChoices) ->
        (nextItem
        |> Seq.singleton
        |> Seq.append accumulation, nextChoices))
       

let rec permutations values =
    values
    |> Seq.map (fun (curr, choices) ->
        match Seq.length choices with
        | 0 -> curr
            |> Seq.singleton
        | _ -> (curr, choices)
            |> getNext
            |> permutations)
    |> Seq.concat

let constants digits =
    digits 
    |> Seq.map Constant

let tuple first second = 
    first, second

let applyOperator combiner input operator =
    let firstExpr = input |> Seq.item 0 
    let secondExpr = input |> Seq.item 1
    let newExpr = operator (firstExpr, secondExpr)
    newExpr
    |> Seq.singleton
    |> combiner (input |> Seq.skip 2)

let applyOperatorBack = applyOperator Seq.append
let applyOperatorFront = applyOperator (fun x y -> Seq.append y x) 

let rec subExpressions (input:seq<expr>):seq<expr> =
    let applyAllOperators applyFn operators = 
        operators
        |> Seq.map (applyFn input)
        |> Seq.map subExpressions
        |> Seq.concat

    match Seq.length input with
    | 1 -> input 
    | _ -> operators
           |> applyAllOperators applyOperatorBack
           |> Seq.append (applyAllOperators applyOperatorFront operators)

let rec expressions (inputs:seq<seq<expr>>):seq<expr> =
    inputs
    |> Seq.map subExpressions
    |> Seq.concat
    
let findAnswers goal inputs =
    inputs
    |> tuple Seq.empty<int>
    |> Seq.singleton 
    |> permutations 
    |> Seq.map constants
    |> expressions
    |> Seq.map (fun expr -> expr, (calculateExpression expr))
    |> Seq.filter (fun (_, x) -> x = goal)
    |> Seq.map (fun (x, _) -> x)
    |> Seq.distinct

let buildAllExpressions expressions =
    seq { for expr1 in expressions do
          yield! seq {for expr2 in expressions do
                      yield! seq {for expr3 in expressions do
                                  yield [expr1;expr2;expr3] }}}

let buildAllTrees (opers:list<expr * expr -> expr>) (constants:list<expr>) = 
    let (oper1, oper2, oper3) = (opers.[0], opers.[1], opers.[2])
    let (constant1, constant2, constant3, constant4) = (constants.[0], constants.[1], constants.[2], constants.[3])
    let tree1 = oper3(oper1(constant1, constant2), oper2(constant3, constant4))
    let tree2 = oper3(oper2(oper1(constant1, constant2), constant3), constant4)
    let tree3 = oper3(constant1, oper2(constant2, oper3(constant3, constant4)))
    let tree4 = oper3(oper2(constant1, oper1(constant2, constant3)), constant4)
    let tree5 = oper3(constant1, oper2(oper1(constant2, constant3), constant4))

    [tree1;tree2;tree3;tree4;tree5]


let findAnswers2 goal inputs =
    let allConstants = 
        inputs
        |> tuple Seq.empty<int>
        |> Seq.singleton
        |> permutations
        |> Seq.distinct
        |> Seq.map constants

    let allExpressions =
        buildAllExpressions [Sum;Quot;Diff;Prod;Power]
    
    let allTrees =
        allConstants
        |> Seq.map Seq.toList
        |> Seq.map (fun constants ->
            allExpressions
            |> Seq.map (fun expressions ->
                buildAllTrees expressions constants)
            |> Seq.concat)
        |> Seq.concat
            
    allTrees
    |> Seq.map (fun expr -> expr, (calculateExpression expr))
    |> Seq.filter (fun (_, x) -> x = goal)
    |> Seq.map (fun (x, _) -> x)
    |> Seq.distinct



let buildBalancedTree (opers:list<expr * expr -> expr>) (constants:list<expr>) =
    opers.[2](opers.[0](constants.[0], constants.[1]),opers.[1](constants.[2], constants.[3]))




let Solve (a,b,c,d) =
    findAnswers 24.0 [a;b;c;d]
         