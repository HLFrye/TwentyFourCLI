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

type expr = 
| Constant of int
| Sum of expr * expr
| Diff of expr * expr
| Prod of expr * expr
| Quot of expr * expr
| Power of expr * expr

let operators = [Sum; Diff; Prod; Quot; Power]

let rec calculateExpression expr =
    match expr with
    | Constant x -> float x 
    | Sum (x, y) -> (calculateExpression x) + (calculateExpression y)
    | Diff (x, y) -> (calculateExpression x) - (calculateExpression y)
    | Prod (x, y) -> (calculateExpression x) * (calculateExpression y)
    | Quot (x, y) -> (calculateExpression x) / (calculateExpression y)
    | Power (x, y) -> (calculateExpression x) ** (calculateExpression y)

let rec formatExpression expr =
    match expr with
    | Constant x -> sprintf "%d" x
    | Sum (x, y) -> sprintf "(%s + %s)" (formatExpression x) (formatExpression y)
    | Diff (x, y) -> sprintf "(%s - %s)" (formatExpression x) (formatExpression y)
    | Prod (x, y) -> sprintf "(%s * %s)" (formatExpression x) (formatExpression y)
    | Quot (x, y) -> sprintf "(%s / %s)" (formatExpression x) (formatExpression y)
    | Power (x, y) -> sprintf "(%s ^ %s)" (formatExpression x) (formatExpression y)
    
let constants digits =
    digits 
    |> Seq.map Constant

let tuple first second = 
    first, second

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

let findAnswers goal inputs =
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

let Solve (a,b,c,d) =
    findAnswers 24.0 [a;b;c;d]
         
[<EntryPoint>]
let main argv =
    let num1 = int argv.[0]
    let num2 = int argv.[1]
    let num3 = int argv.[2]
    let num4 = int argv.[3]
    let results = Solve (num1,num2,num3,num4)
    printf "Found %d answers\n" (Seq.length results)
    for answer in results do
        printf "%s\n" (formatExpression answer)
    1
