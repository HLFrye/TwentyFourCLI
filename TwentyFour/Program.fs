open System

let rec getSwaps maxIndex =
    seq {
        if maxIndex = 1 then
            yield (0, 1)
        else
            yield! getSwaps (maxIndex-1)
            for i = 0 to maxIndex - 1 do
                match maxIndex % 2 with
                | 0 -> yield (0, maxIndex)
                | 1 -> yield (i, maxIndex)
                yield! getSwaps (maxIndex-1)
    }

let permutations values =
    let swaps = 
        values
        |> Array.length
        |> (+) -1
        |> getSwaps

    seq {
        yield Array.copy values
        for swap in swaps do
            let temp = values.[fst swap]
            values.[fst swap] <- values.[snd swap]
            values.[snd swap] <- temp
            yield Array.copy values
    }
    
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
    | Constant x -> double x 
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
    
let constants = Seq.map Constant
    
let rec buildAllExpressions expressions count expressionList =
    seq { 
        for expr in expressions do
            let nextList = (List.Cons (expr, expressionList))
            match count with
            | 1 -> yield nextList
            | _ -> yield! buildAllExpressions expressions (count - 1) nextList
    }

let buildAllTrees (opers:list<expr * expr -> expr>) (constants:list<expr>) = 
    let (oper1, oper2, oper3) = (opers.[0], opers.[1], opers.[2])
    let (constant1, constant2, constant3, constant4) = (constants.[0], constants.[1], constants.[2], constants.[3])
    let tree1 = oper3(oper1(constant1, constant2), oper2(constant3, constant4))
    let tree2 = oper3(oper2(oper1(constant1, constant2), constant3), constant4)
    let tree3 = oper3(constant1, oper2(constant2, oper1(constant3, constant4)))
    let tree4 = oper3(oper2(constant1, oper1(constant2, constant3)), constant4)
    let tree5 = oper3(constant1, oper2(oper1(constant2, constant3), constant4))

    [tree1;tree2;tree3;tree4;tree5]

let findAnswers goal inputs =
    let allConstants = 
        inputs
        |> permutations
        |> Seq.distinct
        |> Seq.map constants

    let allExpressions =
        buildAllExpressions [Sum;Quot;Diff;Prod;Power] 3 []
    
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
    |> Seq.filter (calculateExpression >> (-) goal >> Math.Abs >> (>) 0.000001)
    |> Seq.distinct
    
let Solve = findAnswers 24.0
         
[<EntryPoint>]
let main argv =
    let results = 
        argv
        |> Array.map int
        |> Solve

    printf "Found %d answers\n" (Seq.length results)
    for answer in results do
        printf "%s\n" (formatExpression answer)
    0
