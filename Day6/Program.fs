let inputData = System.IO.File.ReadAllText("./input.txt")

let testInput = """123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
"""

type Token =
    | Number of int64
    | Operator of string

    with 
        member this.getOperator = 
            match this with 
            | Operator s -> s
            | _ -> failwith $"Invalid token: {this}"

        member this.getValue = 
            match this with 
            | Number i -> i
            | _ -> failwith $"Invalid number : {this}"

let parseToken (t: string) =
    match System.Int32.TryParse t with
    | true, v -> Number v
    | false, _ -> Operator t

let buildTokens (input: string) =
    input.Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray
    |> List.map(fun s -> s.Split([|" "|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray) 
    |> List.transpose
    |> List.map (fun l -> l |> List.map parseToken)

let part_1 (input : string) = 
    input
    |> buildTokens
    |> List.map (fun tokens ->
        let operator = tokens[tokens.Length-1].getOperator
        let values = tokens[0..tokens.Length-2] |> List.map _.getValue
        let mutable lineTotal = 0L

        match operator with 
        | "*" -> 
            lineTotal <- 1L
            values |> List.iter (fun i -> lineTotal <- i * lineTotal)
        | "+" -> 
            values |> List.iter (fun i -> lineTotal <- i + lineTotal)
        | s -> failwith $"Invalid operator : {s}" 

        lineTotal
     )
     |> List.sum

[<EntryPoint>]
let main _ = 
    printf $"Part 1 Test : %i{part_1 testInput}\n"
    printf $"Part 1      : %i{part_1 inputData}\n"
    0