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

type Row = {Numbers : List<int64>; Operator : string}

let isOperator s = 
    match s with 
    | '+' | '*' -> true
    | _ -> false

let parseToken (t: string) =
    match System.Int32.TryParse t with
    | true, v -> Number v
    | false, _ -> Operator t

let part_1 (input : string) = 
    input.Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray
    |> List.map(fun s -> s.Split([|" "|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray) 
    |> List.transpose
    |> List.map (List.map parseToken)
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

let part_2 (input : string) = 
    let lines =
        input.Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> s.TrimEnd('\n', '\r'))

    let height = lines.Length
    let width =
        lines
        |> Array.map _.Length
        |> Array.max

    let paddedLines =
        lines
        |> Array.map (fun s -> if s.Length < width then s.PadRight(width, ' ') else s)

    let columns : char list list =
        [0 .. width - 1]
        |> List.map (fun col ->
            [0 .. height - 1]
            |> List.map (fun row -> paddedLines[row][col]))

    let chunkPredicate = 
        [0..columns.[0].Length - 1]
        |> List.map (fun _ -> ' ')

    let rec parseRows (previousNumbers : List<int64>) (rows : List<Row>) (grid : List<List<char>>) : List<Row> =

        let filterAndMap (row : List<char>) = 
            row 
            |> List.filter (fun c -> c <> ' ') 
            |> List.toArray
            |> fun seq -> System.String seq

        match grid with 
        | h :: t -> 
            if h = chunkPredicate then 
                parseRows [] rows t
            else 
                if (h[h.Length - 1] |> isOperator) then 
                    let number = int64 (h[0..h.Length - 2] |> filterAndMap)
                    let operator = h[h.Length - 1] |> string
                    let row = {Numbers = previousNumbers @ [number]; Operator = operator}
                    parseRows [] (rows @ [row]) t
                else 
                    let number = int64 (h[0..h.Length - 2] |> filterAndMap)
                    parseRows (previousNumbers @ [number]) rows t
        | _ -> rows

    let calculateRow (row: Row) = 
        let mutable rowTotal = 0L

        match row.Operator with 
        | "+" -> row.Numbers |> List.iter (fun i -> rowTotal <- i + rowTotal)
        | "*" -> 
            rowTotal <- 1L
            row.Numbers |> List.iter (fun i -> rowTotal <- i * rowTotal)
        | s -> failwith $"Invalid operator {s}"

        rowTotal

    columns
    |> List.rev
    |> parseRows List.empty List.empty
    |> List.map calculateRow
    |> List.sum

[<EntryPoint>]
let main _ = 
    printf $"Part 1 Test : %i{part_1 testInput}\n"
    printf $"Part 1      : %i{part_1 inputData}\n"
    printf $"Part 2 Test : %A{part_2 testInput}\n"
    printf $"Part 2      : %A{part_2 inputData}\n"
    0