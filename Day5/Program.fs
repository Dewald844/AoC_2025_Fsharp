let inputData = System.IO.File.ReadAllText("./input.txt")

let testInput = System.IO.File.ReadAllText("./testInput.txt")

let parseInput (input: string) = 
    input.Split([|"\r\n\r\n"; "\n\n"|], System.StringSplitOptions.RemoveEmptyEntries) 
    |> fun arr -> arr[0],arr[1]

let buildFreshIdSet (freshRange: string) = 
    freshRange.Split("\n")
    |> Array.map (fun r -> 
        r.Split "-"
        |> fun arr -> int64 arr[0] ,int64 arr[1]
    )
    |> List.ofArray

let buildProductIds (products : string)  =
    products.Split("\n")
    |> List.ofArray
    |> List.map int64

let part_1 (input: string) : int = 
    let freshRange, ingredients = parseInput input
    let freshRangeSet = buildFreshIdSet freshRange
    let ingredientSet = buildProductIds ingredients

    ingredientSet
    |> List.map (fun i -> 
        let freshCount = 
            freshRangeSet 
            |> List.filter (fun (start, ending) -> (i >= start && i <= ending))
            |> List.length
        if freshCount > 0 then 1 else 0
    )
    |> List.sum

let countDistinctIds (ranges: (int64 * int64) list) : int64 =
    if List.isEmpty ranges then 0 else

    let sorted = List.sortBy fst ranges

    let (count, currentStart, currentEnd) =
        sorted
        |> List.fold (fun (accCount, accStart, accEnd) (startI, endI) ->
            if startI <= accEnd + 1L then
                (accCount, accStart, max accEnd endI)
            else
                let accCount = accCount + (accEnd - accStart + 1L)
                (accCount, startI, endI)
        ) (0, fst sorted.Head, snd sorted.Head)

    count + (currentEnd - currentStart + (int64 1))

let part_2 (input: string) : int64 = 
    let freshRange, _ = parseInput input
    let freshRangeL = buildFreshIdSet freshRange

    freshRangeL
    |> countDistinctIds

[<EntryPoint>]
let main _ = 

    printf $"Part 1 Test : %i{part_1 testInput}\n"
    printf $"Part 1      : %i{part_1 inputData}\n"
    printf $"Part 2 Test : %i{part_2 testInput}\n"
    printf $"Part 2      : %i{part_2 inputData}\n"

    0