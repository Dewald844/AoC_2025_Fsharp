open System.Text.RegularExpressions
let input = System.IO.File.ReadAllText("./input.txt")
let testInput = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

let parseInput (s: string) = 
    s.Split(",")
    |> Array.map (fun s -> s.Split("-")[0], s.Split("-")[1])
    |> List.ofArray

let partOneRegex = @"^(\d+)\1$"
let partTwoRegex = @"^(\d+)\1+$"
let isInvalidId (s: string, regexpattern: string) = Regex.IsMatch(s, regexpattern)

let buildInvalidIdList regex (list: List<string * string>) : List<int64> = 
    let mutable invalidIdList = []

    list
    |> List.iter (fun (fst, snd) -> 
        if fst.StartsWith('0') || snd.StartsWith('0') then ()
        else 
            [int64 fst .. int64 snd]
            |> List.iter (fun id -> 
                if isInvalidId((id |> string), regex ) then 
                    invalidIdList <- invalidIdList @ [id]
                else ()
            )
    )

    invalidIdList

let part_1 input = input |> parseInput |> buildInvalidIdList partOneRegex |> List.sum
let part_2 input = input |> parseInput |> buildInvalidIdList partTwoRegex |> List.sum

[<EntryPoint>]
let main _ = 
    printf $"Part 1 Test : %i{part_1 testInput}\n"
    printf $"Part 1      : %i{part_1 input}\n"
    printf $"Part 2 Test : %i{part_2 testInput}\n"
    printf $"part 2      : %i{part_2 input}\n"
    0