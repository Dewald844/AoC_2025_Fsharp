let inputData = 
    System.IO.File.ReadAllLines("./input.txt")
    |> List.ofArray

let testInput = [
    "987654321111111"
    "811111111111119"
    "234234234234278"
    "818181911112111"
]

let parseInput (list : List<string>)= 
    list
    |> List.map (fun s -> 
        s
        |> Seq.map (fun c -> int c - int '0') 
        |> Seq.toList
    )

let findHighest(list : List<int>) = 
    let orderedList = list |> List.sortDescending
    let higestIndex = list |> List.findIndex(fun i -> i = orderedList[0])
    if higestIndex = list.Length - 1 then 
        let secondHigestIndex = list |> List.findIndex (fun i -> i = orderedList[1])
        secondHigestIndex

    else 
        higestIndex

let findSecondHigestAfterFirst (firstHigestIndex : int) (list : List<int>) = 
    let listAfterHighest = list[(firstHigestIndex + 1)..]
    let orderedList = listAfterHighest |> List.sortDescending
    let secondHigestIndex = list |> List.findIndex(fun i -> i = orderedList[0])
    secondHigestIndex

let part_1 (input: List<string>)  = 
    input
    |> parseInput
    |> List.map (fun bank -> 
        let fst = findHighest bank
        let snd = findSecondHigestAfterFirst fst bank
        let maxJoltage = int $"%i{bank[fst]}%i{bank[snd]}"
        maxJoltage
    )
    |> List.sum

let selectMaxDigits (digits: int list) (k: int) : int list =
    let n = List.length digits
    let toRemove = n - k

    let rec build digits (stack: int list) removalsLeft =
        match digits with
        | [] -> stack
        | d::ds ->
            let rec popWhileSmaller stk remRem =
                match stk with
                | top :: rest when remRem > 0 && top < d -> popWhileSmaller rest (remRem - 1)
                | _ -> (stk, remRem)
            let stk, remRem = popWhileSmaller stack removalsLeft
            build ds (d::stk) remRem

    let revStack = build digits [] toRemove
    let stack = List.rev revStack
    if List.length stack > k then
        stack |> List.take k
    else
        stack

let part_2 (input: List<string>) = 
    parseInput input 
    |> List.map (fun bank -> 
        let selectedDigits = selectMaxDigits bank 12
        selectedDigits
        |> List.map string
        |> String.concat ""
        |> int64
    )
    |> List.sum


[<EntryPoint>]
let main _ =
    printf $"Part 1 Test : %i{part_1 testInput}\n"
    printf $"Part 1      : %i{part_1 inputData}\n"
    printf $"Part 2 Test : %i{part_2 testInput}\n"
    printf $"Part 2      : %i{part_2 inputData}\n"
    0