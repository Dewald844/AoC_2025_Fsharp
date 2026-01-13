let inputData = 
    System.IO.File.ReadAllLines("./input.txt")
    |> List.ofArray

let testInput = [
    "..@@.@@@@."
    "@@@.@.@.@@"
    "@@@@@.@.@@"
    "@.@@@@..@."
    "@@.@@@@.@@"
    ".@@@@@@@.@"
    ".@.@.@.@@@"
    "@.@@@.@@@@"
    ".@@@@@@@@."
    "@.@.@@@.@."
]

let parseInput (input : List<string>) : List<List<string>> = 
    input
    |> List.map (fun s -> s |> Seq.map string |> Seq.toList)

let getValueO (x: int, y : int) (grid : List<List<string>>) : Option<string> = 
    try
        let v = grid.[y].[x]
        Some v
    with | _ -> None

let paperRoleCount (currentX: int, currentY : int) (grid : List<List<string>>) : int = 
    [
        getValueO (currentX - 1, currentY - 1) grid  // top-left
        getValueO (currentX,     currentY - 1) grid  // top
        getValueO (currentX + 1, currentY - 1) grid  // top-right
        getValueO (currentX - 1, currentY)     grid  // left
        getValueO (currentX + 1, currentY)     grid  // right
        getValueO (currentX - 1, currentY + 1) grid  // bottom-left
        getValueO (currentX,     currentY + 1) grid  // bottom
        getValueO (currentX + 1, currentY + 1) grid  // bottom-right
    ]
    |> List.choose id
    |> List.filter (fun s -> s = "@")
    |> List.length

let part_1 (input : List<string>) = 
    let grid = input |> parseInput

    grid
    |> List.mapi (fun y row ->
        row
        |> List.mapi (fun x _ ->
            if paperRoleCount (x, y) grid < 4 && grid.[y].[x] = "@" then 1 else 0
        )
    )
    |> List.concat
    |> List.sum

let replace grid (posX, posY)  = 
    grid
    |> List.mapi (fun y list -> 
        list
        |> List.mapi (fun x v ->  
            if posY = y && posX = x then "." else v
        )
    )

let rec findAndReplace (rollCount : int) (grid : List<List<string>>) = 
    let mutable positionsToReplace = []

    let iterationCount = 
        grid
        |> List.mapi (fun y row ->
            row
            |> List.mapi (fun x _ ->
                if paperRoleCount (x, y) grid < 4 && grid.[y].[x] = "@" then 
                    positionsToReplace <- positionsToReplace @ [x, y]
                    1 
                else 0
            )
        )
        |> List.concat
        |> List.sum

    if iterationCount > 0 then 
        let nextGrid = List.fold replace grid positionsToReplace
        findAndReplace (rollCount + iterationCount) nextGrid
    else 
        rollCount

let part_2 (input: List<string>) = 
    let grid = input |> parseInput
    findAndReplace 0 grid

[<EntryPoint>]
let main _ = 
    printf $"Part 1 Test : %i{part_1 testInput}\n"
    printf $"Part 1      : %i{part_1 inputData}\n"
    printf $"Part 2 Test : %i{part_2 testInput}\n"
    printf $"Part 2      : %i{part_2 inputData}\n"
    0