let testInput = """.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
"""


let inputData = System.IO.File.ReadAllText("./input.txt")
let parse (s : string) = 
    s.Split([|"\n"; "\r"|],System.StringSplitOptions.RemoveEmptyEntries )
    |> Array.map (fun s -> s |> seq |> Seq.toArray)

let startingColumn (grid : array<array<char>>) = 
    grid[0]
    |> Array.findIndex(fun c -> c = 'S')

let calculateSplitCount (grid: array<array<char>>) = 
    
    let start = startingColumn grid

    let rec calc (beamMap : Map<int * int, int>) (row : int) (splitCount : int) = 
        if row < grid.Length - 1 then
            let mutable nextBeamMap = beamMap
            let mutable rowSplitCount = splitCount
            
            grid[row]
            |> Array.iteri (fun col v -> 
                match beamMap |> Map.tryFind (col, row - 1) with 
                | Some _ ->
                    if v = '^' then
                        nextBeamMap <- nextBeamMap |> Map.add (col - 1, row) 1
                        nextBeamMap <- nextBeamMap |> Map.add (col + 1, row) 1
                        rowSplitCount <- rowSplitCount + 1
                    else 
                        nextBeamMap <- nextBeamMap |> Map.add (col, row) 1
                | None -> ()
            )
            calc nextBeamMap (row + 1) rowSplitCount
        else 
            splitCount

    calc ([(start, 1), 1] |> Map.ofList) 1 0

let part_1 (input : string) = 
    input
    |> parse
    |> calculateSplitCount

let countLeaves startCol (grid: char[][]) =
    let rows = grid.Length
    let cols = if rows > 0 then grid.[0].Length else 0

    let ways = Array2D.create cols rows 0L

    ways.[startCol, 0] <- 1L 

    for row in 0 .. rows - 1 do
      for col in 0 .. cols - 1 do
        let count = ways.[col,row]
        if count > 0L then
          if row = rows - 1 then ()
          else
            match grid.[row].[col] with
            | '^' ->
                if col - 1 >= 0 then 
                    ways.[col - 1, row + 1] <- ways.[col - 1, row + 1] + count
                if col + 1 < cols then 
                    ways.[col + 1, row + 1] <- ways.[col + 1, row + 1] + count
            | 'S' | '|' | '.' | ' ' ->
                ways.[col, row + 1] <- ways.[col, row + 1] + count
            | _ -> ()

    let mutable leafCount = 0L

    for col in 0 .. cols - 1 do
        match grid.[rows - 1].[col] with
        | '^' ->
            leafCount <- leafCount + ways.[col, rows - 1] * 2L
        | 'S' | '|' | '.' | ' ' ->
            leafCount <- leafCount + ways.[col, rows - 1]
        | _ ->
            leafCount <- leafCount + ways.[col, rows - 1]

    leafCount

let part_2 (input : string) = 
    let grid = input |> parse
    countLeaves (startingColumn grid) grid

[<EntryPoint>]
let main _ = 
    printf $"Part 1 Test : %i{part_1 testInput}\n"
    printf $"Part 1      : %i{part_1 inputData}\n"
    printf $"Part 2 Test : %i{part_2 testInput}\n"
    printf $"Part 2      : %i{part_2 inputData}\n"
    0