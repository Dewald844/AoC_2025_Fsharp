type Rotation = | Left of int | Right of int
    with 
        member this.GetRotationCount = 
            match this with 
            | Left i -> i  | Right i -> i

        static member parseX (s: string) = 
            match s[0] with 
            | 'L' -> Left (int s[1..])
            | 'R' -> Right (int s[1..])
            | c -> failwith $"Invalid direction recieved : {c}"

type State = {CurrentPos : int; ZeroCount : int}

let inputData =
    System.IO.File.ReadAllText("./input.txt").Split('\n')
    |> Array.toList

let testInput = [ "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82"]

let calculateNextPos (rotation : Rotation) (currentPos : int)= 
    match rotation with 
    | Left  r -> (currentPos - (r % 100) + 100) % 100
    | Right r -> (currentPos + (r % 100)) % 100

let calculateZeroRestCount (s : State) (rotation : Rotation) : State = 
    let newPos = calculateNextPos rotation s.CurrentPos
    if newPos = 0 then 
        {s with CurrentPos = newPos; ZeroCount = s.ZeroCount + 1}
    else 
        {s with CurrentPos = newPos}
    
let rotationToSignedValue (rotation: Rotation) =
    match rotation with
    | Left r -> -r
    | Right r -> r

let calculateZeroPassCount (s : State) (rotation : Rotation) : State = 
    let modulus = 100
    let clicks = rotationToSignedValue rotation
    let full_spins = abs(clicks / modulus) 
    let rem_clicks = clicks % modulus
    let currentPos = s.CurrentPos
    
    let nsum = currentPos + rem_clicks

    let zeroPassedPartial = 
        if (nsum >= modulus) || (nsum <= 0 && currentPos > 0) then true
        else false

    let zerosAdded =
        if zeroPassedPartial then full_spins + 1 else full_spins

    let newPos = ((nsum % modulus) + modulus) % modulus

    { s with CurrentPos = newPos; ZeroCount = s.ZeroCount + zerosAdded }

let part_1 (input : List<string>) = 
    input
    |> List.map Rotation.parseX
    |> List.fold calculateZeroRestCount {CurrentPos = 50; ZeroCount = 0}
    |> _.ZeroCount

let part_2 (input : List<string>) = 
    input
    |> List.map Rotation.parseX
    |> List.fold calculateZeroPassCount {CurrentPos = 50; ZeroCount = 0}
    |> _.ZeroCount

[<EntryPoint>]
let main _args = 

    // Part one 1029
    printf $"Part 1 Test : {part_1 testInput}\n"
    printf $"Part 1      : {part_1 inputData}\n"
    printf $"Part 2 Test : {part_2 testInput}\n"
    printf $"Part 2      : {part_2 inputData}\n"
    0


