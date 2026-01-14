
[<EntryPoint>]
let main _args = 

    let inputLines =
        System.IO.File.ReadAllText("./input.txt").Split('\n')
        |> Array.toList

    let isInvalidId (n: int64) =
        let s = string n
        let len = s.Length
        if len % 2 <> 0 then
            false
        else
            let half = len / 2
            let first = s.Substring(0, half)
            let second = s.Substring(half, half)
            first = second

    let parseRange (text: string) : int64 * int64 =
        let parts = text.Split('-')
        if parts.Length <> 2 then
            failwithf "Invalid range: '%s'" text
        let lo = int64 parts[0]
        let hi = int64 parts[1]
        lo, hi

    let sumInvalidInRange (lo: int64, hi: int64) : int64 =
        if hi < lo then 0L
        else
            let count = hi - lo + 1L
            if count > int64 System.Int32.MaxValue then
                failwithf "Range too large to enumerate: %d-%d" lo hi
            else
                Seq.init (int count) (fun i -> lo + int64 i)
                |> Seq.filter isInvalidId
                |> Seq.sum

    let totalInvalidSum =
        inputLines
        |> List.collect (fun line ->
            line.Split(',', System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList)
        |> List.map (fun s -> s.Trim())
        |> List.filter (fun s -> s <> "")
        |> List.map parseRange
        |> List.map sumInvalidInRange
        |> List.sum

    printfn "%d" totalInvalidSum

    0


