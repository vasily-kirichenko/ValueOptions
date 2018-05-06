
[<Struct>]
type VOption<'a> =
    | VSome of 'a
    | VNone
    member this.IsSome =
        match this with 
        | VSome _ -> true
        | VNone -> false
    member this.Value = 
        let (VSome x) = this in x

module Array =
    let inline chooseAs<'a, ^r when ^r : (member IsSome: bool) and ^r : (member Value: 'a)>(f: 'a -> ^r)(xs: 'a []) : 'a [] =
        let acc = ResizeArray xs.Length
        for x in xs do
            let r = f x
            if (^r : (member IsSome: bool) r) then 
                acc.Add (^r : (member Value: 'a) r)
        acc.ToArray()
         
//[|1..10|] |> Array.chooseAs (fun x -> if x % 2 = 0 then Some (x * 100) else None)         
//[1..10] |> List.chooseAs (fun x -> if x % 2 = 0 then VSome (x * 100) else VNone)  

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Attributes.Jobs
open BenchmarkDotNet.Running

[<MemoryDiagnoser>]
type Bench() =
    let xs = [|1..100_000|]

//    [<Benchmark(Baseline = true)>]
//    member __.OldChoose() =
//        xs |> List.choose (fun x -> if x % 2 = 0 then Some (x * 100) else None) |> ignore
        
    [<Benchmark(Baseline = true)>]
    member __.RefOption() =
        xs |> Array.chooseAs (fun x -> if x % 2 = 0 then Some (x * 100) else None) |> ignore
    
    [<Benchmark>]
    member __.ValueOption() =
        xs |> Array.chooseAs (fun x -> if x % 2 = 0 then VSome (x * 100) else VNone) |> ignore

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<Bench>() |> ignore
    0
