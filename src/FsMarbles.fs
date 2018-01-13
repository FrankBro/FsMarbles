module FsMarbles.App

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

module List =
    let cons x xs = x :: xs

[<AutoOpen>]
module ExtraProps =
    let BoxSizing (value: string) = !!("boxSizing", value :> obj)
    let BoxShadow (value: string) = !!("boxShadow", value :> obj)
    let Transform (value: string) = !!("transform", value :> obj)
    let Width (value: string) = !!("width", value :> obj)
    let Height (value: string) = !!("height", value :> obj)
    let FontWeight (value: string) = !!("fontWeight", value :> obj)
    let FontFamily (value: string) = !!("fontFamily", value :> obj)
    let FontSize (value: string) = !!("fontSize", value :> obj)

type Position = {
    X: int
    Y: int
}

type Color = {
    R: int
    G: int
    B: int
}
with
    static member Green = { R = 130; G = 215; B = 54 }
    static member Yellow = { R = 255; G = 203; B = 70 }
    static member Red = { R = 255; G = 105; B = 70 }
    static member Blue = { R = 62; G = 161; B = 203 }

type Shape = 
    | Circle
    | Square

type Point<'typ> = {
    Position: Position
    Color: Color
    Value: 'typ
    Shape: Shape
}

type FnSignature =
    | FnSig of FnSignature list
    | Collection of Shape
    | Single of Shape
    | Specific of string
    | Arrow

type Box =
    | IntCollection of Point<int> list
    | IntSingle of Point<int>
    | IntOption of Point<int> option
    | StringCollection of Point<string> list
    | StringSingle of Point<string>
    | StringOption of Point<string> option
    | FloatCollection of Point<float> list
    | FloatSingle of Point<float>
    | FloatOption of Point<float> option
    | BoolCollection of Point<bool> list
    | BoolSingle of Point<bool>
    | BoolOption of Point<bool> option
    | Fn of string
    | FnSignature of FnSignature list
    | Crash

type Page =
    | Append
    | Average
    | AverageBy
    | Choose
    // | ChunkBySize
    | Collect
    // | CompareWith
    | Concat
    | Contains
    | CountBy
    | Distinct
    | Empty
    | Exists
    | Exists2
    | Filter
    | FindIndex
    // | Fold
    // | Fold2
    // | FoldBack
    // | FoldBack2
    | ForAll
    | ForAll2
    | Head
    | Init
    // | IsEmpty
    // | Iter
    // | Iter2
    // | Iteri
    // | Iteri2
    // | Length
    | Map
    | Map2
    | Map3
    | Mapi
    | Mapi2
    | Max
    | MaxBy
    | Min
    | MinBy
    // | Nth
    // | OfArray
    // | OfSeq
    // | Partition
    // | Permute
    // | Pick
    // | Reduce
    // | ReduceBack
    // | Replicate
    // | Rev
    // | Scan
    // | ScanBack
    // | Sort
    // | SortBy
    // | SortWith
    | Sum
    | SumBy
    // | Tail
    // | ToArray
    // | ToSeq
    // | TryFind
    // | TryFindIndex
    // | TryPick
    // | Unzip
    // | Unzip3
    // | Zip
    // | Zip3

with
    static member Pages =
        [
            "Creation Operators", [ Empty; Init ]
            "Combination Operators", [ Append; Concat ]
            "Filtering Operators", [ Filter; Distinct ]
            "Mathematical Operators", [ Average; AverageBy; Max; MaxBy; Min; MinBy; Sum; SumBy ]
            "Transformation Operators", [ Collect; Map; Map2; Map3; Mapi; Mapi2 ]
            "Unknown", [ Choose; Contains; CountBy; Exists; Exists2; FindIndex; ForAll; ForAll2; Head ]
        ]

    override x.ToString () =
        match x with
        | Append -> "append"
        | Average -> "average"
        | AverageBy -> "averageBy"
        | Choose -> "choose"
        // | ChunkBySize -> "chunkBySize"
        | Collect -> "collect"
        // | CompareWith -> "compareWith"
        | Concat -> "concat"
        | Contains -> "contains"
        | CountBy -> "countBy"
        | Distinct -> "distinct"
        | Empty -> "empty"
        | Exists -> "exists"
        | Exists2 -> "exists2"
        | Filter -> "filter"
        | FindIndex -> "findIndex"
        | ForAll -> "forall"
        | ForAll2 -> "forall2"
        | Head -> "head"
        | Init -> "init"
        | Map -> "map"
        | Map2 -> "map2"
        | Map3 -> "map3"
        | Mapi -> "mapi"
        | Mapi2 -> "mapi2"
        | Max -> "max"
        | MaxBy -> "maxBy"
        | Min -> "min"
        | MinBy -> "minBy"
        | Sum -> "sum"
        | SumBy -> "sumBy"

    member x.Content =
        let shape shape color x value = { Position = { X = x; Y = 5}; Color = color; Value = value; Shape = shape }
        let circle = shape Circle
        let circleY = circle Color.Yellow
        let circleB = circle Color.Blue
        let circleG = circle Color.Green
        let circleR = circle Color.Red
        let square = shape Square
        let squareY = square Color.Yellow
        let squareB = square Color.Blue
        let squareG = square Color.Green
        let squareR = square Color.Red
        let mathInputs =
            [
                circleY 05 02
                circleB 15 30
                circleY 25 22
                circleG 35 05
                circleB 45 60
                circleG 55 01
            ]
        let mathInputsF = mathInputs |> List.map (fun input -> { Position = input.Position; Color = input.Color; Value = float input.Value; Shape = input.Shape })
        match x with
        | Append ->
            let fnName = "List.append"
            let inputs1 =
                [
                    circleB 00 1
                    circleG 15 1
                    circleY 50 1
                ]
            let inputs2 =
                [
                    circleR 00 2
                    circleR 08 2
                ]
            let last1 =
                inputs1
                |> List.tryLast
                |> Option.map (fun point -> point.Position.X)
                |> Option.defaultValue 0
            let outputs =
                inputs2
                |> List.mapi (fun i input2 ->
                    { input2 with Position = { input2.Position with X = last1 + (08 * (i + 1)) } }
                )
                |> List.append inputs1
            [
                IntCollection inputs1
                IntCollection inputs2
                Fn fnName
                FnSignature [Collection Circle; Arrow; Collection Circle; Arrow; Collection Circle]
                IntCollection outputs
            ]
        | Average ->
            let fnName = "List.average"
            let inputs = mathInputsF
            let outputValue = inputs |> List.map (fun point -> point.Value) |> List.average
            let output =
                inputs
                |> List.tryLast
                |> Option.map (fun point -> { point with Value = outputValue })
                |> Option.defaultWith (fun () -> { Position = { X = 0; Y = 5 }; Color = Color.Red; Shape = Circle; Value = outputValue })
            [
                FloatCollection inputs
                Fn fnName
                FloatSingle output
            ]
        | AverageBy ->
            let fnName, fn = 
                "(fun x -> float x)" |> (+) "List.averageBy "
                ,(fun x -> float x)
            let inputs = mathInputs
            let outputValue = inputs |> List.map (fun point -> point.Value) |> List.averageBy fn
            let output =
                inputs
                |> List.tryLast
                |> Option.map (fun point -> { Position = point.Position; Color = point.Color; Shape = Square; Value = outputValue })
                |> Option.defaultWith (fun () -> { Position = { X = 0; Y = 5 }; Color = Color.Red; Shape = Square; Value = outputValue })
            [
                IntCollection mathInputs
                Fn fnName
                FloatSingle output
            ]
        | Choose ->
            let fnName, fn =
                "(fun x -> if x > 10 then (Some float) x else None)" |> (+) "List.choose "
                ,(fun x -> if x > 10 then Some (float x) else None)
            let inputs =
                [
                    circleY 05 02
                    circleB 15 30
                    circleY 25 22
                    circleG 35 05
                    circleB 45 60
                    circleG 55 01
                ]
            let outputs =
                inputs
                |> List.choose (fun input ->
                    fn input.Value
                    |> Option.map (fun value ->
                        { Position = input.Position; Color = input.Color; Value = value; Shape = Square }
                    )
                )
            [
                IntCollection inputs
                Fn fnName
                FloatCollection outputs
            ]
        | Collect ->
            let fnName, fn =
                "(fun x -> [float x; float (x + 1)]" |> (+) "List.collect "
                ,(fun x -> [float x; float (x + 1)])
            let inputs =
                [
                    circleY 05 10
                    circleR 25 20
                    circleB 45 30
                ]
            let outputs =
                inputs
                |> List.collect (fun input ->
                    let values = fn input.Value
                    values
                    |> List.mapi (fun i value ->
                        { Position = { input.Position with X = input.Position.X + (i * 8) }; Color = input.Color; Shape = Square; Value = value }
                    )
                )
            [
                IntCollection inputs
                Fn fnName
                FloatCollection outputs
            ]
        // | ChunkBySize ->
        //     let fnName, fn = 
        //         "List.chunkBySize 3"
        //         ,List.chunkBySize 3
        //     let inputs =
        //         [
        //             circleY 05 10
        //             circleG 15 20
        //             circleR 25 30
        //             circleB 35 40
        //             circleY 45 50
        //             circleG 55 60
        //             circleR 65 70
        //             circleB 75 80
        //         ]
        //     let outputss = fn inputs
        //     [
        //         yield IntCollection inputs
        //         yield Fn fnName
        //         for outputs in outputss do
        //             yield IntCollection outputs
        //     ]
        // | CompareWith ->
        //     let fnName, fn = 
        //         "(fun x y -> if x > y then 1 elif x < y then -1 else 0)" |> (+) "List.compareWith"
        //         ,(fun x y -> if x > y then 1 elif x < y then -1 else 0)
        //     let inputs1 =
        //         [
        //             circleB 00 01
        //             circleG 15 01
        //             circleB 50 01
        //         ]
        //     let inputs2 =
        //         [
        //             circleR 00 02
        //             circleR 08 02
        //         ]
        //     let lastSharedIndex =
        //         [ List.length inputs1; List.length inputs2 ]
        //         |> List.min
        //     let lastX =
        //         [ List.item lastSharedIndex inputs1; List.item lastSharedIndex inputs2 ]
        //         |> List.map (fun point -> point.Position.X)
        //         |> List.max
        //     let outputValue =
        //         (inputs1, inputs2)
        //         ||> List.compareWith (fun input1 input2 ->
        //             fn input1.Value input2.Value
        //         )
        //     let output = squareY lastX outputValue
        //     [
        //         IntCollection inputs1
        //         IntCollection inputs2
        //         Fn fnName
        //         IntSingle output
        //     ]
        | Concat ->
            let fnName = "List.concat"
            let inputs1 =
                [
                    circleB 00 01
                    circleG 15 01
                    circleY 50 01
                ]
            let inputs2 =
                [
                    circleR 00 02
                    circleR 08 02
                ]
            let last1 =
                inputs1
                |> List.tryLast
                |> Option.map (fun point -> point.Position.X)
                |> Option.defaultValue 0
            let transformedInputs2 =
                inputs2
                |> List.mapi (fun i input2 ->
                    { input2 with Position = { input2.Position with X = last1 + (08 * (i + 1)) } }
                )
            let outputs =
                [ inputs1; transformedInputs2 ]
                |> List.concat 
            [
                IntCollection inputs1
                IntCollection inputs2
                Fn fnName
                IntCollection outputs
            ]
        | Contains ->
            let fnName = "List.contains 10"    
            let inputs =
                [
                    circleB 00 01
                    circleY 15 10
                    circleG 50 02
                ]
            let lastX =
                inputs
                |> List.tryLast
                |> Option.map (fun input -> input.Position.X)
                |> Option.defaultValue 0
            let output =
                inputs
                |> List.tryPick (fun input ->
                    let contains = input.Value = 10
                    if contains then
                        Some { Position = input.Position; Color = Color.Red; Shape = Square; Value = true }
                    else
                        None
                )
                |> Option.defaultWith (fun () -> squareR lastX false)
            [
                IntCollection inputs
                Fn fnName
                BoolSingle output
            ]
        | CountBy ->
            let fnName, fn =
                "(fun x -> x)" |> (+) "List.countBy "
                ,(fun x -> x)
            let inputs =
                [
                    circleB 00 01
                    circleY 15 02
                    circleB 25 01
                    circleR 35 03
                    circleY 45 02
                    circleB 60 01
                ]
            let lastX =
                inputs
                |> List.tryLast
                |> Option.map (fun input -> input.Position.X)
                |> Option.defaultValue 0
            let outputValues =
                inputs
                |> List.countBy (fun input -> fn input.Value)
            let outputs =
                outputValues
                |> List.mapi (fun i (a, b) ->
                    { Position = { X = lastX + (8 * i); Y = 5 }; Color = Color.Green; Shape = Square; Value = sprintf "%d,%d" a b }
                )
            [
                IntCollection inputs
                Fn fnName
                StringCollection outputs
            ]
        | Distinct ->
            let fnName = "List.distinct"
            let inputs =
                [
                    circleR 05 01
                    circleR 20 02
                    circleB 35 02
                    circleB 60 01
                    circleB 70 03
                ]
            let outputs =
                ([], inputs)
                ||> List.fold (fun (state: Point<int> list) input ->
                    let keep =
                        state
                        |> List.exists (fun kept -> kept.Value = input.Value)
                        |> not
                    if keep then
                        { Position = input.Position; Color = input.Color; Value = input.Value; Shape = Circle } :: state
                    else
                        state
                )
                |> List.rev
            [
                IntCollection inputs
                Fn fnName
                IntCollection outputs
            ]
        | Empty ->
            let fnName = "List.empty"
            let outputs = []
            [
                Fn fnName
                IntCollection outputs
            ]
        | Exists ->
            let fnName, fn =
                "(fun x -> x = 10)" |> (+) "List.exists "
                ,(fun x -> x = 10)
            let inputs =
                [
                    circleB 00 01
                    circleY 15 10
                    circleG 50 02
                ]
            let lastX =
                inputs
                |> List.tryLast
                |> Option.map (fun input -> input.Position.X)
                |> Option.defaultValue 0
            let output =
                inputs
                |> List.tryPick (fun input ->
                    let contains = input.Value = 10
                    if contains then
                        Some { Position = input.Position; Color = Color.Red; Shape = Square; Value = true }
                    else
                        None
                )
                |> Option.defaultWith (fun () -> squareR lastX false)
            [
                IntCollection inputs
                Fn fnName
                BoolSingle output
            ]
        | Exists2 ->
            let fnName, fn =
                "(fun x y -> x = y)" |> (+) "List.exists2 "
                ,(fun x y -> x = y)
            let inputs1 =
                [
                    circleB 00 01
                    circleY 15 10
                    circleG 50 02
                ]
            let inputs2 =
                [
                    circleB 00 02
                    circleY 15 10
                    circleG 50 01
                ]
            let lastX =
                [ inputs1 |> List.tryLast; inputs2 |> List.tryLast ]
                |> List.choose id
                |> List.map (fun input -> input.Position.X)
                |> (function
                    | [] -> 0
                    | xs -> List.min xs
                )
            let output =
                (inputs1, inputs2)
                ||> List.zip
                |> List.tryPick (fun (input1, input2) ->
                    let contains = fn input1.Value input2.Value
                    if contains then
                        Some { Position = input1.Position; Color = Color.Red; Shape = Square; Value = true }
                    else
                        None
                )
                |> Option.defaultWith (fun () -> squareR lastX false)
            [
                IntCollection inputs1
                IntCollection inputs2
                Fn fnName
                BoolSingle output
            ]
        | Filter ->
            let fnName, fn =
                "(fun x -> x > 10)" |> (+) "List.filter "
                ,(fun x -> x > 10)
            let inputs =
                [
                    circleY 05 02
                    circleB 15 30
                    circleY 25 22
                    circleG 35 05
                    circleB 45 60
                    circleG 55 01
                ]
            let outputs =
                inputs
                |> List.choose (fun input ->
                    let keep = fn input.Value
                    if keep then
                        Some { Position = input.Position; Color = input.Color; Value = input.Value; Shape = Circle }
                    else
                        None
                )
            [
                IntCollection inputs
                Fn fnName
                IntCollection outputs
            ]
        | FindIndex ->
            let fnName, fn =
                "(fun x -> x = 10)" |> (+) "List.findIndex "
                ,(fun x -> x = 10)
            let inputs =
                [
                    circleB 00 01
                    circleY 15 10
                    circleG 50 02
                ]
            let lastX =
                inputs
                |> List.tryLast
                |> Option.map (fun input -> input.Position.X)
                |> Option.defaultValue 0
            let outputBox =
                inputs
                |> List.mapi (fun i x -> (i, x))
                |> List.tryPick (fun (i, input) ->
                    let contains = input.Value = 10
                    if contains then
                        { Position = input.Position; Color = Color.Red; Shape = Square; Value = i }
                        |> IntSingle
                        |> Some
                    else
                        None
                )
                |> Option.defaultValue Crash
            [
                IntCollection inputs
                Fn fnName
                outputBox
            ]
        | ForAll ->
            let fnName, fn =
                "(fun x -> x > 10)" |> (+) "List.forall "
                ,(fun x -> x > 10)
            let inputs =
                [
                    circleB 00 01
                    circleY 15 10
                    circleG 50 02
                ]
            let lastX =
                inputs
                |> List.tryLast
                |> Option.map (fun input -> input.Position.X)
                |> Option.defaultValue 0
            let output =
                inputs
                |> List.tryPick (fun input -> 
                    let isTrue = fn input.Value
                    if not isTrue then
                        Some { Position = input.Position; Color = Color.Red; Shape = Square; Value = false }
                    else
                        None
                )
                |> Option.defaultWith (fun () -> squareR lastX true)
            [
                IntCollection inputs
                Fn fnName
                BoolSingle output
            ]
        | ForAll2 ->
            let fnName, fn =
                "(fun x y -> x <> y)" |> (+) "List.forall "
                ,(fun x y -> x <> y)
            let inputs1 =
                [
                    circleB 00 01
                    circleY 15 10
                    circleG 50 02
                ]
            let inputs2 =
                [
                    circleB 00 02
                    circleY 15 10
                    circleG 50 01
                ]
            let lastX =
                [ inputs1 |> List.tryLast; inputs2 |> List.tryLast ]
                |> List.choose id
                |> List.map (fun input -> input.Position.X)
                |> (function
                    | [] -> 0
                    | xs -> List.min xs
                )
            let output =
                (inputs1, inputs2)
                ||> List.zip
                |> List.tryPick (fun (input1, input2) ->
                    let isTrue = fn input1.Value input2.Value
                    if not isTrue then
                        Some { Position = input1.Position; Color = Color.Red; Shape = Square; Value = false }
                    else
                        None
                )
                |> Option.defaultWith (fun () -> squareR lastX true)
            [
                IntCollection inputs1
                IntCollection inputs2
                Fn fnName
                BoolSingle output
            ]
        | Head ->
            let fnName = "List.head"
            let inputs =
                [
                    circleB 00 01
                    circleY 15 10
                    circleG 50 02
                ]
            let outputBox =
                match inputs with
                | [] -> Crash
                | input :: _ -> IntSingle input
            [
                IntCollection inputs
                Fn fnName
                outputBox                
            ]
        | Init ->
            let fnName, fn =
                "(fun x -> x * 10)" |> (+) "List.init "
                ,(fun x -> x * 10)
            let outputs =
                List.init 3 fn
                |> List.mapi (fun i value ->
                    circleR (10 + (8 * i)) value
                )
            [
                Fn fnName
                IntCollection outputs
            ]
        | Map ->
            let fnName, fn =
                "(fun x -> x * 10)" |> (+) "List.map "
                ,(fun x -> x * 10)
            let inputs =
                [
                    circleY 10 1
                    circleR 20 2
                    circleB 50 3
                ]
            let outputs =
                inputs
                |> List.map (fun input ->
                    let value = fn input.Value
                    { Position = input.Position; Color = input.Color; Value = value; Shape = Square }
                )
            [
                IntCollection inputs
                Fn fnName
                FnSignature [FnSig [ Single Circle; Arrow; Single Square ]; Arrow; Collection Circle; Arrow; Collection Square]
                IntCollection outputs
            ]
        | Map2 ->
            let fnName, fn =
                "(fun x y -> float (x + y))" |> (+) "List.map2 "
                ,(fun x y -> float (x + y))
            let inputs1 =
                [
                    circleY 10 1
                    circleR 20 2
                    circleB 50 3
                ]
            let inputs2 =
                [
                    circleY 10 10
                    circleR 20 20
                    circleB 50 30
                ]
            let outputs =
                (inputs1, inputs2)
                ||> List.map2 (fun input1 input2 ->
                    let value = fn input1.Value input2.Value
                    { Position = input1.Position; Color = input1.Color; Value = value; Shape = Square }
                )
            [
                IntCollection inputs1
                IntCollection inputs2
                Fn fnName
                FloatCollection outputs
            ]
        | Map3 ->
            let fnName, fn =
                "(fun x y z -> float (x + y + z))" |> (+) "List.map3 "
                ,(fun x y z -> float (x + y + z))
            let inputs1 =
                [
                    circleY 10 1
                    circleR 20 2
                    circleB 50 3
                ]
            let inputs2 =
                [
                    circleY 10 05
                    circleR 20 15
                    circleB 50 30
                ]
            let inputs3 =
                [
                    circleY 10 10
                    circleR 20 20
                    circleB 50 30
                ]
            let outputs =
                (inputs1, inputs2, inputs3)
                |||> List.map3 (fun input1 input2 input3 ->
                    let value = fn input1.Value input2.Value input3.Value
                    { Position = input1.Position; Color = input1.Color; Value = value; Shape = Square }
                )
            [
                IntCollection inputs1
                IntCollection inputs2
                IntCollection inputs3
                Fn fnName
                FloatCollection outputs
            ]
        | Mapi ->
            let fnName, fn =
                "(fun i x -> float (x * i))" |> (+) "List.mapi "
                ,(fun i x -> float (x * i))
            let inputs =
                [
                    circleY 10 1
                    circleR 20 2
                    circleB 50 3
                ]
            let outputs =
                inputs
                |> List.mapi (fun i input ->
                    let value = fn i input.Value
                    { Position = input.Position; Color = input.Color; Value = value; Shape = Square }
                )
            [
                IntCollection inputs
                Fn fnName
                FloatCollection outputs
            ]
        | Mapi2 ->
            let fnName, fn =
                "(fun i x y -> float ((x + y) * i))" |> (+) "List.mapi2 "
                ,(fun i x y -> float ((x + y) * i))
            let inputs1 =
                [
                    circleY 10 1
                    circleR 20 2
                    circleB 50 3
                ]
            let inputs2 =
                [
                    circleY 10 10
                    circleR 20 20
                    circleB 50 30
                ]
            let outputs =
                (inputs1, inputs2)
                ||> List.mapi2 (fun i input1 input2 ->
                    let value = fn i input1.Value input2.Value
                    { Position = input1.Position; Color = input1.Color; Value = value; Shape = Square }
                )
            [
                IntCollection inputs1
                IntCollection inputs2
                Fn fnName
                FloatCollection outputs
            ]
        | Max ->
            let fnName = "List.max"
            let inputs = mathInputs
            let outputValue = inputs |> List.map (fun point -> point.Value) |> List.max
            let output =
                inputs
                |> List.tryLast
                |> Option.map (fun point -> { point with Value = outputValue })
                |> Option.defaultWith (fun () -> { Position = { X = 0; Y = 5 }; Color = Color.Red; Shape = Circle; Value = outputValue })
            [
                IntCollection inputs
                Fn fnName
                IntSingle output
            ]
        | MaxBy ->
            let fnName, fn = 
                "(fun x -> float x)" |> (+) "List.maxBy "
                ,(fun x -> float x)
            let inputs = mathInputs
            let outputValue = inputs |> List.map (fun point -> point.Value) |> List.maxBy fn
            let output =
                inputs
                |> List.tryLast
                |> Option.map (fun point -> { Position = point.Position; Color = point.Color; Shape = Circle; Value = outputValue })
                |> Option.defaultWith (fun () -> { Position = { X = 0; Y = 5 }; Color = Color.Red; Shape = Circle; Value = outputValue })
            [
                IntCollection mathInputs
                Fn fnName
                IntSingle output
            ]
        | Min ->
            let fnName = "List.min"
            let inputs = mathInputs
            let outputValue = inputs |> List.map (fun point -> point.Value) |> List.min
            let output =
                inputs
                |> List.tryLast
                |> Option.map (fun point -> { point with Value = outputValue })
                |> Option.defaultWith (fun () -> { Position = { X = 0; Y = 5 }; Color = Color.Red; Shape = Circle; Value = outputValue })
            [
                IntCollection inputs
                Fn fnName
                IntSingle output
            ]
        | MinBy ->
            let fnName, fn = 
                "(fun x -> float x)" |> (+) "List.minBy "
                ,(fun x -> float x)
            let inputs = mathInputs
            let outputValue = inputs |> List.map (fun point -> point.Value) |> List.minBy fn
            let output =
                inputs
                |> List.tryLast
                |> Option.map (fun point -> { Position = point.Position; Color = point.Color; Shape = Circle; Value = outputValue })
                |> Option.defaultWith (fun () -> { Position = { X = 0; Y = 5 }; Color = Color.Red; Shape = Circle; Value = outputValue })
            [
                IntCollection mathInputs
                Fn fnName
                IntSingle output
            ]
        | Sum ->
            let fnName = "List.sum"
            let inputs = mathInputs
            let outputValue = inputs |> List.map (fun point -> point.Value) |> List.sum
            let output =
                inputs
                |> List.tryLast
                |> Option.map (fun point -> { point with Value = outputValue })
                |> Option.defaultWith (fun () -> { Position = { X = 0; Y = 5 }; Color = Color.Red; Shape = Circle; Value = outputValue })
            [
                IntCollection inputs
                Fn fnName
                IntSingle output
            ]
        | SumBy ->
            let fnName, fn = 
                "(fun x -> float x)" |> (+) "List.sumBy "
                ,(fun x -> float x)
            let inputs = mathInputs
            let outputValue = inputs |> List.map (fun point -> point.Value) |> List.sumBy fn
            let output =
                inputs
                |> List.tryLast
                |> Option.map (fun point -> { Position = point.Position; Color = point.Color; Shape = Square; Value = outputValue })
                |> Option.defaultWith (fun () -> { Position = { X = 0; Y = 5 }; Color = Color.Red; Shape = Square; Value = outputValue })
            [
                IntCollection mathInputs
                Fn fnName
                FloatSingle output
            ]


type Model = {
    Page: Page
}

open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser

let toHash page =
    match page with
    | Page.Append -> "#append"
    | Page.Average -> "#average"
    | Page.AverageBy -> "#averageBy"
    | Page.Choose -> "#choose"
    // | Page.ChunkBySize -> "#chunkBySize"
    | Page.Collect -> "#collect"
    // | Page.CompareWith -> "#compareWith"
    | Page.Concat -> "#concat"
    | Page.Contains -> "#contains"
    | Page.CountBy -> "#countBy"
    | Page.Distinct -> "#distinct"
    | Page.Empty -> "#empty"
    | Page.Exists -> "#exists"
    | Page.Exists2 -> "#exists2"
    | Page.Filter -> "#filter"
    | Page.FindIndex -> "#findIndex"
    | Page.ForAll -> "#forall"
    | Page.ForAll2 -> "#forall2"
    | Page.Head -> "#head"
    | Page.Init -> "#init"
    | Page.Map -> "#map"
    | Page.Map2 -> "#map2"
    | Page.Map3 -> "#map3"
    | Page.Mapi -> "#mapi"
    | Page.Mapi2 -> "#mapi2"
    | Page.Max -> "#max"
    | Page.MaxBy -> "#maxBy"
    | Page.Min -> "#min"
    | Page.MinBy -> "#minBy"
    | Page.Sum -> "#sum"
    | Page.SumBy -> "#sumBy"

let pageParser : Parser<Page->Page,Page> =
    oneOf [
        map Page.Append (s "append")
        map Page.Average (s "average")
        map Page.AverageBy (s "averageBy")
        map Page.Choose (s "choose")
        // map Page.ChunkBySize (s "chunkBySize")
        map Page.Collect (s "collect")
        // map Page.CompareWith (s "compareWith")
        map Page.Concat (s "concat")
        map Page.Contains (s "contains")
        map Page.CountBy (s "countBy")
        map Page.Distinct (s "distinct")
        map Page.Empty (s "empty")
        map Page.Exists (s "exists")
        map Page.Exists2 (s "exists2")
        map Page.Filter (s "filter")
        map Page.FindIndex (s "findIndex")
        map Page.ForAll (s "forall")
        map Page.ForAll2 (s "forall2")
        map Page.Head (s "head")
        map Page.Init (s "init")
        map Page.Map (s "map")
        map Page.Map2 (s "map2")
        map Page.Map3 (s "map3")
        map Page.Mapi (s "mapi")
        map Page.Mapi2 (s "mapi2")
        map Page.Max (s "max")
        map Page.MaxBy (s "maxBy")
        map Page.Min (s "min")
        map Page.MinBy (s "minBy")
        map Page.Sum (s "sum")
        map Page.SumBy (s "sumBy")
    ]

let urlUpdate (result: Page option) model =
    match result with
    | None ->
        Browser.console.error "Error parsing url"
        model, Navigation.modifyUrl (toHash model.Page)
    | Some page ->
        { model with Page = page }, Cmd.Empty

type Action =
    | PageChange of Page

let init oPage =
    let page =
        oPage
        |> Option.defaultValue Page.Map
    let model = { Page = page }
    model, Cmd.Empty

let update (action: Action) (model: Model) =
    match action with
    | PageChange page -> 
        { model with Page = page }, Cmd.Empty

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable

let sidebar =
    div [Style [Flex "1 1 0%"]] [
        Page.Pages
        |> List.mapi (fun i (section, pages) ->
            let sectionElement =
                li [Style [TextTransform "uppercase"; FontSize "0.7em"; MarginTop (if i = 0 then "0px" else "22px"); Color "rgb(124, 124, 124)"]] [str section]
            pages
            |> List.map (fun page ->
                li [Style [FontSize "1rem"; LineHeight "1.6rem"; Color "rgb(124, 124, 124)"]] [
                    a [ClassName "link"; Href (toHash page); Style [Position "relative"; Display "block"; Color "rgb(124, 124, 124)"]] [str (string page)]
                ]
            )
            |> (List.cons sectionElement)
            |> (ul [Style [Margin "0px"; Padding "0px"; ListStyleType "none"]])
        )
        |> (div [Style [MarginRight "32px"; BoxSizing "border-box"; OverflowY "scroll"; Height "calc(100vh - 150px)"]])
    ]

let lineSvg =
    svg [ViewBox "0 0 7 10"; Style [Width "48px"; Height "68px"; Overflow "visible"]] [
        line [X1 "0"; X2 "112"; Y1 "5"; Y2 "5"; Stroke "black"; Style [StrokeWidth 0.3]] []
        polygon [Points "111.7,6.1 111.7,3.9 114,5"] []
    ]

let circleSvg position color value =
    let transform = sprintf "translate(%d, %d)" position.X position.Y
    let fill = sprintf "rgb(%d, %d, %d)" color.R color.G color.B
    g [ClassName "marble"; ExtraProps.Transform transform; Style [Cursor "default"]] [
        circle [R "2.3"; Stroke "black"; Style [Fill fill; StrokeWidth 0.3]] []
        text [TextAnchor "middle"; Y "0.8"; Style [FontSize "2.5px"; FontFamily "\"Source Sans Pro\", sans-serif"]] [str (string value)]
    ]

let squareSvg position color value =
    let transform = sprintf "translate(%d, %d)" position.X position.Y
    let fill = sprintf "rgb(%d, %d, %d)" color.R color.G color.B
    g [ClassName "marble"; ExtraProps.Transform transform; Style [Cursor "default"]] [
        rect [X "-2.3"; Y "-2.3"; ExtraProps.Width "4.6"; ExtraProps.Height "4.6"; Stroke "black"; Style [Fill fill; StrokeWidth 0.3]] []
        text [TextAnchor "middle"; Y "0.8"; Style [FontSize "2.5px"; FontFamily "\"Source Sans Pro\", sans-serif"]] [str (string value)]
    ]

let arrowSvg x =
    let transform = sprintf "translate(%d, %d)" x 0
    g [ClassName "marble"; ExtraProps.Transform transform; Style [Cursor "default"]] [
        line [X1 0; X2 4; Y1 "5"; Y2 "5"; Stroke "black"; Style [StrokeWidth 0.3]] []
        polygon [Points "3.7,6.1 3.7,3.9 6,5"] []
    ]

let shapeSvg point =
    match point.Shape with
    | Circle -> circleSvg point.Position point.Color point.Value
    | Square -> squareSvg point.Position point.Color point.Value

let shapesSvg points =
    points
    |> List.map shapeSvg
    |> (svg [ViewBox "0 0 100 10"; Style [Width "680px"; Height "68px"; Overflow "visible"]])


let listBox points =
    div [Style [Padding "10px 22px"]] [
        lineSvg
        shapesSvg points
    ]

let fnBox fnName =
    div [ClassName "operatorBox"; Style [Border "1px solid rgba(0, 0, 0, 0.06)"; Padding "22px"; TextAlign "center"; Position "relative"]] [
        div [Style [Display "block"; Position "absolute"; Left "0px"; Top "0px"; Right "0px"; Bottom "0px"; BoxShadow "rgba(0, 0, 0, 0.17) 0px 2px 10px 0px"]] []
        span [ClassName "operatorLabel"; Style [FontWeight "400"; FontSize "2rem"; FontFamily "\"Source Code Pro\", monospace"]] [str fnName]
        div [Style [Display "block"; Position "absolute"; Left "0px"; Top "0px"; Right "0px"; Bottom "0px"; BoxShadow "rgba(0, 0, 0, 0.26) 0px 2px 5px 0px"]] []
    ]

let fnSignatureBox signatures =
    let size =
        let rec loop x = function
            | [] -> x
            | signature :: signatures ->
                match signature with
                | FnSignature.FnSig xs ->
                    let endX = loop (x + 6) xs
                    loop (endX + 4) signatures
                | FnSignature.Collection s -> 
                    let endX = (x + 15)
                    loop endX signatures
                | FnSignature.Single s -> 
                    loop (x + 4) signatures
                | FnSignature.Specific tName -> 
                    loop x signatures
                | FnSignature.Arrow -> 
                    loop (x + 10) signatures
        loop 0 signatures
    let startX = (105 - size) / 2
    let middleContent =
        let rec loop (x, state) = function
            | [] -> (x, state)
            | signature :: signatures ->
                match signature with
                | FnSignature.FnSig xs ->
                    let paren1 = text [X x; Y 6.5; ExtraProps.FontWeight "200"; ExtraProps.FontSize "0.25rem"; ExtraProps.FontFamily "\"Source Code Pro\", monospace"] [str "("]
                    let endX, innerState = loop ((x + 6), []) xs
                    let paren2 = text [X endX; Y 6.5; ExtraProps.FontWeight "200"; ExtraProps.FontSize "0.25rem"; ExtraProps.FontFamily "\"Source Code Pro\", monospace"] [str ")"]
                    let state = (endX + 4), [ yield paren1; yield! innerState; yield paren2 ]
                    loop state signatures
                | FnSignature.Collection s ->
                    let shape =
                        match s with
                        | Circle -> circleSvg { X = x; Y = 5 } Color.Red ""
                        | Square -> squareSvg { X = x; Y = 5 } Color.Red ""
                    let list = text [X (x + 4); Y 6.5; ExtraProps.FontWeight "200"; ExtraProps.FontSize "0.25rem"; ExtraProps.FontFamily "\"Source Code Pro\", monospace"] [str "list"]
                    let state = (x + 15), shape :: list :: state
                    loop state signatures
                | FnSignature.Single s -> 
                    let shape =
                        match s with
                        | Circle -> circleSvg { X = x; Y = 5 } Color.Red ""
                        | Square -> squareSvg { X = x; Y = 5 } Color.Red ""
                    let state = (x + 4), shape :: state
                    loop state signatures
                | FnSignature.Specific tName -> x, state
                | FnSignature.Arrow ->
                    let arrow = arrowSvg x
                    let state = (x + 10), arrow :: state
                    loop state signatures
        loop (startX, []) signatures
    let middle =
        middleContent
        |> snd
        |> (svg [ViewBox "0 0 100 10"; Style [Width "680px"; Height "68px"; Overflow "visible"]])
    [

        div [Style [Display "block"; Position "absolute"; Left "0px"; Top "0px"; Right "0px"; Bottom "0px"; BoxShadow "rgba(0, 0, 0, 0.17) 0px 2px 10px 0px"]] []
        middle
        div [Style [Display "block"; Position "absolute"; Left "0px"; Top "0px"; Right "0px"; Bottom "0px"; BoxShadow "rgba(0, 0, 0, 0.26) 0px 2px 5px 0px"]] []
    ]
    |> (div [ClassName "operatorBox"; Style [Border "1px solid rgba(0, 0, 0, 0.06)"; Padding "22px"; TextAlign "center"; Position "relative"]])

let body (page: Page) =
    let content = 
        page.Content
        |> List.map (fun line ->
            let intFn = (fun point -> { Position = point.Position; Color = point.Color; Shape = point.Shape; Value = string point.Value })
            let floatFn = (fun point -> { Position = point.Position; Color = point.Color; Shape = point.Shape; Value = sprintf "%2.0f." point.Value })
            let stringFn = (fun point -> { Position = point.Position; Color = point.Color; Shape = point.Shape; Value = sprintf "%s" point.Value })
            let boolFn = (fun point -> { Position = point.Position; Color = point.Color; Shape = point.Shape; Value = sprintf "%s" (if point.Value then "T" else "F") })
            match line with
            | IntCollection points -> points |> List.map intFn |> listBox 
            | IntSingle point -> point |> intFn |> List.singleton |> listBox
            | IntOption oPoint -> failwith "Not implemented"
            | StringCollection points -> points |> List.map stringFn |> listBox
            | StringSingle point -> point |> stringFn |> List.singleton |> listBox
            | StringOption oPoint -> failwith "Not implemented"
            | FloatCollection points -> points |> List.map floatFn |> listBox 
            | FloatSingle point -> point |> floatFn |> List.singleton |> listBox
            | FloatOption oPoint -> failwith "Not implemented"
            | BoolCollection points -> points |> List.map boolFn |> listBox 
            | BoolSingle point -> point |> boolFn |> List.singleton |> listBox
            | BoolOption oPoint -> failwith "Not implemented"
            | Fn fnName -> fnBox fnName
            | FnSignature signature -> fnSignatureBox signature
            | Crash -> fnBox "EXCEPTION"
        )
    div [Style [Flex "0 0 820px"]] [
        content
        |> (div [Style [BackgroundColor "rgb(255, 255, 255)"; BoxShadow "rgba(0, 0, 0, 0.17) 0px 1px 2px 1px"; BorderRadius "2px"]])
    ]

let view (model: Model) (dispatch: Dispatch<Action>) : Fable.Import.React.ReactElement =
    div [ClassName "app-container"] [
        div [Style [Width "1060px"; Margin "0px auto"]] [
            a [ClassName "github-fork-ribbon"; Href "https://github.com/FrankBro/FsMarbles"; Title "Fork me on GitHub"] [str "Fork me on GitHub"]
            div [Style [Display "flex"; AlignItems "baseline"]] [
                h1 [Style [FontFamily "Signika, Helvetica, serif"; Color "rgb(124, 124, 124)"; Flex "1 1 0%"]] [
                    str "Fs Marbles"
                ]
                h3 [Style [Color "rgb(124, 124, 124)"; Flex "0 0 820px"]] [
                    str "Interactive diagrams of Fs Collections"
                ]
            ]
            div [Style [Display "flex"]] [
                sidebar
                body model.Page
            ]
            section [Style [Position "fixed"; Bottom "2px"; Right "22px"; Color "rgb(124, 124, 124)"]] [
                str "Built on "
                a [Href "http://fable.io"] [str "Fable"]
                str ", inpired by "
                a [Href "http://rxmarbles.com/"] [str "RxMarbles"]
            ]
        ]
    ]

open Elmish.React

// App
Program.mkProgram init update view
|> Program.toNavigable (parseHash pageParser) urlUpdate
|> Program.withReact "elmish-app"
|> Program.run
