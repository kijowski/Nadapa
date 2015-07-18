// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#I @"/Users/michal/Code/Nadapa/packages/Newtonsoft.Json.6.0.5/lib/net45/"
#r @"/Users/michal/Code/Nadapa/packages/Newtonsoft.Json.6.0.5/lib/net45/Newtonsoft.Json.dll"

#I @"/Users/michal/Code/Nadapa/packages/FSharp.Data.2.2.5/lib/net40/"
#r @"FSharp.Data.dll"

open FSharp.Data
open FSharp.Data.JsonExtensions

let x = JsonValue.Parse("""{"absolute":[{"millenium":"20000101"}]}""")
x?absolute.AsArray() |> Seq.iter (printfn "%A")


#I @"/Users/michal/Code/Nadapa/packages/FParsec.1.0.1/lib/net40-client"
#r @"FParsecCS.dll"
#r @"FParsec.dll"

#load "DateParser.fs"
open Nadapa
let sut = DateParser()
sut.Parse("today")
sut.Parse("3 days before now")