//// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
//// for more guidance on F# programming.
//
#I @"../../packages/FParsec/lib/net40-client"
#r @"FParsecCS.dll"
#r @"FParsec.dll"
//
#load "DateParser.fs"
open Nadapa
let sut = DateParser()
//sut.Parse("today")
//sut.Parse("3 days before now")