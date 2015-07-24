namespace Nadapa
open FParsec

module FParsec =
  let createParser dateTransform (names:string seq) =
    names
    |> Seq.sortBy(fun x -> - x.Length)
    |> Seq.map (fun name -> stringCIReturn name dateTransform  .>> spaces )
    |> choice

  let anyLabel (labels:string seq) =
    labels |> Seq.sortBy (fun x -> - x.Length) |> Seq.map skipStringCI |> choice .>> spaces
