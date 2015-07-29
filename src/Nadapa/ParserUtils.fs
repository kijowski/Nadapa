namespace Nadapa
open FParsec

module FParsec =

  let anyLabel (labels:string seq) =
    labels |> Seq.sortBy (fun x -> - x.Length) |> Seq.map skipStringCI |> choice .>> spaces

  let createP<'a> (elements : (string list * 'a) seq) : Parser<'a,unit> =
    elements
    |> Seq.collect(fun (labels, retValue) -> labels |> Seq.map(fun label -> label,retValue))
    |> Seq.sortBy(fun (label,_) -> - label.Length)
    |> Seq.map(fun (lab,retVal) -> stringCIReturn lab retVal .>> spaces)
    |> choice
    .>> spaces
