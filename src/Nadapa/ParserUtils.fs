namespace Nadapa
open FParsec

module FParsec =

  let anyLabel (labels:string seq) =
    labels |> Seq.sortBy (fun x -> - x.Length) |> Seq.map skipStringCI |> choice .>> spaces

  let createParser<'a> caseSensitive (elements : (string list * 'a) seq) : Parser<'a,unit> =
    let parser = if caseSensitive then stringReturn else stringCIReturn
    elements
    |> Seq.collect(fun (labels, retValue) -> labels |> Seq.map(fun label -> label,retValue))
    |> Seq.sortBy(fun (label,_) -> - label.Length)
    |> Seq.map(fun (lab,retVal) -> parser lab retVal .>> spaces)
    |> choice
    .>> spaces
