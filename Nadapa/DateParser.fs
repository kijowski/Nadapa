namespace Nadapa
open System
open FParsec


type DateTransform = DateTime -> DateTime

type ParseResult =
    | SuccessfulParse of DateTime
    | FailedParse of string

type ShiftType =
    | DayBased of (int -> DateTransform)


type AbsoluteShift =
    { 
        Apply:int -> DateTransform
        Labels : string seq
    }
   
[<AutoOpen>]
module Helpers = 
    let addYear i (date:DateTime) = 
        date.AddYears(i)
    let addMonth i (date:DateTime) = 
        date.AddMonths(i)
    let addWeek i (date:DateTime) = 
        date.AddDays(float i * 7.)
    let addDay i (date:DateTime) =
        date.AddDays(float i)
    let nextOccurenceOf weekday (date:DateTime) = 
        let rec noo (d:DateTime) = 
            if d.DayOfWeek = weekday 
            then d
            else noo (addDay 1 d)
        noo date
               

module ParserCreators = 
    let createParser dateTransform (names:string seq) =
        names 
        |> Seq.sortBy(fun x -> - x.Length)
        |> Seq.map (fun name -> stringCIReturn name dateTransform  .>> spaces )
        |> choice

    let createConsumingParser (names:string seq) =
        names 
        |> Seq.sortBy(fun x -> - x.Length)
        |> Seq.map (fun name -> skipStringCI name  .>> spaces )
        |> choice

module InternalParsers = 
    open ParserCreators

    let todayP = 
        createParser (id) ["today"; "tdy" ; "now"] 
    let tomorrowP = 
        createParser (addDay 1) ["tomorow";"tomorrow";"tommorrow";"tommorow";"tmr"] 
    let yesterdayP = 
        createParser (addDay -1) ["yesterday"; "yest" ; "ye"]     

    let shiftP (shiftTypes:AbsoluteShift list) = 
        pint32 .>> spaces 
        .>>. (shiftTypes |> List.map(fun shiftType -> (createParser shiftType.Apply shiftType.Labels)) |> choice) 


    let fromP (shiftType:AbsoluteShift list) parser= 
        attempt (
            pipe2 
                (shiftP shiftType .>> createConsumingParser ["from"; "after"])
                parser
                (fun (size, shift) dateTransf -> shift ((size)) >> dateTransf)
        )
    let beforeP (shiftType:AbsoluteShift list) parser = 
        attempt (
            pipe2
                (shiftP shiftType .>> createConsumingParser ["before"])
                parser
                (fun (size, shift) dateTransf -> shift ((-size)) >> dateTransf)
        )
    let agoP (shiftType:AbsoluteShift list) = 
        attempt (
            shiftP shiftType
            .>>? ParserCreators.createConsumingParser ["ago"]
            |>> fun (size, shift) -> shift ((-size))
        )
    let fallbackP =
        attempt (
            many1CharsTill
                (noneOf " \n\t") 
                ((skipAnyOf " \n\t") <|> eof)
                >>=
                    (fun x -> 
                    DateTime.TryParse(x) // DateTime.TryParseExact(x,["yyyymmdd"],Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.None)
                    |> function
                        | (true, date) -> preturn(fun _ -> date)
                        | (false,_) -> fail "date not recognized")
        )    

type DateParser(?baseDate : DateTime) = 
    let bDate = defaultArg baseDate DateTime.Now  

    let basicShifts = 
        [
            {Labels = ["days"; "day"] ; Apply = addDay}
            {Labels = ["weeks"; "week"] ; Apply = addWeek}
            {Labels = ["fortnight" ; "fortnights"] ; Apply = (fun x -> addWeek (2*x))}
            {Labels = ["months" ; "month"] ; Apply = addMonth}
            {Labels = ["years" ; "year"] ; Apply = addYear}
        ]

    let combinedParser = 
        let pars, refPar = createParserForwardedToRef()     
        refPar := choice [
            InternalParsers.todayP 
            InternalParsers.tomorrowP
            InternalParsers.yesterdayP
            InternalParsers.agoP basicShifts
            InternalParsers.fromP basicShifts pars
            InternalParsers.beforeP basicShifts pars
            InternalParsers.fallbackP
        ]
        pars
    
         
    member this.Parse(arg:string) =
        match run (combinedParser) arg with
            | Success (result, _, __) -> SuccessfulParse(result bDate)
            | Failure(x,y,z) -> FailedParse(x)

    member this.ParseAtEnd(arg:string) =
        match run ( manyCharsTillApply anyChar combinedParser (fun x y -> y) .>> eof) arg with
            | Success (result, _, __) -> SuccessfulParse(result bDate)
            | Failure(x,y,z) -> FailedParse(x)
            

