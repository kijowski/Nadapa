namespace Nadapa
open System
open FParsec


type DateTransform = DateTime -> DateTime

type ParseResult =
    | SuccessfulParse of DateTime
    | FailedParse of string

type AbsoluteShift =
    { 
        ShiftInDays:int
        Labels : string seq
    }

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
    let todayP = 
        ParserCreators.createParser (id) ["today"; "tdy" ; "now"] 
    let tomorrowP = 
        ParserCreators.createParser (fun (date:DateTime) -> date.AddDays(1.)) ["tomorow";"tomorrow";"tommorrow";"tommorow";"tmr"] 
    let yesterdayP = 
        ParserCreators.createParser (fun (date:DateTime) -> date.AddDays(-1.)) ["yesterday"; "yest" ; "ye"]     

    let shiftP (shiftTypes:AbsoluteShift list) = 
        pint32 
        .>> spaces 
        .>>. (shiftTypes |> List.map(fun shiftType -> (ParserCreators.createParser shiftType.ShiftInDays shiftType.Labels)) |> choice) 
        |>> fun (shift, size) -> float(shift * size)

    let fromP (shiftType:AbsoluteShift list) parser= 
        attempt (
            shiftP shiftType
            .>> spaces 
            .>> ParserCreators.createConsumingParser ["from"; "after"]
            .>> spaces 
            .>>. parser
            |>> fun (shift, dateTransf) -> ((fun (date:DateTime) -> date.AddDays(shift)) >> dateTransf)
        )
    let beforeP (shiftType:AbsoluteShift list) parser = 
        attempt (
            shiftP shiftType
            .>> spaces 
            .>> ParserCreators.createConsumingParser ["before"]
            .>> spaces 
            .>>. parser
            |>> fun (shift, dateTransf) -> ((fun (date:DateTime) -> date.AddDays(-shift)) >> dateTransf)
        )
    let agoP (shiftType:AbsoluteShift list) = 
        attempt (
            shiftP shiftType
            .>> spaces 
            .>>? ParserCreators.createConsumingParser ["ago"]
            |>> fun (shift) -> ((fun (date:DateTime) -> date.AddDays(-shift)))
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
            {ShiftInDays = 1 ; Labels = ["days"; "day"]}
            {ShiftInDays = 7 ; Labels = ["weeks"; "week"]}
            {ShiftInDays = 14 ; Labels = ["fortnight" ; "fortnights"]}
            {ShiftInDays = 72 ; Labels = ["kardashian"]}
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
            

