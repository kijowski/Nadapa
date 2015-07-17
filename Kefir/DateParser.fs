namespace Kefir
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
    let createParser dateTransform names =
        names 
        |> Seq.map (fun name -> stringCIReturn name dateTransform  .>> spaces )
        |> choice
    let createConsumingParser names =
        names 
        |> Seq.map (fun name -> skipStringCI name  .>> spaces )
        |> choice

module InternalParsers = 
    let todayP = 
        ParserCreators.createParser (id) ["today"; "tdy" ; "now"] 
    let tomorrowP = 
        ParserCreators.createParser (fun (date:DateTime) -> date.AddDays(1.)) ["tomorow";"tomorrow";"tommorrow";"tommorow";"tmr"] 
    let yesterdayP = 
        ParserCreators.createParser (fun (date:DateTime) -> date.AddDays(-1.)) ["yesterday"; "yest" ; "ye"]     

    let shiftP (shiftType:AbsoluteShift) = 
        pint32 
        .>> spaces 
        .>> (ParserCreators.createConsumingParser shiftType.Labels) 
        |>> fun shiftSize -> float(shiftSize * shiftType.ShiftInDays)

    let fromP (shiftType:AbsoluteShift) parser= 
        shiftP shiftType
        .>> spaces 
        .>>? ParserCreators.createConsumingParser ["from"; "after"]
        .>> spaces 
        .>>. parser
        |>> fun (shift, dateTransf) -> ((fun (date:DateTime) -> date.AddDays(shift)) >> dateTransf)
    
    let beforeP (shiftType:AbsoluteShift) parser = 
        shiftP shiftType
        .>> spaces 
        .>>? ParserCreators.createConsumingParser ["before"]
        .>> spaces 
        .>>. parser
        |>> fun (shift, dateTransf) -> ((fun (date:DateTime) -> date.AddDays(-shift)) >> dateTransf)

    let agoP (shiftType:AbsoluteShift) = 
        shiftP shiftType
        .>> spaces 
        .>>? ParserCreators.createConsumingParser ["ago"]
        |>> fun (shift) -> ((fun (date:DateTime) -> date.AddDays(-shift)))


type DateParser(?baseDate : DateTime) = 
    let bDate = defaultArg baseDate DateTime.Now  


    let combinedParser = 
        let pars, refPar = createParserForwardedToRef()      

        refPar := choice [
            InternalParsers.todayP 
            InternalParsers.tomorrowP
            InternalParsers.yesterdayP
            InternalParsers.agoP {ShiftInDays = 1 ; Labels = ["days"; "day"]}
            InternalParsers.fromP {ShiftInDays = 1 ; Labels = ["days"; "day"]} pars
            InternalParsers.beforeP {ShiftInDays = 1 ; Labels = ["days"; "day"]} pars
        ]
        pars
    
         
    member this.Parse(arg:string) =
        match run (combinedParser) arg with
            | Success (result, _, __) -> SuccessfulParse(result bDate)
            | Failure(x,y,z) -> FailedParse(x)

            

