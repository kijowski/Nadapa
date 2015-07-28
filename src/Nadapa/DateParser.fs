namespace Nadapa
open System
open FParsec
open DateUtils

module Domain =
  type SpecificDate =
    | Today
    | Tomorrow
    | Yesterday
    | Specific of DateTime

  type RelativeOffset =
    | Next
    | Previous

  type DateParts =
    | Day
    | Week
    | Fortnight
    | Month
    | Year

  type RelativeShift =
    | DayShift of DayOfWeek
    | PeriodShift of DateParts

  type AbsoluteShift =
    | Before of Shift
    | After of Shift
    | Ago
  and Shift =
    | Date of SpecificDate
    | Weekday of DayOfWeek
    | Relative of RelativeOffset * RelativeShift
    | Absolute of int * DateParts * AbsoluteShift

module Configuration =
  open Domain
  open FSharp.Configuration

  type Config = YamlConfig<"config.yaml">

  let config = Config()


  type KeywordsLabels =
    {
      After : string seq
      Before : string seq
      Ago : string seq
    }

  let weekdays =
    [
      ["Monday"; "mon"], DayOfWeek.Monday
      ["Tuesday"; "tue"] ,  DayOfWeek.Tuesday
      ["Wednesday"; "wed"] ,  DayOfWeek.Wednesday
      ["Thursday"; "thu"] , DayOfWeek.Thursday
      ["Friday" ; "fri"] ,  DayOfWeek.Friday
      ["Saturday" ; "sat" ; "weekend"] ,  DayOfWeek.Saturday
      ["Sunday" ; "sun"] , DayOfWeek.Sunday
    ]

  let dateParts =
    [
      ["Day"; "days"] , Day
      ["Week"; "weeks"] ,  Week
      ["Fortnight" ; "fortnights"] , Fortnight
      ["Month" ; "months"] ,  Month
      ["Year" ; "years"] , Year
    ]

  let relativeOffsets =
    [
      ["next" ; "following"] , Next
      ["previous" ; "last"] , Previous
    ]

  let relativeDates =
    [
      //["today"; "tdy" ; "now"] , Today
      config.today |> Seq.toList, Today
      ["tomorow";"tomorrow";"tommorrow";"tommorow";"tmr"] , Tomorrow
      ["yesterday"; "yest" ; "ye"] , Yesterday
    ]

  let defaultKeywords =
    {
      After = ["after" ; "from"]
      Before = ["before"]
      Ago = ["ago"]
    }

module Evaluation =
  open Domain
  let special =
    function
      | Today -> id
      | Tomorrow -> addDay 1
      | Yesterday -> addDay -1
      | Specific(dt) -> specificDate dt

  let previous  =
    function
      | PeriodShift Day -> addDay -1
      | PeriodShift Week -> lastWeek
      | PeriodShift Fortnight -> lastWeek >> lastWeek
      | PeriodShift Month -> lastMonth
      | PeriodShift Year -> lastYear
      | DayShift x -> previousOccurenceOf x

  let next =
    function
      | PeriodShift Day -> addDay 1
      | PeriodShift Week -> nextWeek
      | PeriodShift Fortnight -> nextWeek >> nextWeek
      | PeriodShift Month -> nextMonth
      | PeriodShift Year -> nextYear
      | DayShift x -> nextOccurenceOf x

  let move size =
    function
      | Day -> addDay size
      | Week -> addWeek size
      | Fortnight -> addWeek (2*size)
      | Month -> addMonth size
      | Year -> addYear size

  let rec evaluate =
    function
        | Date(d) -> special d
        | Weekday(day) -> nextOccurenceOf day
        | Relative(offset, shift) -> match offset with
                                            | Previous -> previous shift
                                            | Next -> next shift
        | Absolute(size, datePart, shift) -> match shift with
                                                  | Before sh -> (move -size datePart) >> evaluate sh
                                                  | After sh -> (move size datePart) >> evaluate sh
                                                  | AbsoluteShift.Ago -> (move -size datePart)

module Parsers =
  open Domain
  open Configuration

  let relativeDateP : Parser<SpecificDate,unit> =
    relativeDates |> createP

  let relativeOffsetP : Parser<RelativeOffset,unit> =
    relativeOffsets |> createP

  let weekdaysP : Parser<DayOfWeek,unit> =
    weekdays |> createP

  let datePartsP : Parser<DateParts, unit> =
    dateParts |> createP

  let relativeShiftP : Parser<RelativeShift, unit>=
    (weekdays |> List.map(fun (labs, day) -> labs, DayShift day))
    @
    (dateParts |> List.map(fun (labs,part) -> labs, PeriodShift part))
    |> createP

  let relativeP =
    relativeOffsetP .>>. relativeShiftP

  let absoluteShiftP par =
    [
      anyLabel defaultKeywords.After >>. par |>> After
      anyLabel defaultKeywords.Before >>. par |>> Before
      anyLabel defaultKeywords.Ago >>% Ago
    ]
    |> choice

  let absoluteP par =
    pint32 .>> spaces .>>.? datePartsP .>>. absoluteShiftP par |>> fun((x,y),z) -> (x,y,z)

  let dateFormatP =
    regex @"\d{4}[\-/]?\d{2}[\-/]?\d{2}"
    >>=
    fun x ->
      match DateTime.TryParseExact(x,[|"yyyy-MM-dd" ; "yyyyMMdd" ; "yyyy/MM/dd"|],Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.None) with
        | (true,date) -> preturn(Specific(date))
        | (false,_) -> fail "Date not recognized"

  let completeP =
    let pars, refPar = createParserForwardedToRef()
    refPar :=
      choice [
        relativeDateP |>> Date
        weekdaysP |>> Weekday
        relativeP |>> Relative
        absoluteP pars |>> Absolute
        dateFormatP |>> Date
      ]
    pars
    |>> Evaluation.evaluate


type ParseResult =
  | SuccessfulParse of DateTime
  | FailedParse of string

type DateParser() =
  member this.Parse(arg:string, ?baseDate : DateTime) =
    let bDate = defaultArg baseDate DateTime.Now
    match run (spaces >>. Parsers.completeP .>> eof) arg with
      | Success (result, _, _) -> SuccessfulParse(result bDate)
      | Failure(x,y,z) -> FailedParse(x)

  member this.ParseAtEnd(arg:string, ?baseDate : DateTime) =
    let bDate = defaultArg baseDate DateTime.Now
    match run ( manyCharsTillApply anyChar (attempt(Parsers.completeP)) (fun x y -> y) .>> eof) arg with
      | Success (result, _, _) -> SuccessfulParse(result bDate)
      | Failure(x,y,z) -> FailedParse(x)
