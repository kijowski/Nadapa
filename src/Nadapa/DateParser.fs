namespace Nadapa
open System
open FParsec
open DateUtils

module Config =
  type DateParts =
    {
      Day : string seq
      Week : string seq
      Month : string seq
      Year : string seq
    }

  type WeekDays =
    {
      Monday : string seq
      Tuesday : string seq
      Wednesday : string seq
      Thursday : string seq
      Friday : string seq
      Saturday : string seq
      Sunday : string seq
    }

type DateTransform = DateTime -> DateTime

type ParseResult =
  | SuccessfulParse of DateTime
  | FailedParse of string

type StaticShifts =
  {
    Labels : string seq
    StaticApply : DateTransform
    Priority : int
  }

type RelativeShift =
  {
    Apply : DateTransform
    Labels : string seq
  }

type AbsoluteShift =
  {
    Apply:int -> DateTransform
    Labels : string seq
  }

// New
module Types =
  type SpecificDate =
    | Today
    | Tomorrow
    | Yesterday
    | Specific of DateTime
      with member x.Apply =
                    match x with
                      | Today -> id
                      | Tomorrow -> addDay 1
                      | Yesterday -> addDay -1
                      | Specific(dt) -> specificDate dt

  type RelativeOffset =
    | Next
    | Previous

  type Weekdays =
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday

  type DateParts =
    | Day
    | Week
    | Fortnight
    | Month
    | Year

  type Shift =
    | Date of SpecificDate
    | Weekday of DayOfWeek
    | RelativeDay of RelativeOffset * DayOfWeek
    | RelativeShift of RelativeOffset * DateParts
    | AbsoluteForwardShift of int * DateParts * Shift
    | AbsoluteBackwardShift of int * DateParts * Shift
    | Ago of int * DateParts

module NewParsers =
  open Types
  let relativeDateP : Parser<SpecificDate,unit> =
    choice [
      anyLabel ["today"; "tdy" ; "now"] >>% Today
      anyLabel ["tomorow";"tomorrow";"tommorrow";"tommorow";"tmr"] >>% Tomorrow
      anyLabel ["yesterday"; "yest" ; "ye"] >>% Yesterday
    ] .>> spaces

  let relativeOffsetP : Parser<RelativeOffset,unit> =
    choice [
      anyLabel ["next" ; "following"] >>% Next
      anyLabel ["previous" ; "next"] >>% Previous
    ] .>> spaces

  let weekdaysP : Parser<DayOfWeek,unit> =
    choice [
      anyLabel ["Monday"; "mon"] >>% DayOfWeek.Monday
      anyLabel ["Tuesday"; "tue"] >>% DayOfWeek.Tuesday
      anyLabel ["Wednesday"; "wed"] >>% DayOfWeek.Wednesday
      anyLabel ["Thursday"; "thu"] >>%DayOfWeek.Thursday
      anyLabel ["Friday" ; "fri"] >>% DayOfWeek.Friday
      anyLabel ["Saturday" ; "sat" ; "weekend"] >>% DayOfWeek.Saturday
      anyLabel ["Sunday" ; "sun"] >>% DayOfWeek.Sunday
    ] .>> spaces

  let datePartsP : Parser<DateParts, unit> =
    choice [
      anyLabel ["Day"; "days"] >>% Day
      anyLabel ["Week"; "weeks"] >>% Week
      anyLabel ["Fortnight" ; "fortnights"] >>% Fortnight
      anyLabel ["Month" ; "months"] >>% Month
      anyLabel ["Year" ; "years"] >>% Year
    ] .>> spaces

  let fallbackP =
      many1CharsTill
        (noneOf " \n\t")
        ((skipAnyOf " \n\t") <|> eof)
        >>=
        (fun x ->
          DateTime.TryParse(x) // DateTime.TryParseExact(x,["yyyymmdd"],Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.None)
          |> function
              | (true, date) -> preturn(date)
              | (false,_) -> fail "date not recognized")

  let previous datePart date =
    match datePart with
      | Day -> addDay -1 date
      | Week -> lastWeek date
      | Fortnight -> lastWeek (lastWeek date)
      | Month -> lastMonth date
      | Year -> lastYear date
  let next datePart date =
    match datePart with
      | Day -> addDay 1 date
      | Week -> nextOccurenceOf DayOfWeek.Monday date
      | Fortnight -> nextOccurenceOf DayOfWeek.Monday ((nextOccurenceOf DayOfWeek.Monday) date)
      | Month -> nextMonth date
      | Year -> nextYear date
  let move size datePart date =
    match datePart with
      | Day -> addDay size date
      | Week -> addWeek size date
      | Fortnight -> addWeek (2*size) date
      | Month -> addMonth size date
      | Year -> addYear size date

  let rec evaluate =
    function
        | Date(d) -> d.Apply
        | Weekday(day) -> nextOccurenceOf day
        | RelativeDay(offset, weekday) -> match offset with
                                            | Previous -> previousOccurenceOf weekday
                                            | Next -> nextOccurenceOf weekday
        | RelativeShift(offset,datePart) -> match offset with
                                                    | Previous -> (previous datePart) //>> (evaluate shift)
                                                    | Next -> (next datePart) //>> (evaluate shift)
        | AbsoluteForwardShift(size, datePart, shift) -> (move size datePart) >> evaluate shift
        | AbsoluteBackwardShift(size, datePart, shift) -> (move -size datePart) >> evaluate shift
        | Ago(size, datePart) -> (move -size datePart)

  let completeP =
    let pars, refPar = createParserForwardedToRef()
    refPar :=
      choice [
        relativeDateP |>> Date
        weekdaysP |>> Weekday
        attempt (relativeOffsetP .>>. datePartsP) |>> RelativeShift
        attempt (relativeOffsetP .>>. weekdaysP)|>> RelativeDay
        attempt (pint32 .>> spaces .>>. datePartsP .>> anyLabel ["after"; "from"] .>>. pars) |>> fun ((x,y),z) -> AbsoluteForwardShift(x,y,z)
        attempt (pint32 .>> spaces .>>. datePartsP .>> anyLabel ["before"] .>>. pars) |>> fun ((x,y),z) -> AbsoluteBackwardShift(x,y,z)
        attempt (pint32 .>> spaces .>>. datePartsP .>> anyLabel ["ago"]) |>> Ago
        fallbackP |>> fun date -> Date(Specific(date))
      ]
    pars
    |>> evaluate


type ParserConfig =
  {
    Yesterday : string list
    Today: string list
    Tomorrow : string list
    Day : string list
    Week : string list
    Fortnight : string list
    Month : string list
    Year : string list
    BackShiftKeywords : string list
    ForwardShiftKeywords : string list
  }

// next Monday, first Monday = Date -> Date
// next Monday before = Date -> Date - does it make sense? NO
// next Monday after = Date -> Date
// 2 days before = Date -> Date
// previous Monday after = don't make much sense
// last Monday, previous Monday = Date -> Date
// previous Monday before =  Date -> Date
// Monday = next Monday

module InternalParsers =
  let relativeAnchorP (staticShifts : StaticShifts list) =
    staticShifts
    |> List.sortBy(fun shift -> shift.Priority)
    |> List.map(fun shift -> createParser shift.StaticApply shift.Labels)
    |> choice

  let shiftP (shiftTypes:AbsoluteShift list) =
    pint32 .>> spaces
    .>>. (shiftTypes |> List.map(fun shiftType -> (createParser shiftType.Apply shiftType.Labels)) |> choice)

  let forwardShiftP labels (shiftTypes:AbsoluteShift list) parser=
    attempt (
      pipe2
        (shiftP shiftTypes .>> anyLabel labels)
        parser
        (fun (size, shift) dateTransf -> shift ((size)) >> dateTransf)
    )

  let backShiftP labels (shiftTypes:AbsoluteShift list) parser =
    attempt (
      pipe2
        (shiftP shiftTypes .>> anyLabel labels)
        parser
        (fun (size, shift) dateTransf -> shift ((-size)) >> dateTransf)
    )

  let agoP (shiftTypes:AbsoluteShift list) =
    attempt (
      shiftP shiftTypes
      .>> anyLabel ["ago"]
      |>> fun (size, shift) -> shift ((-size))
    )

  let nextP (shifts:RelativeShift list)=
    attempt (
      skipStringCI "next" .>> spaces
      >>. (shifts
        |> List.collect(fun shift -> shift.Labels |> Seq.map(fun lab -> lab, shift.Apply) |> Seq.toList)
        |> List.sortBy(fun (lab,shift) -> - lab.Length)
        |> List.map(fun (lab,shift) -> createParser shift [lab]) |> choice)
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

type DateParser(?config:ParserConfig) =
  let conf =
    defaultArg
      config
      {
          Yesterday = ["yesterday"; "yest" ; "ye"]
          Today = ["today"; "tdy" ; "now"]
          Tomorrow = ["tomorow";"tomorrow";"tommorrow";"tommorow";"tmr"]
          Day = ["days"; "day"]
          Week = ["weeks"; "week"]
          Fortnight = ["fortnight" ; "fortnights"]
          Month = ["months" ; "month"]
          Year = ["years" ; "year"]
          BackShiftKeywords = ["before"]
          ForwardShiftKeywords = ["from"; "after"]
      }

  let staticShifts =
    [
      {StaticApply = id ; Labels = conf.Today; Priority = 1}
      {StaticApply = addDay 1 ; Labels = conf.Tomorrow; Priority = 2}
      {StaticApply = addDay -1 ; Labels = conf.Yesterday; Priority = 3}
    ]

  let basicShifts =
    [
      {Labels = conf.Day ; Apply = addDay}
      {Labels = conf.Week ; Apply = addWeek}
      {Labels = conf.Fortnight ; Apply = (fun x -> addWeek (2*x))}
      {Labels = conf.Month ; Apply = addMonth}
      {Labels = conf.Year ; Apply = addYear}
    ]

  let relativeShifts =
    [
      {RelativeShift.Labels = ["Monday"; "mon" ; "week"] ; Apply = nextOccurenceOf DayOfWeek.Monday}
      {RelativeShift.Labels = ["Tuesday" ; "tue"] ; Apply = nextOccurenceOf DayOfWeek.Tuesday}
      {RelativeShift.Labels = ["Wednesday" ; "wed"] ; Apply = nextOccurenceOf DayOfWeek.Wednesday}
      {RelativeShift.Labels = ["Thursday" ; "thu"] ; Apply = nextOccurenceOf DayOfWeek.Thursday}
      {RelativeShift.Labels = ["Friday" ; "fri"] ; Apply = nextOccurenceOf DayOfWeek.Friday}
      {RelativeShift.Labels = ["Saturday" ; "sat" ; "weekend"] ; Apply = nextOccurenceOf DayOfWeek.Saturday}
      {RelativeShift.Labels = ["Sunday"; "sun"] ; Apply = nextOccurenceOf DayOfWeek.Sunday}
      {RelativeShift.Labels = ["month"] ; Apply = nextMonth}
      {RelativeShift.Labels = ["year"] ; Apply = nextYear}
    ]

  let combinedParser =
    let pars, refPar = createParserForwardedToRef()
    refPar := choice [
      InternalParsers.relativeAnchorP staticShifts
      InternalParsers.agoP basicShifts
      InternalParsers.forwardShiftP conf.ForwardShiftKeywords basicShifts pars
      InternalParsers.backShiftP conf.BackShiftKeywords basicShifts pars
      InternalParsers.nextP relativeShifts
      InternalParsers.fallbackP
      ]
    pars

  let newParser = NewParsers.completeP

  member this.Parse(arg:string, ?baseDate : DateTime) =
    let bDate = defaultArg baseDate DateTime.Now
    //match run (spaces >>. combinedParser .>> eof) arg with
    match run (spaces >>. newParser .>> eof) arg with
      | Success (result, _, _) -> SuccessfulParse(result bDate)
      | Failure(x,y,z) -> FailedParse(x)

  member this.ParseAtEnd(arg:string, ?baseDate : DateTime) =
    let bDate = defaultArg baseDate DateTime.Now
    match run ( manyCharsTillApply anyChar combinedParser (fun x y -> y) .>> eof) arg with
      | Success (result, _, _) -> SuccessfulParse(result bDate)
      | Failure(x,y,z) -> FailedParse(x)
