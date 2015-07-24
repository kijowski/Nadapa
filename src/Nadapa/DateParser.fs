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
  // Declaration of independence - special dates, valid only on it's own
  // Today, tomorrow, yesterday - special cases, valid only on it's own
  // Monday, Tuesday, ... - valid on it's own and with some shifts (next, last, previous)
  // Day, week, month - valid only with shifts of all kinds (next, last, 2, 5)

  type SpecificDate =
    | Today
    | Tomorrow
    | Yesterday
    | Specific of DateTime

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
    | Month
    | Year

  type Shift =
    | Date of SpecificDate
    | Weekday of Weekdays
    | RelativeDay of RelativeOffset * Weekdays
    | RelativeShift of RelativeOffset * DateParts * Shift
    | AbsoluteForwardShift of int * DateParts * Shift
    | AbsoluteBackwardShift of int * DateParts * Shift
    | Ago of int * DateParts

module NewParsers =
  open Types
  let relativeDateP : Parser<SpecificDate,unit> =
    choice [
      stringCIReturn "today" Today
      stringCIReturn "tomorrow" Tomorrow
      stringCIReturn "yesterday" Yesterday
    ] .>> spaces

  let relativeOffsetP : Parser<RelativeOffset,unit> =
    choice [
      stringCIReturn "next" Next
      stringCIReturn "previous" Previous
    ] .>> spaces

  let weekdaysP : Parser<Weekdays,unit> =
    choice [
      stringCIReturn "Monday" Monday
      stringCIReturn "Tuesday" Tuesday
      stringCIReturn "Wednesday" Wednesday
      stringCIReturn "Thursday" Thursday
      stringCIReturn "Friday" Friday
      stringCIReturn "Saturday" Saturday
      stringCIReturn "Sunday" Sunday
    ]

  let datePartsP : Parser<DateParts, unit> =
    choice [
      stringCIReturn "Day" Day
      stringCIReturn "Week" Week
      stringCIReturn "Month" Month
      stringCIReturn "Year" Year
    ]

  let completeP : Parser<Shift,unit> =
    let pars, refPar = createParserForwardedToRef()
    refPar :=
      choice [
        relativeDateP |>> Date
        weekdaysP |>> Weekday
        relativeOffsetP .>>. weekdaysP |>> RelativeDay
        relativeOffsetP .>>. datePartsP .>>. pars |>> fun ((x,y),z) -> RelativeShift(x,y,z)
        (pint32 .>> spaces .>>. datePartsP .>> skipStringCI "after" .>>. pars) |>> fun ((x,y),z) -> AbsoluteForwardShift(x,y,z)
        (pint32 .>> spaces .>>. datePartsP .>> skipStringCI "before" .>>. pars) |>> fun ((x,y),z) -> AbsoluteBackwardShift(x,y,z)
        (pint32 .>> spaces .>>. datePartsP .>> skipStringCI "ago") |>> Ago
      ]
    pars
    //let combinedParser =
    //  let pars, refPar = createParserForwardedToRef()
    //  refPar := choice [
    //    InternalParsers.relativeAnchorP staticShifts
    //    InternalParsers.agoP basicShifts
    //    InternalParsers.forwardShiftP conf.ForwardShiftKeywords basicShifts pars
    //    InternalParsers.backShiftP conf.BackShiftKeywords basicShifts pars
    //    InternalParsers.nextP relativeShifts
    //    InternalParsers.fallbackP
    //    ]
    //  pars

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

  //let shiftTypeP =
  //  choice [
  //    stringCIReturn "next" Next
  //    stringCIReturn "previous" Previous
  //    pint32 |>> fun size -> Countable size
  //    preturn Next
  //  ] .>> spaces
  //
  //let shift2P (shift:Shift) =
  //  createParser shift.Apply shift.Labels
  //
  //let shiftsP (shifts:Shift seq) =
  //  shifts |> Seq.map shift2P |> choice
  //
  //let combP (shifts:Shift seq) =
  //  pipe2
  //    (shiftTypeP)
  //    (shiftsP shifts)
  //    (|>)
  //
  //let directionP =
  //  choice[
  //    anyLabel ["from"] >>% Forward
  //    anyLabel ["before"] >>% Backward
  //  ]
  //
  //let comb2P (shifts:Shift seq) =
  //  pipe2
  //    (combP shifts)
  //    directionP
  //    (<|)


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


  member this.Parse(arg:string, ?baseDate : DateTime) =
    let bDate = defaultArg baseDate DateTime.Now
    match run (spaces >>. combinedParser .>> eof) arg with
      | Success (result, _, _) -> SuccessfulParse(result bDate)
      | Failure(x,y,z) -> FailedParse(x)

  member this.ParseAtEnd(arg:string, ?baseDate : DateTime) =
    let bDate = defaultArg baseDate DateTime.Now
    match run ( manyCharsTillApply anyChar combinedParser (fun x y -> y) .>> eof) arg with
      | Success (result, _, _) -> SuccessfulParse(result bDate)
      | Failure(x,y,z) -> FailedParse(x)
