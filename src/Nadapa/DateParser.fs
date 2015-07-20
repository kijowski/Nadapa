namespace Nadapa
open System
open FParsec


type DateTransform = DateTime -> DateTime

type ParseResult =
  | SuccessfulParse of DateTime
  | FailedParse of string

type ShiftType =
  | DayBased of (int -> DateTransform)
  | Manual of DateTransform

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
    noo (addDay 1 date)
  let nextWorkingDay (date:DateTime) =
    match date.DayOfWeek with
      | DayOfWeek.Friday -> addDay 3 date
      | DayOfWeek.Saturday -> addDay 2 date
      | _ -> addDay 1 date
  let nextWeekend (date:DateTime) =
    nextOccurenceOf DayOfWeek.Saturday

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

  let todayP labels =
    createParser (id) labels
  let tomorrowP labels =
    createParser (addDay 1) labels
  let yesterdayP labels =
    createParser (addDay -1) labels

  let shiftP (shiftTypes:AbsoluteShift list) =
    pint32 .>> spaces
    .>>. (shiftTypes |> List.map(fun shiftType -> (createParser shiftType.Apply shiftType.Labels)) |> choice)

  let forwardShiftP labels (shiftTypes:AbsoluteShift list) parser=
    attempt (
      pipe2
        (shiftP shiftTypes .>> createConsumingParser labels)
        parser
        (fun (size, shift) dateTransf -> shift ((size)) >> dateTransf)
    )

  let backShiftP labels (shiftTypes:AbsoluteShift list) parser =
    attempt (
      pipe2
        (shiftP shiftTypes .>> createConsumingParser labels)
        parser
        (fun (size, shift) dateTransf -> shift ((-size)) >> dateTransf)
    )

  let agoP (shiftTypes:AbsoluteShift list) =
    attempt (
      shiftP shiftTypes
      .>> ParserCreators.createConsumingParser ["ago"]
      |>> fun (size, shift) -> shift ((-size))
    )

  let nextP (shifts:RelativeShift list)=
    attempt (
      skipStringCI "next" .>> spaces  >>. (shifts |> List.map(fun shift -> createParser shift.Apply shift.Labels) |> choice)
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
      {RelativeShift.Labels = ["Monday"; "mon"] ; Apply = nextOccurenceOf DayOfWeek.Monday}
      {RelativeShift.Labels = ["Tuesday" ; "tue"] ; Apply = nextOccurenceOf DayOfWeek.Tuesday}
      {RelativeShift.Labels = ["Wednesday" ; "wed"] ; Apply = nextOccurenceOf DayOfWeek.Wednesday}
      {RelativeShift.Labels = ["Thursday" ; "thu"] ; Apply = nextOccurenceOf DayOfWeek.Thursday}
      {RelativeShift.Labels = ["Friday" ; "fri"] ; Apply = nextOccurenceOf DayOfWeek.Friday}
      {RelativeShift.Labels = ["Saturday" ; "sat"] ; Apply = nextOccurenceOf DayOfWeek.Saturday}
      {RelativeShift.Labels = ["Sunday"; "sun"] ; Apply = nextOccurenceOf DayOfWeek.Sunday}
    ]

  let combinedParser =
    let pars, refPar = createParserForwardedToRef()
    refPar := choice [
      InternalParsers.todayP conf.Today
      InternalParsers.tomorrowP conf.Tomorrow
      InternalParsers.yesterdayP conf.Yesterday
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
      | Success (result, _, __) -> SuccessfulParse(result bDate)
      | Failure(x,y,z) -> FailedParse(x)

  member this.ParseAtEnd(arg:string, ?baseDate : DateTime) =
    let bDate = defaultArg baseDate DateTime.Now
    match run ( manyCharsTillApply anyChar combinedParser (fun x y -> y) .>> eof) arg with
      | Success (result, _, __) -> SuccessfulParse(result bDate)
      | Failure(x,y,z) -> FailedParse(x)
