namespace Nadapa
open System
open FParsec

module Configuration =
  open Domain
  open FSharp.Configuration

  type Config = YamlConfig<"config.yaml">

  let config = Config()

  let weekdays =
    [
      config.weekdays.monday |> Seq.toList, DayOfWeek.Monday
      config.weekdays.tuesday |> Seq.toList ,  DayOfWeek.Tuesday
      config.weekdays.wednesday |> Seq.toList,  DayOfWeek.Wednesday
      config.weekdays.thursday |> Seq.toList, DayOfWeek.Thursday
      config.weekdays.friday |> Seq.toList,  DayOfWeek.Friday
      config.weekdays.saturday |> Seq.toList,  DayOfWeek.Saturday
      config.weekdays.sunday |> Seq.toList, DayOfWeek.Sunday
    ]

  let dateParts =
    [
      config.dateparts.day |> Seq.toList , Day
      config.dateparts.week |> Seq.toList ,  Week
      config.dateparts.fortnight |> Seq.toList , Fortnight
      config.dateparts.month |> Seq.toList ,  Month
      config.dateparts.year |> Seq.toList , Year
    ]

  let relativeOffsets =
    [
      config.relative.next |> Seq.toList , Next
      config.relative.last |> Seq.toList , Previous
    ]

  let relativeDates =
    [
      config.special.today |> Seq.toList, Today
      config.special.tomorrow |> Seq.toList, Tomorrow
      config.special.yesterday |> Seq.toList, Yesterday
    ]

  let specificDates =
    config.specific
    |> Seq.map(fun item -> [item.label], Specific (DateTime.ParseExact(item.date,[|"yyyy-MM-dd" ; "yyyyMMdd" ; "yyyy/MM/dd"|],Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.None)))
    |> Seq.toList

module Parsers =
  open Domain
  open Configuration

  let createP x :Parser<'a,unit> = createParser config.caseSensitive x

  let specificDateP =
    specificDates |> createP

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
      anyLabel config.absolute.after >>. par |>> After
      anyLabel config.absolute.before >>. par |>> Before
      anyLabel config.absolute.ago >>% Ago
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
        specificDateP |>> Date
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
