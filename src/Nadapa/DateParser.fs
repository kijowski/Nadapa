namespace Nadapa
open System
open FParsec
open FSharp.Configuration
open Domain

/// Parser configuration.
/// To load new YAML based configuration use Load method.
type ParserConfig = YamlConfig<"config.yaml">

/// <summary>Natural date parser.</summary>
/// <param name="configuration">Parser configuration.</param>
/// <param name="caseSensitive">Set to true for case sensitive parsing.</param>
type DateParser(?configuration:ParserConfig, ?caseSensitive:bool) =
  let config = defaultArg configuration (ParserConfig())
  let caseSens = defaultArg caseSensitive false

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
    |> Seq.map(fun item ->
        (item.label |> Seq.toList),
        Specific (DateTime.ParseExact(item.date,[|"yyyy-MM-dd" ; "yyyyMMdd" ; "yyyy/MM/dd"|], Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.None)))
    |> Seq.toList

  let anyLabel (labels:string seq) =
    labels
    |> Seq.sortBy (fun x -> - x.Length)
    |> Seq.map (if caseSens then skipString else skipStringCI)
    |> choice
    .>> spaces

  let createP (elements : (string list * 'a) seq) =
    let parser = if caseSens then stringReturn else stringCIReturn
    elements
    |> Seq.collect(fun (labels, retValue) -> labels |> Seq.map(fun label -> label,retValue))
    |> Seq.sortBy(fun (label,_) -> - label.Length)
    |> Seq.map(fun (lab,retVal) -> parser lab retVal .>> spaces)
    |> choice
    .>> spaces

  let specificDateP : Parser<SpecificDate,unit>  =
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
    let parser, refPar = createParserForwardedToRef()
    refPar :=
      choice [
        specificDateP |>> Date
        relativeDateP |>> Date
        weekdaysP |>> Weekday
        relativeP |>> Relative
        absoluteP parser |>> Absolute
        dateFormatP |>> Date
      ]
    parser
    |>> Evaluation.evaluate


  /// <summary>Try to parse date from supplied string.</summary>
  /// <param name="arg">String containing date to be parsed.</param>
  /// <param name="baseDate">Optional date used as a base date. Leave default for DateTime.Now</param>
  /// <returns>Option type containing parsed date.</returns>
  member x.TryParse(arg:string, ?baseDate : DateTime) =
    let bDate = defaultArg baseDate DateTime.Now
    match run (spaces >>. completeP .>> eof) arg with
      | Success (result, _, _) -> Some(result bDate)
      | Failure(_,_,_) -> None
