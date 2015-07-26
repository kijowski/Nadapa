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
      anyLabel ["Thursday"; "thu"] >>% DayOfWeek.Thursday
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

  let relativeShiftP : Parser<RelativeShift, unit>=
    [
      ["Monday"; "mon"], DayShift DayOfWeek.Monday
      ["Tuesday"; "tue"] , DayShift DayOfWeek.Tuesday
      ["Wednesday"; "wed"] , DayShift DayOfWeek.Wednesday
      ["Thursday"; "thu"] , DayShift DayOfWeek.Thursday
      ["Friday" ; "fri"] , DayShift DayOfWeek.Friday
      ["Saturday" ; "sat" ; "weekend"] , DayShift DayOfWeek.Saturday
      ["Sunday" ; "sun"] , DayShift DayOfWeek.Sunday
      ["Day"; "days"] , PeriodShift Day
      ["Week"; "weeks"] , PeriodShift Week
      ["Fortnight" ; "fortnights"] , PeriodShift Fortnight
      ["Month" ; "months"] , PeriodShift Month
      ["Year" ; "years"] , PeriodShift Year
    ]
    |> createP

  let relativeP =
    relativeOffsetP .>>. relativeShiftP |>> Relative

  let absoluteShiftP par =
    [
      anyLabel ["after"; "from"] >>. par |>> After
      anyLabel ["before"] >>. par |>> Before
      anyLabel ["ago"] >>% AbsoluteShift.Ago
    ]
    |> choice

  let absoluteP par =
    pint32 .>> spaces .>>. datePartsP .>>. absoluteShiftP par |>> fun((x,y),z) -> Absolute(x,y,z)

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
      | PeriodShift Week -> nextOccurenceOf DayOfWeek.Monday
      | PeriodShift Fortnight -> nextOccurenceOf DayOfWeek.Monday >> nextOccurenceOf DayOfWeek.Monday
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
        | Date(d) -> d.Apply
        | Weekday(day) -> nextOccurenceOf day
        | Relative(offset, shift) -> match offset with
                                            | Previous -> previous shift
                                            | Next -> next shift
        | Absolute(size, datePart, shift) -> match shift with
                                                  | Before sh -> (move -size datePart) >> evaluate sh
                                                  | After sh -> (move size datePart) >> evaluate sh
                                                  | AbsoluteShift.Ago -> (move -size datePart)
  let completeP =
    let pars, refPar = createParserForwardedToRef()
    refPar :=
      choice [
        relativeDateP |>> Date
        weekdaysP |>> Weekday
        relativeP
        attempt (absoluteP pars)
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

  member this.Parse(arg:string, ?baseDate : DateTime) =
    let bDate = defaultArg baseDate DateTime.Now
    match run (spaces >>. NewParsers.completeP .>> eof) arg with
      | Success (result, _, _) -> SuccessfulParse(result bDate)
      | Failure(x,y,z) -> FailedParse(x)

  member this.ParseAtEnd(arg:string, ?baseDate : DateTime) =
    let bDate = defaultArg baseDate DateTime.Now
    match run ( manyCharsTillApply anyChar (attempt(NewParsers.completeP)) (fun x y -> y) .>> eof) arg with
      | Success (result, _, _) -> SuccessfulParse(result bDate)
      | Failure(x,y,z) -> FailedParse(x)
