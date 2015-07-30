namespace Nadapa
module Evaluation =
  open System
  open Domain

  [<AutoOpen>]
  module Utils =
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
    let previousOccurenceOf weekday (date:DateTime) =
      let rec noo (d:DateTime) =
        if d.DayOfWeek = weekday
        then d
        else noo (addDay -1 d)
      noo (addDay -1 date)
    let nextWeek =
      nextOccurenceOf DayOfWeek.Monday
    let nextMonth (date:DateTime) =
      let temp = addMonth 1 date
      DateTime(temp.Year, temp.Month, 1)
    let nextYear (date:DateTime) =
      DateTime(date.Year+1, 1, 1)
    let lastWeek (date:DateTime) =
      addDay 1 date
      |> previousOccurenceOf DayOfWeek.Monday
      |> previousOccurenceOf DayOfWeek.Monday
    let lastMonth (date:DateTime) =
      let temp = addMonth -1 date
      DateTime(temp.Year, temp.Month, 1)
    let lastYear (date:DateTime) =
      DateTime(date.Year-1, 1, 1)
    let specificDate (spDate:DateTime) =
      fun d -> spDate

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
