namespace Nadapa
open System

[<AutoOpen>]
module DateUtils =
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
  let nextWeekend (date:DateTime)=
    nextOccurenceOf DayOfWeek.Saturday
  let nextWeek =
    nextOccurenceOf DayOfWeek.Monday
  let nextMonth (date:DateTime) =
    DateTime(date.Year, (date.Month + 1 % 12), 1)
  let nextYear (date:DateTime) =
    DateTime(date.Year+1, 1, 1)
  let lastWeek (date:DateTime) =
    previousOccurenceOf DayOfWeek.Monday (previousOccurenceOf DayOfWeek.Monday (addDay 1 date))
  let lastWeekend (date:DateTime) =
    previousOccurenceOf DayOfWeek.Saturday
  let lastMonth (date:DateTime) =
    let temp = addMonth -1 date
    DateTime(temp.Year, temp.Month, 1)
  let lastYear (date:DateTime) =
    DateTime(date.Year-1, 1, 1)
  let specificDate (spDate:DateTime) =
    fun d -> spDate
