namespace Nadapa
open System

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
