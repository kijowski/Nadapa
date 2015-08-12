namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Nadapa")>]
[<assembly: AssemblyProductAttribute("Nadapa")>]
[<assembly: AssemblyDescriptionAttribute("Simple micro-library for human readable dates parsing.")>]
[<assembly: AssemblyVersionAttribute("1.0.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.0"
