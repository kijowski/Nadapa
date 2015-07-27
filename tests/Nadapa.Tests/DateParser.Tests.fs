namespace Nadapa.Tests
open System
open NUnit.Framework
open Nadapa

[<TestFixture>]
type BasicParsing() =
    let baseTestDate = DateTime(2015, 1,5)
    let sut = DateParser()

    [<Test>]
    [<TestCase("today")>]
    [<TestCase("tdy")>]
    [<TestCase("now")>]
    member x.``today parse OK``(input : string) =
        let expected = SuccessfulParse(baseTestDate)
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("tomorrow")>]
    [<TestCase("tmr")>]
    [<TestCase("tomorow")>]
    [<TestCase("tommorow")>]
    [<TestCase("tommorrow")>]
    member x.``tommorrow parse OK``(input : string) =
        let expected = SuccessfulParse(baseTestDate.AddDays(1.))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("yesterday")>]
    [<TestCase("ye")>]
    [<TestCase("yest")>]
    member x.``yesterday parse OK``(input : string) =
        let expected = SuccessfulParse(baseTestDate.AddDays(-1.))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("2 days from now")>]
    [<TestCase("2 days after today")>]
    member x.``simple forward day shifts parse OK``(input : string) =
        let expected = SuccessfulParse(baseTestDate.AddDays(2.))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("3 days before now")>]
    [<TestCase("4 days before tomorrow")>]
    member x.``simple backward day shifts parse OK``(input : string) =
        let expected = SuccessfulParse(baseTestDate.AddDays(-3.))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("2 weeks from now")>]
    [<TestCase("2 week after today")>]
    member x.``simple forward week shifts parse OK``(input : string) =
        let expected = SuccessfulParse(baseTestDate.AddDays(14.))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("1 fortnight from now")>]
    [<TestCase("1 fortnights after today")>]
    member x.``simple forward fortnight shifts parse OK``(input : string) =
        let expected = SuccessfulParse(baseTestDate.AddDays(14.))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("2 months from now")>]
    [<TestCase("2 month after today")>]
    member x.``simple forward month shifts parse OK``(input : string) =
        let expected = SuccessfulParse(baseTestDate.AddMonths(2))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("2 year from now")>]
    [<TestCase("2 years after today")>]
    member x.``simple forward year shifts parse OK``(input : string) =
        let expected = SuccessfulParse(baseTestDate.AddYears(2))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("3 weeks before now")>]
    [<TestCase("4 week before 7 days after today")>]
    member x.``simple backward week shifts parse OK``(input : string) =
        let expected = SuccessfulParse(baseTestDate.AddDays(-21.))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("3 days before 2 days before yesterday")>]
    [<TestCase("4 days before 1 days before yesterday")>]
    member x.``complex backward day shifts parse OK``(input : string) =
        let expected = SuccessfulParse(baseTestDate.AddDays(-6.))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("3 days before 2 days after yesterday")>]
    [<TestCase("4 days before 3 days after yesterday")>]
    member x.``complex backward and forward day shifts parse OK``(input : string) =
        let expected = SuccessfulParse(baseTestDate.AddDays(-2.))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("3 days ago")>]
    member x.``ago based backward day shifts parse OK``(input : string) =
        let expected = SuccessfulParse(baseTestDate.AddDays(-3.))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("2012-03-06")>]
    [<TestCase("2012/03/06")>]
    member x.``ISO date literals parse OK``(input : string) =
        let expected = SuccessfulParse(DateTime(2012,3,6))
        printf "%A" (sut.Parse(input,baseTestDate))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("next monday")>]
    [<TestCase("next mon")>]
    member x.``next monday on monday parse OK``(input : string) =
        let expected = SuccessfulParse(DateTime(2015,1,12))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("next tuesday")>]
    [<TestCase("next tue")>]
    member x.``next thuesday on monday parse OK``(input : string) =
        let expected = SuccessfulParse(DateTime(2015,1,6))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("next wednesday")>]
    [<TestCase("next wed")>]
    member x.``next wednesday on monday parse OK``(input : string) =
        let expected = SuccessfulParse(DateTime(2015,1,7))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("next thursday")>]
    [<TestCase("next thu")>]
    member x.``next thursday on monday parse OK``(input : string) =
        let expected = SuccessfulParse(DateTime(2015,1,8))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("next friday")>]
    [<TestCase("next fri")>]
    member x.``next friday on monday parse OK``(input : string) =
        let expected = SuccessfulParse(DateTime(2015,1,9))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("next saturday")>]
    [<TestCase("next sat")>]
    member x.``next saturday on monday parse OK``(input : string) =
        let expected = SuccessfulParse(DateTime(2015,1,10))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("next sunday")>]
    [<TestCase("next sun")>]
    member x.``next sunday on monday parse OK``(input : string) =
        let expected = SuccessfulParse(DateTime(2015,1,11))
        Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("next week")>]
    member x.``next week parse ok``(input : string) =
      let expected = SuccessfulParse(DateTime(2015,1,12))
      Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("next weekend")>]
    member x.``next weekend parse ok``(input : string) =
      let expected = SuccessfulParse(DateTime(2015,1,10))
      let actual = sut.Parse(input,baseTestDate)
      Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("next month")>]
    member x.``next month parse ok``(input : string) =
      let expected = SuccessfulParse(DateTime(2015,2,1))
      let actual = sut.Parse(input,baseTestDate)
      Assert.AreEqual(expected, sut.Parse(input,baseTestDate))

    [<Test>]
    [<TestCase("next year")>]
    member x.``next year parse ok``(input : string) =
      let expected = SuccessfulParse(DateTime(2016,1,1))
      Assert.AreEqual(expected, sut.Parse(input,baseTestDate))


    [<Test>]
    [<TestCase("toddday")>]
    [<TestCase("tmrm")>]
    [<TestCase("random input")>]
    [<TestCase("today today")>]
    member x.``bad input should not be parsed correctly``(input : string) =
        match sut.Parse(input,baseTestDate) with
            | SuccessfulParse(date) -> Assert.Fail("Error: {0} should not produce correct date: {1}", input, date)
            | FailedParse(message) -> Assert.Pass("Success: got following error: {0}", message)

[<TestFixture>]
type ComplexParsing() =
    [<Test>]
    member x.``date at the end parse OK``() =
        let sut = DateParser()
        let expected = SuccessfulParse(DateTime(2012,3,6))
        let actual = sut.ParseAtEnd("blablab alb  alb asd aas 2012-03-06")
        Assert.AreEqual(expected, actual)
