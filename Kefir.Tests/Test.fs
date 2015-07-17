namespace Kefir.Tests
open System
open NUnit.Framework
open Kefir

[<TestFixture>]
type Test() = 
    let anchorDate = DateTime(2015, 1,5)

    [<Test>]
    member x.``today should be parsed correctly``() =
        let sut = DateParser(anchorDate)
        let expected = SuccessfulParse(anchorDate)
        Assert.AreEqual(expected, sut.Parse("today"))
        Assert.AreEqual(expected, sut.Parse("tdy"))
        Assert.AreEqual(expected, sut.Parse("now"))

    [<Test>]
    member x.``tommorrow should be parsed correctly``() =
        let sut = DateParser(anchorDate)
        let expected = SuccessfulParse(anchorDate.AddDays(1.))
        Assert.AreEqual(expected, sut.Parse("tomorrow"))
        Assert.AreEqual(expected, sut.Parse("tmr"))
        Assert.AreEqual(expected, sut.Parse("tomorow"))
        Assert.AreEqual(expected, sut.Parse("tommorow"))
        Assert.AreEqual(expected, sut.Parse("tommorrow"))

    [<Test>]
    member x.``yesterday should be parsed correctly``() =
        let sut = DateParser(anchorDate)
        let expected = SuccessfulParse(anchorDate.AddDays(-1.))
        Assert.AreEqual(expected, sut.Parse("yesterday"))
        Assert.AreEqual(expected, sut.Parse("yest"))
        Assert.AreEqual(expected, sut.Parse("ye"))
    [<Test>]
    member x.``simple forward day shifts should be parsed correctly``() =
        let sut = DateParser(anchorDate)
        let expected = SuccessfulParse(anchorDate.AddDays(2.))
        Assert.AreEqual(expected, sut.Parse("2 days from now"))
        Assert.AreEqual(expected, sut.Parse("2 days after today"))
    [<Test>]
    member x.``simple backward day shifts should be parsed correctly``() =
        let sut = DateParser(anchorDate)
        let expected = SuccessfulParse(anchorDate.AddDays(-3.))
        Assert.AreEqual(expected, sut.Parse("3 days before now"))
        Assert.AreEqual(expected, sut.Parse("4 days before tomorrow"))
    [<Test>]
    member x.``complex backward day shifts should be parsed correctly``() =
        let sut = DateParser(anchorDate)
        let expected = SuccessfulParse(anchorDate.AddDays(-6.))
        Assert.AreEqual(expected, sut.Parse("3 days before 2 days before yesterday"))
    [<Test>]
    member x.``complex backward and forward day shifts should be parsed correctly``() =
        let sut = DateParser(anchorDate)
        let expected = SuccessfulParse(anchorDate.AddDays(-2.))
        Assert.AreEqual(expected, sut.Parse("3 days before 2 days after yesterday"))
    [<Test>]
    member x.``ago based backward day shifts should be parsed correctly``() =
        let sut = DateParser(anchorDate)
        let expected = SuccessfulParse(anchorDate.AddDays(-3.))
        Assert.AreEqual(expected, sut.Parse("3 days ago"))
    [<Test>]
    member x.``ISO date literals should be parsed correctly``() =
        let sut = DateParser(anchorDate)
        let expected = SuccessfulParse(DateTime(2012,3,5))
        let actual = sut.Parse("20120306")
        Assert.AreEqual(expected, actual)

