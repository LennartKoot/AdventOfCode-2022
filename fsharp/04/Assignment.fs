module CampCleanup.Assigment

open Utility

type Assigment = { lowerboundSection: int; upperboundSection: int } with
    member public this.fullyContains (a:Assigment) =
        this.lowerboundSection <= a.lowerboundSection &&
        this.upperboundSection >= a.upperboundSection
    member public this.overlaps (a:Assigment) =
        this.upperboundSection >= a.lowerboundSection

let parseAssignment (s:string) =
    splitIntoTwoAndApply s '-' (fun x y -> { lowerboundSection = (int x); upperboundSection = (int y)})
