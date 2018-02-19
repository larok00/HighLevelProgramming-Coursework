// This will work in FSI if the path contains the FAKE binaries (with FakeLib dll).
// This can be compiled from source by downlaoding the github FAKE repository and building it.
// Or, FAKE can be loaded via NuGet.
#I @"C:\github\FSharp.Formatting.tjwc\packages\FAKE\tools"
#r "FakeLib.dll"
open Fake  // for the FAKE functions
let test =
    !!("./*.pptx") // Define a FileIncludes record: !! creates it from a path with wildcards
    |> fun fx -> {fx with BaseDirectory = "lecturesRoot"} // update a field in the record
    |> Seq.toList // convert record (interpreted as sequence) to list
    |> List.map (fun fn -> printfn "%s->" fn; (fn, FileHelper.changeExt ".pdf" fn))