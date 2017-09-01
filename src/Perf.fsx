#load "Ulid.fs"

open Ulid

#time
List.init 1000000 (fun _ -> Ulid.New)

fsi.ShowDeclarationValues <- false