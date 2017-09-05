module Tests

open Expecto
open Ulid

[<Tests>]
let tests =
  testList "Ulid tests" [
    testCase "Creates a new Ulid of the correct length" <| fun _ ->
      let target = Ulid.New.Value
      Expect.equal target.Length 26 "Incorrect Ulid length"

    testCase "Creates a new Ulid from timestamp" <| fun _ ->
      let target = Ulid.FromTimestamp(1469918176385L).Value.Substring(0, 10)
      Expect.equal target "01ARYZ6S41" "Incorrect time part"
  ]
