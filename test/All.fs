module Tests

open Expecto
open Ulid

[<Tests>]
let tests =
  testList "Ulid tests" [
    testCase "Creates a new Ulid of the correct length" <| fun _ ->
      let subject = Ulid.New.Value
      Expect.equal subject.Length 26 "Incorrect Ulid length"

    testCase "Creates a new Ulid from timestamp" <| fun _ ->
      let subject = Ulid.FromTimestamp(1469918176385L).Value.Substring(0, 10)
      Expect.equal subject "01ARYZ6S41" "Incorrect time part"
  ]
