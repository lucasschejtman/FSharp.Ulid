namespace Ulid

open System

module internal Extensions =
    type DateTime with
        static member UnixTime =
            Convert.ToInt64 (DateTime.UtcNow.Subtract(DateTime(1970, 1, 1)).TotalSeconds)

    type Random with
        member this.GetSequence(min, max) =
            Seq.initInfinite (fun _ -> this.Next(min, max))

module internal Helpers =
    let (|Positive|NotPositive|) num =
        if num > 0 then Positive else NotPositive

[<AutoOpen>]
module Ulid =
    open Extensions
    open Helpers

    let encoding        = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
    let encodingLength  = 32L

    let concatEncoding chars =
        List.fold (fun acc char -> acc + (encoding.Chars char).ToString()) "" chars

    let randoms length min max =
        Random().GetSequence(min, max)
            |> Seq.take length
            |> Seq.toList

    let encodeTime timestamp length =
        let rec loop ts len chars =
            match len with
            | Positive    -> let char = ts % encodingLength
                             let acc = (ts - char) / encodingLength
                             loop acc (len - 1) ((Convert.ToInt32 char)::chars)
            | NotPositive -> chars

        loop timestamp length []


    type Ulid private (value) =
        member self.Value with get() = value

        override self.ToString() = value
        
        static member private Generate timestamp =
            let timePart    = encodeTime timestamp 10
            let randomPart  = randoms 16 0 31
            List.append timePart randomPart
                |> concatEncoding

        static member private ToUlid =
            Ulid.Generate >> Ulid

        static member FromTimestamp timestamp =
            timestamp |> Ulid.ToUlid

        static member New =
            DateTime.UnixTime |> Ulid.ToUlid
