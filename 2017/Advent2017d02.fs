module Advent2017d02

let lineToInts (line:string) :(int list) =
  List.map int (Array.toList (line.Split '\t'))

let rec minMaxInner (minSoFar:int) (maxSoFar:int) (xs:int list) =
  match xs with
  | [] -> (minSoFar, maxSoFar)
  | head :: tail ->
      let newMin = min head minSoFar
      let newMax = max head maxSoFar
      minMaxInner newMin newMax tail

let minMax (xs:int list) = minMaxInner (List.head xs) (List.head xs) (List.tail xs)

let rowDifference (xs:int list) :int =
  let (rowMin, rowMax) = minMax xs
  rowMax - rowMin

let rowsChecksum (rows:string list) :int =
  List.sum (List.map rowDifference (List.map lineToInts rows))

let fileChecksum (filename:string) :int =
  let lines = System.IO.File.ReadLines(filename)
  rowsChecksum (Seq.toList lines)
