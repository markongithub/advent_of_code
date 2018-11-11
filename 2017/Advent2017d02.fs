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

let getMatchIfExists set pred =
  let matches = Set.filter pred set
  if Set.isEmpty matches then None else Some(Set.minElement matches)

let rec findDivisorsInner (divisors: Set<int>) (dividends: Set<int>) (remaining: int list) =
  match remaining with
  | [] -> failwith "We did not find a pair"
  | head :: tail ->
// try dividing all the potential dividends by head
// if we get a match, return that
// if it's even and >= 4 try dividing it by all the divisors
// if we get a match return that
// recurse by creating newDivisors and newDividends
      let divides a b = (b % a = 0)
      let dividend = getMatchIfExists dividends (divides head)
      match dividend with
      | Some(d) -> (head, d)
      | None ->
        let isPotentialDividend = head >=4
        let newDividends = lazy(if isPotentialDividend then dividends.Add(head) else dividends)
        let newDivisors = divisors.Add(head)
        let divisor = lazy(getMatchIfExists divisors (fun x -> divides x head))
        let recurse = lazy(findDivisorsInner newDivisors (newDividends.Force()) tail)
        if (not isPotentialDividend) then recurse.Force()
          else match divisor.Force() with
               | Some(d) -> (d, head)
               | None -> recurse.Force()

let findDivisors = findDivisorsInner Set.empty Set.empty

let rowQuotient xs =
  let (x,y) = findDivisors xs
  y / x

let rowsChecksum2 (rows:string list) :int =
  List.sum (List.map rowQuotient (List.map lineToInts rows))

let fileChecksum2 (filename:string) :int =
  let lines = System.IO.File.ReadLines(filename)
  rowsChecksum2 (Seq.toList lines)
