let rec countDupesInner
  (first:int) (previous:int) (total:int) (s:int list) :int =
  match s with
  | []           -> if (first = previous) then (total + previous) else total
  | head :: tail ->
      let newTotal = if (head = previous) then (total + previous) else total
      countDupesInner first head newTotal tail

let charToInt (c:char) :int =
  let value = int c - int '0'
  if (value >= 0 && value <= 9) then value
    else failwith (string c + " is not a digit")

let countDupes (s:string) :int =
  match (List.map charToInt (Seq.toList s)) with
  | [] -> 0
  | head :: tail -> countDupesInner head head 0 tail
