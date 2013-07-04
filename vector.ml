
type 'a t = {
  mutable base : 'a array;
  mutable len : int;
}

exception Empty

let create () =
  { base = [||]; len = 0 }

let length v = v.len

let capacity v = Array.length v.base

let grow v x required =
  let cap = Array.length v.base in
  let newcap = cap + (max cap required) in
  let newbase = Array.create newcap x in
  for i = 0 to v.len - 1 do
    Array.unsafe_set newbase i (Array.unsafe_get v.base i)
  done;
  v.base <- newbase

let push v x =
  let cap = Array.length v.base in
  if v.len = cap then
    grow v x 1;
  Array.unsafe_set v.base v.len x;
  v.len <- v.len + 1

let contains v x =
  let rec loop i =
    if i = v.len then false
    else if Array.unsafe_get v.base i = x then true
    else loop (i + 1)
  in
    loop 0

let for_all p v =
  let rec loop i =
    if i = v.len then true
    else if not (p (Array.unsafe_get v.base i)) then false
    else loop (i + 1) in
  loop 0

let push_if_unique v x =
  if not (contains v x) then push v x

let pop v =
  if v.len = 0 then
    raise Empty;
  v.len <- v.len - 1;
  Array.unsafe_get v.base v.len

let pop_n v n =
  if v.len < n then
    raise Empty;
  v.len <- v.len - n

let iter f v =
  for i = 0 to v.len - 1 do f (Array.unsafe_get v.base i) done

let iteri f v =
  for i = 0 to v.len - 1 do f i (Array.unsafe_get v.base i) done

let map f v =
  { base = Array.init v.len (fun i -> f (Array.unsafe_get v.base i));
    len = v.len }

let mapi f v =
  { base = Array.init v.len (fun i -> f i (Array.unsafe_get v.base i));
    len = v.len }

let map_to_array f v =
  if v.len = 0 then [||]
  else
    let a = Array.create v.len (f (Array.unsafe_get v.base 0)) in
    for i = 1 to v.len - 1 do
      Array.unsafe_set a i (f (Array.unsafe_get v.base i))
    done;
    a

let fold f init v =
  let len = length v in
  let rec loop i accum =
    if i = len then accum
    else
      loop (i + 1) (f accum (Array.unsafe_get v.base i)) in
  loop 0 init

let member elt v =
  let rec loop i =
    if i = v.len then false
    else if Array.unsafe_get v.base i = elt then true
    else loop (i + 1) in
  loop 0

let memq elt v =
  let rec loop i =
    if i = v.len then false
    else if Array.unsafe_get v.base i == elt then true
    else loop (i + 1) in
  loop 0

let clear v =
  v.len <- 0

let purge v =
  v.base <- [||];
  v.len <- 0

let get v i = v.base.(i)
let set v i x = v.base.(i) <- x
let unsafe_get v i = Array.unsafe_get v.base i
let unsafe_set v i x = Array.unsafe_set v.base i x

let find pred v =
  let rec loop i =
    if i = v.len then v.len
    else if pred v.base.(i) then i
    else loop (i + 1) in
  loop 0

let remove_if f v =
  let rec loop l r =
    if r = v.len then l
    else if f v.base.(r) then loop l (r + 1)
    else (v.base.(l) <- v.base.(r); loop (l + 1) (r + 1)) in
  v.len <- loop 0 0

let sub v l h =
  if l > v.len || h > v.len then
    raise (Invalid_argument "Vector.sub")
  else
    Array.sub v.base l h

let front v =
  if v.len = 0 then
    raise (Invalid_argument "Vector.front");
  v.base.(0)

let back v =
  if v.len = 0 then
    raise (Invalid_argument "Vector.back");
  v.base.(v.len - 1)

let insert v elt pos =
  if pos < 0 || pos > v.len then
    raise (Invalid_argument "Vector.insert");
  let cap = Array.length v.base in
  if v.len = cap then
    grow v elt 1;
  for i = v.len - 1 downto pos do
    v.base.(i + 1) <- v.base.(i)
  done;
  v.base.(pos) <- elt;
  v.len <- v.len + 1

let erase v low high =
  if low < 0 || high > v.len then
    raise (Invalid_argument "Vector.erase")
  else
    let sublen = v.len - high in
    for i = 0 to sublen - 1 do
      v.base.(low + i) <- v.base.(high + i)
    done;
    v.len <- low + sublen

let append_sub v a n =
  if n = 0 then () else begin
    let newlen = v.len + n in
    if newlen > Array.length v.base then begin
      let newbase = Array.create newlen a.(0) in
      Array.blit v.base 0 newbase 0 v.len;
      v.base <- newbase
    end;
    Array.blit a 0 v.base v.len n;
    v.len <- newlen
  end

let append_array v a =
  append_sub v a (Array.length a)

let append a b =
  append_sub a b.base b.len

(* Sigh, the stdlib has no provision to sort an array
 * subsequence. Should probably write a sort. *)
let sort f v =
  if v.len != Array.length v.base then
    v.base <- Array.sub v.base 0 v.len;
  Array.sort f v.base

let to_array v =
  Array.sub v.base 0 v.len

let of_array a =
  { base = Array.copy a; len = Array.length a; }

let of_list xs =
  let a = Array.of_list xs in
  { base = a; len = Array.length a; }

let append_map v f a =
  let len = length a in
  if len = 0 then () else begin
    let one = f (front a) in
    if v.len + len >= Array.length v.base then
      grow v one len
    else
      v.base.(v.len) <- one;
    for i = 1 to len - 1 do
      v.base.(v.len + i) <- f (get a i)
    done;
    v.len <- v.len + len
  end
  (* iter (fun elt -> push v (f elt)) a *)

let partition v pred =
  let a = v.base in
  let len = length v in
  let rec loop l r =
    if r >= len then l else begin
      let elt = a.(r) in
      if pred elt then begin
        a.(r) <- a.(l);
        a.(l) <- elt;
        loop (l + 1) (r + 1)
      end else
        loop l (r + 1)
    end in
  loop 0 0

let range lo hi =
  let v = create () in
  for i = lo to hi do push v i done;
  v

let push_ordered v cmp elt =
  let rec find low high =
    if low = high then low
    else let mid = (low + high) / 2 in
      if cmp elt v.base.(mid) then find low mid
      else if cmp v.base.(mid) elt then find (mid + 1) high
      else mid
  in
  insert v elt (find 0 (length v))

let is_sorted v cmp =
  let len = length v in
  if len = 0 then true
  else
    let rec loop i =
      if i = len - 1 then true
      else if cmp (get v (i + 1)) (get v i) then false
      else loop (i + 1) in
    loop 0

let any pred v =
  let len = length v in
  let rec loop i =
    if i = len then false
    else if pred (get v i) then true
    else loop (i + 1) in
  loop 0
