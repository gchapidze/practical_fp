type product_type = ELECTRONIC | BOOK | COSMETIC;;

type product = { name: string; product_type: product_type; price : float};;

let products = [{name = "iPad"; product_type = ELECTRONIC ; price =
  800.0}; {name = "Pride and Prejudice"; product_type = BOOK; price =
  10.0}; {name = "Mac Pro"; product_type = ELECTRONIC; price = 2000.0};
  {name = "Smart TV"; product_type = ELECTRONIC; price = 500.0}];;

let total_price ~collection = 
  List.map(fun product -> product.price)collection 
  |> List.fold_left(( +. ))(0.);;

let price_of_electronics ~collection = 
  List.filter(fun product -> product.product_type = ELECTRONIC)collection 
  |> List.map(fun product -> product.price)
  |> List.fold_left(( +. ))(0.);;

let first_by ~collection ~predicate = 
  List.filter(predicate)collection |> List.hd;;

let contains ~collection ~predicate = 
  List.filter(predicate)collection |> List.length > 0;;

(* contains ~collection:products ~predicate:(fun product -> product.price < 1000.);; *)

let sort ~collection ~compare = List.sort(compare) collection;;

let reverse ~collection = 
  List.fold_left(fun acc p -> p :: acc)([])(collection);;

(* reverse ~collection:products;; *)

let rec take collection ~length = 
  match collection with
  | [] -> []
  | x :: xs -> 
    if length <= 0 then [] 
    else x :: take(xs)~length:(length - 1);;

let take_sort ~collection ~length ~compare =  
  List.sort(compare) collection
  |> take ~length:length;;

let rec take_while ~collection ~predicate = 
  match collection with
  | [] -> []
  | x :: xs -> 
    if predicate x then x :: (take_while ~collection:xs ~predicate:predicate)
    else [];;