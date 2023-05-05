type shape = 
| Point of int * int;;
(*

Maze by GC with timer ! ! !

|  S   |  0   |  0   |  0  | 
|  0   |  0   |  0   |  0    
|  0   |  0   |  0      0    
|  0   |  0      0      0    
|  0      0      0      0    

*)
let solve_maze ~grid ~start_ ~end_ = 
  let Point(x, y) = end_ in
  let end_x = x in
  let end_y = y in
  let x_dim = Array.length grid.(0) in
  let y_dim = Array.length grid in
  let not_visited (x, y) = 
    if grid.(y).(x) then false
    else true in
  let rec find current (f:unit) = 
    let Point(x, y) = current in
    if (x = end_x) && (y = end_y) then (*grid*) Printf.printf "%s\n%s" "Point was found!" "----- End ------"
    else if x + 1 < x_dim && not_visited((x + 1, y)) then find(Point(x + 1, y))(grid.(y).(x + 1) <- true)
    else if y + 1 < y_dim && not_visited((x, y + 1)) then find(Point(x, y + 1))(grid.(y + 1).(x) <- true)
    else if x - 1 > -1 && not_visited((x - 1, y)) then find(Point(x - 1,y))(grid.(y).(x - 1) <- true)
    else if y - 1 > -1 && not_visited((x, y - 1)) then find(Point(x, y - 1))(grid.(y - 1).(x) <- true)
    else (*grid*) Printf.printf "%s\n%s" "Point was not found!" "----- End ------"
  in find start_ (print_endline "----- Start -----");;

let s = Sys.time();;

let matrix = Array.make_matrix 10_000 10_000 false;;
solve_maze ~grid:(matrix) ~start_:(Point(0, 0)) ~end_:(Point(500, 5000));;

Printf.printf "%s\t%.8f%s\n" "Execution time:" (Sys.time() -. s) "s";;