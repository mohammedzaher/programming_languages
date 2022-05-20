datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank
datatype color = Red | Black
datatype move = Discard of card | Draw 
exception IllegalMove

fun same_string(s1 : string, s2 : string) = 
    s1 = s2

fun append ([],ys) = ys
| append (x::xss,ys) = x :: append(xss,ys)

fun rev2 lst =
  let fun aux(lst,acc) =
    case lst of
      [] => acc
    | x::xs => aux(xs, x::acc)
in
  aux(lst,[])
end

(* put your solutions for problem 1 here *)

fun all_except_option(x, xs) =
  case xs of
       [] => NONE
     | y::ys => if same_string(y, x)
                  then SOME ys
                  else case all_except_option(x, ys) of
                            NONE => NONE
                          | SOME z => SOME(y::z)

(* The Solutions for Problems 2 *)
fun get_substitutions1(xs, s) =
  case xs of
       [] => []
     | y :: ys => case all_except_option(s, y) of
                       NONE => []
                     | SOME i => append(i, get_substitutions1(ys, s))


(* The Solutions for Problem 3 *)
fun get_substitutions2(xs, s) =
let fun f([], acc) = acc
       |f(z :: zs, acc) = f(zs, append(acc, (case all_except_option(s, z) of
                                                  NONE => []
                                                | SOME i => i)))
in
  f(xs, [])
end

(* The Solution of Probelm 4 *)
fun similar_names (x, {first=a, middle=b, last=c}) =
let fun f(xs) =
case xs of
     [] => []
   | z::zs => {first = z, middle = b, last = c} :: f(zs)
in
  f(a :: get_substitutions2(x, a))
end

(* The Solution of Problem 5 *)
fun card_color x =
  case x of
       (Spades,_) => Black
     | (Clubs,_) => Black
     | (Diamonds,_) => Red
     | (Hearts,_) => Red

(* The Solution of Problem 6 *)
fun card_value x =
  case x of
       (_, Ace) => 11
     | (_, Num x) => x
     | (_,_) => 10

(* The Solution of Problem 7 *)
fun remove_card (cs, c, e) = 
let fun aux(xs, acc) =
      case xs of
           [] => raise e
         | s :: ss => if s=c then append(acc, ss) else aux(ss, s::acc)
in
  aux(cs, [])
end

(* The Solution of Problem 8 *)
fun all_same_color x =
  case x of
       head::(neck::rest) => ((card_color neck = card_color head) andalso all_same_color(neck::rest))
     | _ => true


(* The Solution of Problem 9 *)
fun sum_cards x =
let fun aux(x, acc) =
      case x of
           [] => acc
         | s :: ss => aux(ss, acc+card_value(s))
in
  aux(x, 0)
end

(* The Solution of Problem 10 *)
fun score (x, goal) =
let val m = sum_cards x 
in
  if(m > goal)
  then let val m1 = 3 * (m - goal) in if(all_same_color x) then m1 div 2 else m1 end
  else let val m2 = goal - m in if(all_same_color x) then m2 div 2 else m2 end
end
(* The Solution of Problem 11 *)

fun officiate (cards,plays,goal) =
    let 
        fun loop (current_cards,cards_left,plays_left) =
            case plays_left of
                [] => score(current_cards,goal)
              | (Discard c)::tail => 
                loop (remove_card(current_cards,c,IllegalMove),cards_left,tail)
              | Draw::tail =>
                case cards_left of
                    [] => score(current_cards,goal)
                  | c::rest => if sum_cards (c::current_cards) > goal
                               then score(c::current_cards,goal)
                               else loop (c::current_cards,rest,tail)
    in 
        loop ([],cards,plays)
    end
