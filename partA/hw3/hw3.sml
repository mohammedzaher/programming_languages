exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****) 
fun only_capitals xs = List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 xs = foldl (fn (x, y) => if (String.size x > String.size y) then x else y) "" xs

fun longest_string2 xs = foldl (fn (x, y) => if (String.size x < String.size y) then y else x) "" xs

fun longest_string_helper f xs = foldl (fn(x, y) => if f(String.size x, String.size y) then x else y) "" xs

val longest_string3 = longest_string_helper(fn(x, y) => x > y) 

val longest_string4 = longest_string_helper(fn(x, y) => x >= y)

fun longest_capitalized xs = (longest_string1 o only_capitals )xs

fun rev_string s = (implode o List.rev o explode)s

fun first_answer f x =
  case x of
       [] => raise NoAnswer
     | z :: zs => case f z of SOME z => z | _ => first_answer f zs

fun all_answers f x =
let fun answers xs acc =
 case xs of 
     [] => SOME acc
   | y :: ys => case f y of NONE => NONE | SOME z => answers ys (z@acc)
in
 answers x []
end

val count_wildcards  = g (fn () => 1) (fn (x) => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)

fun count_some_var (x, y) = g (fn () => 0) (fn z => if z = x then 1 else 0) y

fun check_pat pat =
let 
  fun f1 x1 acc = 
  case x1 of 
       TupleP ps => foldl (fn (pp, ac) => f1 pp ac) acc ps
      |ConstructorP(_, sp) => f1 sp acc
      |Variable y => y::acc
      |_ => acc
  fun f2 x2 = 
    case x2 of
         [] => true
       | s::sl => not(List.exists (fn x => x=s) sl) andalso f2 sl
in 
 f2 (f1 pat [])
end

fun match (a, b) =
  case (a, b) of
       (_, Wildcard) => SOME[]
     | (v, Variable x) => SOME[(x,v)]
     | (Unit, UnitP) => SOME[]
     | (Const n1, ConstP n2) => if n1 = n2 then SOME[] else NONE
     | (Tuple pv, TupleP ps) => if (List.length pv = List.length ps) then all_answers (fn (x, y) => match (x, y)) (ListPair.zip (pv,ps)) else NONE
     | (Constructor(s2, v), ConstructorP(s1, p)) => if s1=s2 then match(v, p) else NONE
     | (_, _) => NONE

fun first_match v p = SOME(first_answer(fn y => match(v, y)) p)
  handle NoAnswer => SOME[]
