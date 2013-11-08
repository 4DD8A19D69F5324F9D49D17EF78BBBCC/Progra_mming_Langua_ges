(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

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

(**** you can put all your code here ****)

fun only_capitals xs = 
    List.filter (fn s => Char.isUpper(String.sub(s,0))) xs

fun longest_string1 xs =
    let fun helper(s,acc) = if String.size s > String.size acc then s else acc
    in
	foldl helper "" xs
    end

fun longest_string2 xs =
    let fun helper(s,acc) = if String.size s >= String.size acc then s else acc
    in
	foldl helper "" xs
    end


fun longest_string_helper f xs =
    foldl (fn (s,acc) => if f(String.size s,String.size acc) then s else acc) "" xs

val longest_string3 = longest_string_helper (fn (a,b) => a>b)
val longest_string4 = longest_string_helper (fn (a,b) => a>=b)
val longest_capitalized = longest_string1 o only_capitals
fun rev_string s = (implode o rev o explode) s

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => (case (f x) of
		       SOME v => v
		     | NONE => first_answer f xs')

fun all_answers f xs =
    let fun helper (xs,acc) = 
	    case xs of 
		[] => acc
	      | x::xs' => case (f x) of
			      SOME v => helper(xs',SOME((valOf acc)@v) )
			    | NONE => NONE
    in
	helper(xs,SOME [])
    end
		       

val count_wildcards = g (fn()=> 1) (fn x => 0) 
val count_wild_and_variable_lengths = g (fn()=> 1) (fn s => String.size s)
fun count_some_var (str,pat) = 
    g (fn() => 0) (fn s => if s = str then 1 else 0) pat 





  
fun check_pat pat =
    let fun contains_duplicate xs =
	    case xs of
		[] => false
	      | x::xs' => if List.exists (fn s => s=x ) xs' then true
			  else contains_duplicate(xs')
	fun extract_string pat =
	    case pat of
		Variable s => [s] 
	     |  TupleP ps => (case ps of 
				  [] => []
				| p::ps' => extract_string(p) @ extract_string(TupleP ps'))
	     | _ => []
    in
	not(contains_duplicate(extract_string pat))
    end


fun match (v,pat) =
    case pat of
	Wildcard => SOME []
     | Variable s => SOME[(s,v)]
     | UnitP =>  (case v of Unit => SOME [] 
			 |  _ => NONE)
     | ConstP c => (case v of Const c2 => if c=c2 then SOME [] else NONE
			    | _ => NONE)
     | TupleP ps => (case v of Tuple vs => if (length ps) = (length vs) then all_answers match (ListPair.zip(vs,ps)) else NONE
			     | _ => NONE)
     | ConstructorP (s,p) => (case v of Constructor(s2,v') => if s=s2 then match(v',p) else NONE 
				      | _ => NONE)

fun first_match v ps =
    SOME (first_answer match (ListPair.zip(List.tabulate(length ps,fn x=>v),ps)))
    handle NoAnswer => NONE
