(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* put your solutions for problem 1 here *)

fun all_except_option (str, lst ) =
    case lst of
	[] => NONE 
      | x::xs => if str = x
		 then SOME xs
		 else case all_except_option(str,xs) of
			  NONE => NONE 
			| SOME lst2 => SOME(x::lst2)
 
fun get_substitutions1 (lstlst, str) =
    case lstlst of
	[] => [] 
      | x::xs => case all_except_option(str ,x) of
		     NONE => get_substitutions1(xs,str)
		   | SOME lst2 => lst2 @ get_substitutions1(xs,str)

fun get_substitutions2 (lstlst , str) = 
    let fun helper(lstlst, str, acc) =
	    case lstlst of
		[] => acc 
	      | x::xs => case all_except_option(str, x) of
			     NONE => helper(xs,str,acc) 
			   | SOME lst2  => helper(xs,str,lst2@acc)
    in
	helper(lstlst,str,[])
    end


fun similar_names(lstlst, name) =
    let fun compose(firstlst, middle, last) = 
	    case firstlst of 
		[] => []
	      | x::xs => {first=x,middle=middle,last=last} :: compose(xs,middle,last)
    in
	case name of
	    {first =f, middle = m ,last =l } => compose(f :: get_substitutions2(lstlst,f),m,l)
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color c =
    case c of 
	(Spades,rank) => Black 
     | (Clubs,rank) => Black 
     | _ => Red

fun card_value c =
    case c of
	(suit, Jack) => 10
      | (suit, Queen) => 10
      | (suit, King) => 10
      | (suit, Ace) => 11
      | (suit, Num(x)) => x
  
	
fun remove_card(cs,c,e) = 
    case all_except_option(c,cs) of
	NONE => raise e
      | SOME lst => lst

fun all_same_color cs = 
    case cs of
	[] => true
     | x::[] => true
     | x1::x2::xs => if (card_color x1) = (card_color x2) 
			       then all_same_color( x2::xs)
			       else false
    

fun sum_cards cs = let fun helper(cs , acc) = 
			   case cs of 
			       [] => acc
			     | x::xs  => helper(xs,acc + (card_value x))
		   in
		       helper(cs,0)
		   end
						   
fun score(cs,goal) = 
    let val tmpsum = sum_cards cs 
	val todiv = if all_same_color cs then 2 else 1
    in
	if tmpsum > goal then (tmpsum - goal) * 3 div todiv
	else (goal-tmpsum) div todiv
    end

fun officiate(cs,ms,goal) = 
    let fun helper(cs,ms,hs,sum,goal) =
	    if sum>goal then score(hs,goal)
	    else
		case ms of
		    [] => score(hs,goal)
		  | Discard(c)::xs => helper(cs, xs, remove_card(hs,c,IllegalMove) , sum - card_value(c) , goal)
		  | Draw::xs => case cs of 
				    [] => score(hs,goal)
				  | c :: cs0 => helper(cs0, xs , c::hs, sum + card_value(c) , goal)
    in
	helper(cs,ms,[],0,goal)
    end

fun score_challenge(cs,goal) = 
    let
	fun calc(sum,goal) = 
	    if sum>goal then (sum - goal)*3
	    else goal - sum
	fun aces cs =
	    case cs of
		[] => 0 
	      | (suit,Ace)::xs => 1+ aces xs 
	      | x::xs => aces xs
	
	fun mi(sum,aces) =
	    let val x = calc(sum,goal) in
		if aces>0 then Int.min(x, mi(sum-10,aces-1))
		else x
	    end
	val tsum = sum_cards cs
	val tdiv = if all_same_color cs then 2 else 1		
    in
	mi(tsum,aces cs) div tdiv
    end

fun officiate_challenge(cs,ms,goal) =
    let fun helper(cs,ms,hs,sum,goal,aces) =
	    if sum-10*aces>goal then score_challenge(hs,goal)
	    else
		case ms of
		    [] => score_challenge(hs,goal)
		  | Discard(c)::xs => helper(cs, xs, remove_card(hs,c,IllegalMove) , sum - card_value(c) , goal,aces)
		  | Draw::xs => case cs of 
				    [] => score(hs,goal)
				  | (suit,Ace) :: cs0 => helper(cs0, xs, (suit,Ace)::hs, sum+ card_value(suit,Ace), goal, aces+1)
				  | c :: cs0 => helper(cs0, xs , c::hs, sum + card_value(c) , goal, aces)
    in
	helper(cs,ms,[],0,goal,0)
    end


(* not completed yet !*)
fun careful_player(cs,goal) = 
    let fun helper(cs,goal,hs,sum,acc) =
	    case cs of 
		[] => acc  
	     |  x::xs => if goal - sum >10 orelse sum+ card_value x <= goal then
			    helper(xs , goal , x::hs, sum + card_value x, Draw::acc)
			 else
			    case hs of 
				[] => acc
			     | c::cs => helper(xs,goal,cs, sum - card_value(c) , Discard(c)::acc)
    in
	helper(cs,goal,[],0,[])
    end
   
