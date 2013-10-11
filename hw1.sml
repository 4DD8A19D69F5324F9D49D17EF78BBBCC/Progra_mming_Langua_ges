fun is_older(x : int*int*int ,y : int*int*int) =
    if (#1 x) <> (#1 y) then
	(#1 x) < (#1 y) 
    else
	if (#2 x) <> (#2 y) then
	    (#2 x) < (#2 y)
	else
	    (#3 x) < (#3 y)


fun number_in_month(x : (int*int*int) list, y: int) =
    if null x then 0 
    else
	if (#2 (hd x) ) = y then 1+number_in_month(tl x,y)
	else number_in_month(tl x,y)

fun number_in_months(x : (int*int*int) list, y: int list) =
    if null y then 0 
    else
	number_in_month(x,hd y) + number_in_months(x,tl y)

fun dates_in_month(x : (int*int*int) list, y: int) =
    if null x then [] 
    else
	if (#2 (hd x)) = y then (hd x) :: dates_in_month(tl x,y)
	else dates_in_month(tl x,y)

fun dates_in_months(x : (int*int*int) list, y: int list) = 
    if null y then []
    else
	dates_in_month(x, hd y) @ dates_in_months(x,tl y)

fun get_nth(xs : string list, n: int) =
    if n = 1 then hd xs else get_nth(tl xs,n-1)

fun date_to_string( date: int*int*int) =
    let val monthstr = ["January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(monthstr, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date) 
    end

fun number_before_reaching_sum( sum: int , xs : int list) = 
    if sum <=0 orelse null xs then ~1 else 
    1 + number_before_reaching_sum( sum - (hd xs) , tl xs)

fun what_month( day : int) = 
    let val days_of_month = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] in
	1 + number_before_reaching_sum(day, days_of_month)
    end

fun month_range( day1 : int, day2 :int) = 
    if day1 > day2 then [] else
    what_month(day1) :: month_range(day1+1, day2)

fun oldest( dates : (int*int*int) list ) =
    if null dates then NONE
    else
	let val tl_ans = oldest( tl dates) in
	    if isSome tl_ans andalso is_older(valOf tl_ans,hd dates) 
	    then tl_ans else SOME(hd dates)
	end

fun remove_duplicate(orig : int list) = 
    let
	fun helper( xs : int list, acc : int) = 
	    if null xs then [] else
	    let fun is_appeared(xs :int list, n: int, x: int) =
		    if n=0 orelse null xs then false else
		    if (hd xs) = x then true else is_appeared(tl xs,n-1,x)
	    in
		if is_appeared(orig,acc,hd xs) then helper(tl xs,acc+1)
		else (hd xs) :: helper(tl xs,acc+1)
	    end
    in
	helper(orig,0)
    end

fun number_in_months_challenge(x: (int*int*int) list, y: int list) =
    number_in_months(x,remove_duplicate(y))

fun dates_in_months_challenge(x: (int*int*int) list, y: int list) =
    dates_in_months(x, remove_duplicate(y))

fun reasonable_date(date : (int*int*int)) =
    let fun is_leap(year : int) =
	    if year mod 4 = 0 
	    then
		if year mod 100 = 0 
		then
		    if year mod 400 = 0 then true else false
		else true
	    else false
    in
	if (#1 date) >0 andalso (#2 date) >=1 andalso (#2 date) <=12 andalso (#3 date) >=1 
	then
	    let val days_of_month = [0 , 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] in
		if is_leap(#1 date) andalso (#2 date) =2 then (#3 date)<=List.nth(days_of_month,(#2 date))+1
		else (#3 date)<= List.nth(days_of_month,(#2 date))
	    end
	else false
    end
