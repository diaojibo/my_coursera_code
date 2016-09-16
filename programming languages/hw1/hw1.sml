val months = [31,28,31,30,31,30,31,31,30,31,30,31];
val months_name = ["January","February","March","April","May","June","July","August","September","October","November","December"]
		      
fun count_day(x : int,l: int list)=
  if x=1
  then hd l
  else hd l + count_day(x-1,tl(l))

fun is_older(datex : int*int*int, datey : int*int*int) =
  let
      val x = (#1 datex)*12*365 + count_day(#2 datex,months)+(#3 datex)
      val y = (#1 datey)*12*365 + count_day(#2 datey,months)+(#3 datey)
  in
      if(x<y) then true
      else false
  end

fun number_in_month(x : (int*int*int) list,m : int) =
  if null x
  then 0
  else
      if((#2 (hd x))=m)
      then number_in_month(tl x,m)+1
      else number_in_month(tl x,m)

fun number_in_months(xl : (int*int*int) list,yl:int list) =
  if null yl
  then 0
  else number_in_month(xl,hd yl)+number_in_months(xl,tl yl)

fun dates_in_month(xl : (int*int*int) list,m : int) =
  if null xl
  then []
  else
      if (#2 (hd xl))=m
      then ((hd xl))::dates_in_month(tl xl,m)
      else dates_in_month(tl xl,m)

fun dates_in_months(xl : (int*int*int) list,yl : int list) =
  if null yl
  then []
  else
      dates_in_month(xl,hd yl)@dates_in_months(xl,tl yl)

fun get_nth(xl : string list,y : int) =
  if y=1
  then hd xl
  else get_nth(tl xl,y-1)

	      
fun date_to_string(datex : int*int*int) =
  get_nth(months_name,#2 datex)^" "^Int.toString(#3 datex)^", "^Int.toString(#1 datex)
									    
fun number_before_reaching_sum(sum : int,xl : int list) =
  if(sum <= hd(xl))
  then 0
  else 1+number_before_reaching_sum(sum-hd(xl),tl xl)

fun what_month(day : int) =
  number_before_reaching_sum(day,months) + 1

fun get_months_range(x : int, y: int) =
  if(x=y) then x::[]
  else x::get_months_range(x+1,y)
			  
fun month_range(day1 : int , day2 : int) =
  if(day1=day2) then what_month(day1)::[]
  else if(day1>day2)then month_range(day2,day1)
  else what_month(day1)::month_range(day1+1,day2)

fun oldest(xl : (int*int*int) list) =
  if null(xl) then NONE
  else
      let
	  val max_ans = oldest(tl xl)
      in
	  if(isSome(max_ans) andalso is_older(valOf(max_ans),hd xl))
	  then max_ans
	  else SOME(hd(xl))
      end
