val months = [31,28,31,30,31,30,31,31,30,31,30,31];

fun count_day(x : int,l: int list)=
  if x=1
  then hd l
  else hd l + count_day(x-1,tl(l))

fun is_order(datex : int*int*int, datey : int*int*int) =
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
 
 
