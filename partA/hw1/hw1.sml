fun is_older (x : int*int*int, y : int*int*int) = 
  if #1 x = #1 y
  then (if #2 x = #2 y
        then #3 x < #3 y
        else #2 x < #2 y)
  else #1 x < #1 y

fun number_in_month (x : (int*int*int)list, y : int) =
  let
    fun number_in_monthh(x : (int*int*int)list) = 
      if null x
      then 0
      else if #2(hd x) <> y
      then 0 + number_in_monthh(tl x)
      else 1 + number_in_monthh(tl x)
  in
    number_in_monthh(x)
  end

fun number_in_months (x : (int*int*int)list, y : int list) = 
  let fun months(y : int list) =
        if null y
        then 0
        else number_in_month(x, hd y) + months(tl y)
  in 
    months(y)
  end

fun dates_in_month (x : (int*int*int)list, y : int) =
  let fun date(x : (int*int*int)list) =
    if null x
    then []
    else if #2 (hd x) <> y
    then date(tl x)
    else hd x :: date(tl x)
  in
    date(x)
  end

fun dates_in_months (x : (int*int*int)list, y : int list) =
  let fun dates(y : int list) =
    if null y
    then []
    else dates_in_month(x, hd y) @ dates(tl y)
  in
    dates(y)
  end

fun get_nth (x : string list, y : int) =
  if y = 1
  then hd x
  else get_nth(tl x, y - 1)

fun date_to_string(x : int*int*int) =
  let val y = get_nth(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], #2 x)
  in
    y ^ " " ^ Int.toString(#3 x) ^ ", " ^ Int.toString(#1 x)
  end

fun number_before_reaching_sum(sum : int, x : int list) =
  if hd x < sum
  then 1 + number_before_reaching_sum(sum - hd x, tl x)
  else 0

fun what_month(sum : int) =
  let
    val months=[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(sum, months)+1
  end


fun month_range (from : int, to : int) =
  let
    fun range (from : int) =
      if from > to
      then []
      else what_month(from) :: range(from + 1)
  in
    range(from)
  end

fun oldest (x : (int*int*int)list) = 
  if null x
  then NONE
  else
    let
      fun oldestt(x : (int*int*int)list) =
        if null (tl x)
        then hd x
        else 
          let val tl_ans = oldestt(tl x)
          in
            if is_older(hd x, tl_ans)
            then hd x
            else tl_ans
          end
    in
      SOME(oldestt(x))
    end 
