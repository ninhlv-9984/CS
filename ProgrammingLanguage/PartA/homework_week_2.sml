(* Excercise 1*)
fun is_older(date_1: int*int*int, date_2: int*int*int) =
  let
    val year_1 = (#1 date_1)
    val month_1 = (#2 date_1)
    val day_1 = (#3 date_1)
    val year_2 = (#1 date_2)
    val month_2 = (#2 date_2)
    val day_2 = (#3 date_2)
  in
    if year_1 < year_2
    then true
    else if year_1 > year_2
    then false
    else
      if month_1 < month_2
      then true
      else if month_1 > month_2
      then false
      else
          if day_1 < day_2
          then true
          else false
  end

(* Excercise 2*)
fun number_in_month(xs: (int*int*int) list, month: int) =
  if null xs
  then 0
  else
    let
      val date = hd xs
      val date_in_month = if (#2 date) = month then 1 else 0
    in
      date_in_month + number_in_month((tl xs), month)
    end

(* Excercise 3*)
fun number_in_months(xs: (int*int*int) list, months: int list) =
  if null months
  then 0
  else
    let
      val month = hd months
      val date_in_months = number_in_month(xs, month)
    in
      date_in_months + number_in_months(xs, (tl months))
    end

(* Excercise 4*)
fun dates_in_month(xs: (int*int*int) list, month: int) =
  if null xs
  then []
  else
    let
      val date = hd xs
    in
      if (#2 date) = month
      then date :: dates_in_month((tl xs), month)
      else dates_in_month((tl xs), month)
    end

(* Excercise 5*)
fun dates_in_months(xs: (int*int*int) list, months: int list) =
  if null months
  then []
  else
    dates_in_month(xs, (hd months)) @ dates_in_months(xs, (tl months))

(* Excercise 6*)
fun get_nth(xs: string list, i: int) =
  if i = 1
  then hd xs
  else
    get_nth((tl xs), i - 1)

(* Excercise 7*)
fun date_to_string(xs: (int*int*int)) =
  let
    val monthStringList = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    get_nth(monthStringList, (#2 xs)) ^ " "  ^ Int.toString((#3 xs)) ^ "," ^ " " ^ Int.toString((#1 xs))
  end

(* Excercise 8*)
fun number_before_reaching_sum(sum: int, xs: int list) =
  if null xs
  then 0
  else
    let
      val num = hd xs
    in
      if sum - num > 0
      then 1 + number_before_reaching_sum(sum - num, (tl xs))
      else 0
    end

(* Excercise 9*)
fun what_month(date: int) =
  let
    val monthDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    1 + number_before_reaching_sum(date, monthDays)
  end

(* Excercise 10*)
fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else
    what_month(day1)::month_range(day1 + 1, day2)

(* Excercise 11*)
fun oldest(xs: (int*int*int) list) =
  if null xs
  then NONE
  else
    let
      fun oldest_one_empty(xs: (int*int*int) list) =
        if null (tl xs)
        then hd xs
        else
          let
            val tl_ans = oldest_one_empty(tl xs)
          in
            if is_older((hd xs), tl_ans)
            then hd xs
            else tl_ans
          end
    in
      SOME(oldest_one_empty xs)
    end
