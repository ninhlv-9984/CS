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
