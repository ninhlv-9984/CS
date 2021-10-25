(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

(* 1a *)
fun all_except_option(s: string, lst: string list) =
  let
    fun list_except_string(_, []) = NONE
      | list_except_string(prev_list, x::xs) =
          if same_string(s, x)
          then SOME(prev_list @ xs)
          else
            list_except_string(prev_list @ [x], xs)
  in
    list_except_string([], lst)
  end

(* 1b *)
fun get_substitutions1([], _) = [] |
    get_substitutions1(x::xs, s) =
      case all_except_option(s, x) of
          NONE => get_substitutions1(xs, s)
        | SOME i => i @ get_substitutions1(xs, s)

(* 1c *)
fun get_substitutions2(lsts, s) =
  let fun get_substitutions(lsts, s, acc) =
         case lsts of
           [] => acc
         | x::xs => case all_except_option(s, x) of
                         NONE => get_substitutions(xs, s, acc)
                        | SOME i => get_substitutions(xs, s, acc @ i)
  in
    get_substitutions(lsts, s, [])
  end
