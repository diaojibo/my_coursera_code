(* Coursera Programming Languages, Homework 3, Provided Code *)

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
fun start_upper st =
  Char.isUpper(String.sub (st,0))

fun only_capitals xs = List.filter start_upper xs

fun replacel (sx,sy) =
  if String.size sx <= String.size sy
  then sy
  else sx

fun longest_string1 xs =
  foldl replacel "" xs
fun longest_string2 xs =
  foldl (fn (sx,sy) =>
           if String.size sx < String.size sy
           then sy
           else sx ) "" xs

fun longest_string_helper f xs =
  let
      fun hreplace (sx,sy) =
        if f (String.size sx,String.size sy)
        then sx
        else sy
  in
      foldl hreplace "" xs
  end

val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode
