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

fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x'::xs' => (case f x' of
                      NONE => first_answer f xs'
                  | SOME x'' => x''
                )
fun all_answers f xs =
  let
      fun myappend (ax,ay) =
        case ax of
            NONE => SOME ay
          | SOME ax' => SOME (ax' @ ay)
      fun ahelper (ac,xs) =
        case xs of
            [] => ac
          | x'::xs' => (case f x' of
                            NONE => ahelper(ac,xs')
                         | SOME x'' => ahelper(myappend (ac,x''),xs')
                       )
  in
      case xs of
          [] => SOME []
        | _ => ahelper (NONE,xs)
  end

fun g f1 f2 p =
  let
      val r = g f1 f2
  in
      case p of
          Wildcard => f1 p
       | Variable s => f2 s
       | TupleP tp => foldl (fn (p,i) => (r p) + i ) 0 tp
       | ConstructorP(_,p) => r p
       | _ => 0
  end

val count_wildcards = g (fn x => 1)(fn x => 0)
val count_wild_and_variable_lengths  = g (fn x => 1)(fn x => String.size x)
fun count_some_var (s,p) =
  g (fn x => 0) (fn x => if (x = s) then 1 else 0) p

fun check_pat p =
  let
      fun helper p =
        case p of
            Variable s => [s]
          | TupleP ps => foldl (fn (p,i) => i @ helper p) [] ps
          | _ => []
      val stl = helper p
      fun isdup xs =
        case xs of
            [] => false
          | x'::xs' => (List.exists(fn s => if x'=s then true else false) xs') orelse isdup xs'
  in
      if (isdup stl) then false else true
  end

fun match (vu,p) =
  case (vu,p) of
      (_,Wildcard) => SOME []
    | (_,Variable s) => SOME [(s,vu)]
    | (Unit,UnitP) => SOME []
    | (Const x,ConstP y) => if x=y then SOME [] else NONE
    | (Tuple vs,TupleP ps) => (if List.length vs = List.length ps
                               then (case all_answers match (ListPair.zip (vs,ps)) of
                                         NONE => NONE
                                       | SOME xl => SOME xl
                                    )
                               else NONE
                              )
    | (Constructor (s2,v),ConstructorP (s1,p)) => (if s1 = s2
                                                   then match (v,p)
                                                   else NONE)
    | _ => NONE

fun first_match vu pl = SOME (first_answer match (map (fn p => (vu,p)) pl))
                          handle NoAnswer => NONE

fun get_pattern_type (type_data,pattern) =
  let
      fun check_type_data (type_data1,datatype_str,cons_type) =
        case type_data1 of
            [] => raise NoAnswer
          | (x,y,z)::xs => if x = datatype_str andalso cons_type = z
                           then Datatype y
                           else check_type_data(xs,datatype_str,cons_type)

      fun helper ptn =
        case ptn of
            [] => []
          | x::xs => (get_pattern_type(type_data,x))::helper xs
  in
      case pattern of
          Wildcard => Anything
        | UnitP => UnitT
        | ConstP v => IntT
        | TupleP v => TupleT(helper v)
        | Variable v1 => Anything
        | ConstructorP(s,v) => check_type_data (type_data,s,get_pattern_type(type_data,v))
  end

fun get_most_lenient (typ1,typ2) =
  let
      fun helper typelst =
        case typelst of
            ([],[]) => []
          | (x::xs,y::ys) => (get_most_lenient (x,y))::helper(xs,ys)
          | _ => raise NoAnswer
  in
      case (typ1,typ2) of
          (Anything,_) => typ2
        | (Datatype s1,Datatype s2) => if s1 = s2 then typ1 else raise NoAnswer
        | (IntT,IntT) => IntT
        | (UnitT,UnitT) => UnitT
        | (_,Anything) => typ1
        | (_,_) => raise NoAnswer
  end
fun typecheck_patterns (type_data,pattern_list) =
  SOME (List.foldl (fn(x,acc) => get_most_lenient(get_pattern_type(type_data,x),acc)) Anything pattern_list)
  handle NoAnswer => NONE
