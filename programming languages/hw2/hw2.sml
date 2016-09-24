(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s,sl) =
  case sl of
      [] => NONE
    | x::xs' => (
        if same_string(x,s)
        then SOME xs'
        else case all_except_option(s,xs') of
                 NONE => NONE
              | SOME l => SOME(x::l)
    )
fun get_substitutions1 (sll,st) =
  case sll of
      [] => []
    | s::st' => ( case all_except_option(st,s) of
                      NONE => get_substitutions1(st',st)
                    | SOME sl => sl @ get_substitutions1(st',st)
                )
fun get_substitutions2 (sll,st) =
  let fun gsub (sll,st,ans) =
        case sll of
            [] => ans
          | s::st' => (case all_except_option(st,s) of
                           NONE => gsub(st',st,ans)
                        | SOME sl => gsub(st',st,ans @ sl)
                      )
  in
      gsub(sll,st,[])
  end
fun similar_names (sll,{first=fir,middle=mid,last=las}) =
  let
      val ans = {first = fir,middle = mid,last = las}
      fun add_ans (sl) =
        case sl of
            [] => []
          | s'::sl' => {first = s',middle = mid,last = las}::add_ans(sl')
      val tlist = get_substitutions2(sll,fir)
  in
      ans::add_ans(tlist)
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

fun card_color (c) =
  case c of
      (Clubs,_) => Black
    | (Spades,_) => Black
    | (Diamonds,_) => Red
    | (Hearts,_) => Red

fun card_value (c) =
  case c of
      (_,Num i) => i
   | (_,Ace) => 11
   | (_,_) => 10

fun remove_card (cs,c,e) =
  let
      fun rmc (cs,c,ans) =
        case cs of
            [] => raise e
          | c'::cs' => (if c'=c
                        then ans @ cs'
                        else rmc(cs',c,ans @ [c']))
  in
      rmc (cs,c,[])
  end

fun all_same_color (cs) =
  case cs of
      (c1::(c2::cs')) => (if(card_color(c1)=card_color(c2))
                            then all_same_color(c2::cs')
                            else false)
    | _ => true

fun sum_cards (cs) =
  let
      fun sum_c(cs,ans) =
        case cs of
            [] => ans
          | c'::cs' => sum_c(cs',card_value(c') + ans)
  in
      sum_c(cs,0)
  end

fun score (cs,goal) =
  let
      val ps = sum_cards(cs)
      val dif = goal - ps
      val fs = (if dif >0 then dif else 3*(~dif))
  in
      if all_same_color(cs)
      then fs div 2
      else fs
  end

fun officiate (cs,ml,goal) =
  let
      fun off(mycards,cs,ml) =
        case ml of
            [] => score(mycards,goal)
          | m'::ml' => (case m' of
                            Draw => (case cs of
                                         c'::[] => score(c'::mycards,goal)
                                      | c'::cs' => off(c'::mycards,cs',ml')
                                    )
                         | Discard ci => off(remove_card(cs,ci,IllegalMove),cs,ml')
                       )
  in
      off([],cs,ml)
  end

fun score_challenge (cs,goal) =
  let
      fun new_sum (cs,ans) =
        case cs of
            [] => (if ans>goal
                   then 3*(ans-goal)
                   else goal-ans
                  )
          | (_,Ace)::cs' => (let
                                val xs = new_sum(cs',ans+1)
                                val ys = new_sum(cs',ans+11)
                            in
                                if(xs<ys)
                                then xs
                                else ys
                            end)
          | c'::cs' => new_sum(cs',ans+card_value(c'))
      val fs = new_sum(cs,0)
  in
      if all_same_color(cs)
      then fs div 2
      else fs
  end

fun officiate_challenge (cs,ml,goal) =
  let
      fun off(mycards,cs,ml) =
        case ml of
            [] => score_challenge(mycards,goal)
          | m'::ml' => (case m' of
                            Draw => (case cs of
                                         c'::[] => score_challenge(c'::mycards,goal)
                                       | c'::cs' => off(c'::mycards,cs',ml')
                                    )
                          | Discard ci => off(remove_card(cs,ci,IllegalMove),cs,ml')
                       )
  in
      off([],cs,ml)
  end

fun careful_player (cs,goal) =
  let
      fun find_card (cs,c) =
        case cs of
            [] => NONE
          | c'::cs' => if(card_value(c')=c) then SOME c' else find_card(cs',c)
      fun cp (mycards,cs,ml,scores) =
        case cs of
            c'::cs' => (if scores+card_value(c')<goal
                        then cp(c'::mycards,cs',ml @ [Draw],scores+card_value(c'))
                        else (if scores+card_value(c')=goal
                              then ml @ [Draw]
                              else (let
                                       val cc = find_card(mycards,card_value(c')-(goal-scores))
                                   in
                                       case cc of
                                           NONE => ml
                                        | SOME c'' => ml @ [Discard c'',Draw]
                                   end)
                             )
                       )
          | [] => ml
  in
      cp([],cs,[],0)
  end
