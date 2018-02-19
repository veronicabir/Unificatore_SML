datatype termine =
	Var of string |
	Fun of string* (termine list);

datatype equazione =
	Eq of (termine*termine)|
	Neq of (termine*termine);

fun termineToString (Var(v)) = "Var("^v^")" |
	termineToString (Fun(n,lista)) = n^"("^(listaToString lista)^")"
and listaToString [] = "" |
	listaToString (h::t) = (termineToString h) ^ (listaToString t);

fun equazioneToString (Eq(terminel,terminer)) = "Eq(" ^ (termineToString terminel) ^ "=" ^ (termineToString terminer) ^ ")" |
	equazioneToString (Neq(terminel, terminer)) = "Neq(" ^ (termineToString terminel) ^ "=" ^ (termineToString terminer) ^ ")";

fun listaEquazioniToString [] = ""
   | listaEquazioniToString (h :: t) = equazioneToString h ^"," ^ listaEquazioniToString t;

exception noUnificatore of string;

fun term_reduction(l1: (termine list), l2: (termine list)): equazione list =
	map (fn x => Eq(x)) (ListPair.zipEq (l1, l2));

fun presente (v: string, tl: termine):bool =
	case tl of
		Var(u) => if u=v then true else false |
		Fun(s,tl) => List.exists( fn x=> presente(v,x) ) tl;

fun  sostituisci (nomeIncognita:string, sostituzione: termine, t:termine): termine =
	case t of 
		Fun(s,tl) => Fun(s, map (fn x=> sostituisci (nomeIncognita,sostituzione,x) ) tl)|
		Var(v) => if v=nomeIncognita then sostituzione else t;
		

fun sostituisci_init (nomeincognita:string, sostituzione: termine, e: equazione): equazione =
	case e of
		Neq(l,r) => Neq( sostituisci(nomeincognita,sostituzione,l) , sostituisci(nomeincognita,sostituzione,r))|
		Eq(l,r) => Eq(sostituisci(nomeincognita,sostituzione,l) , sostituisci(nomeincognita,sostituzione,r));

fun unifica_elem (e: equazione, s1: equazione list):equazione list =
	case e of
		Neq(t) => [Neq(t)] @ s1|
		Eq(Var(v),Var(u)) => if v=u then s1 else (map (fn x => sostituisci_init(v,Var(u),x)) s1) @ [Neq(Var(v),Var(u))]|
		Eq(Var(v),Fun(s,tl)) => if (presente (v, Fun(s,tl))) then raise noUnificatore("No unificatore")
								else (map (fn x => sostituisci_init(v,Fun(s,tl),x)) s1) @ [Neq(Var(v),Fun(s,tl))]|
		Eq(Fun(s3,l3),Fun(s2,l2)) => if (s3=s2 andalso length l3 = length l2 ) 
			then  (if (not (null l3)) then (term_reduction(l3,l2) @ s1) else [Eq(Fun(s3,l3),Fun(s2,l2))]@s1 ) else raise noUnificatore("No unificatore")|
		Eq(Fun(s,tl),Var(v)) => [Eq(Var(v),Fun(s,tl))] @ s1;

fun unifica(s: equazione list): equazione list =
	let 
		val s1 = unifica_elem (hd s, tl s)
	in 
		if (s1 = s) then s1 else (unifica s1)
	end;


val grande =  unifica [ Eq (  Fun("f",[Var("x1"),Fun("h",[Var("x1")]),Var("x2")]),   
					 Fun("f",[Fun("g",[Var("x3")]),Var("x4"),Var("x3")])   )] ;
print (listaEquazioniToString grande);

val grandegrande = unifica [ Eq( Fun("g", [Var("x2")]), Var("x1")),     Eq( Fun("f", [Var("x1"), Fun("h",[Var("x1")]), Var("x2")]),  Fun("f", [Fun("g", [Var("x3")]), Var("x4"), Var("x3")]))];

print (listaEquazioniToString grandegrande);
(*val gu1 = unifica_elem (hd grande, tl grande);
val gu2 = unifica_elem (hd gu1, tl gu1);
val gu3 = unifica_elem (hd gu2, tl gu2);*)

(*val piccolo = unifica [Eq(Var("x1"),Var("x2"))];*)