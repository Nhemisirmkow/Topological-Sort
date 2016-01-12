(* Autor: Marcin Michorzewski      *)
(* Zadanie: Sortowanie topologiczne*)
(* Code review : Szymon Pajzert    *)

open Array
open List
open PMap

exception Cykliczne

type opt = Visited | Computed

(* Funkcja create_map l - dla danej listy l tworzy mape *)
(* reprezentujaca graf, klucz - wierzchołek, wartosc to *)
(* lista synow                                          *)

let create_map l =
  let rec pomoc l m =
    match l with
    | [] -> m
    | h::t -> match h with
               | (a, b) -> pomoc t (add a b m)
  in
    pomoc l PMap.empty

let topol l =
  let visited = ref PMap.empty                (* mapa odwiedzin *)
  and anwser = ref []                         (* wynik *)
  and graph = ref (create_map l) in           (* mapa - reprezentacja grafu *)
  (* funkcja pomocnicza dfs v - dla wierzchołka v sprawdza, czy nie ma petli*)
  (* jezeli nie, to go przetwarzamy( jezeli nie byl wczesniej przetworzony) *)
  (* w wyniku najpierw dopiszemy wszystkich jego synow, a nastepnie v       *)
  let rec dfs v =
    if (mem v !visited && find v !visited = Visited) then raise Cykliczne
    else if not (mem v !visited) then
    (
      visited := add v Visited !visited;
      if mem v !graph then                (* przed dodaniem v do wyniku - *)
        List.iter dfs (find v !graph);    (* przetwarzamy synow v         *)
      anwser := v::(!anwser);
      visited := add v Computed !visited; (* zaznaczamy, że v - przetworzony *)
      )
  in
  (
    List.iter (function (v, _) -> dfs v) l;
    !anwser;
    )
