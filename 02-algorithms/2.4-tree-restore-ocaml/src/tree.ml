type tree = Empty | Node of string * tree * tree

exception Bad of string

let fail msg =
  prerr_endline ("Ошибка: " ^ msg);
  exit 1

let usage () =
  prerr_endline "Использование: ocaml src/tree.ml pre-in|in-post < файл";
  prerr_endline "  в файле две строки: два обхода, ключи через пробел";
  exit 1

let rec index x = function
  | [] -> raise (Bad "обходы несогласованы")
  | y :: ys -> if x = y then 0 else 1 + index x ys

let rec take n l =
  if n = 0 then [] else match l with [] -> [] | x :: xs -> x :: take (n - 1) xs

let rec drop n l =
  if n = 0 then l else match l with [] -> [] | _ :: xs -> drop (n - 1) xs

let rec last = function
  | [] -> raise (Bad "пустой обход")
  | [x] -> x
  | _ :: xs -> last xs

let same a b = List.sort compare a = List.sort compare b

let rec build_pre_in pre ino =
  match pre with
  | [] -> Empty
  | root :: rest ->
    let i = index root ino in
    let left_in = take i ino and right_in = drop (i + 1) ino in
    let left_pre = take i rest and right_pre = drop i rest in
    if not (same left_pre left_in) then raise (Bad "обходы несогласованы");
    Node (root, build_pre_in left_pre left_in, build_pre_in right_pre right_in)

let rec build_in_post ino post =
  match post with
  | [] -> Empty
  | _ ->
    let root = last post in
    let rest = take (List.length post - 1) post in
    let i = index root ino in
    let left_in = take i ino and right_in = drop (i + 1) ino in
    let left_post = take i rest and right_post = drop i rest in
    if not (same left_post left_in) then raise (Bad "обходы несогласованы");
    Node (root, build_in_post left_in left_post, build_in_post right_in right_post)

let rec preorder = function
  | Empty -> []
  | Node (v, l, r) -> (v :: preorder l) @ preorder r

let rec inorder = function
  | Empty -> []
  | Node (v, l, r) -> inorder l @ (v :: inorder r)

let rec postorder = function
  | Empty -> []
  | Node (v, l, r) -> postorder l @ postorder r @ [v]

let levels t =
  let q = Queue.create () in
  Queue.add t q;
  let result = ref [] in
  while not (Queue.is_empty q) do
    match Queue.pop q with
    | Empty -> ()
    | Node (v, l, r) ->
      result := v :: !result;
      Queue.add l q;
      Queue.add r q
  done;
  List.rev !result

let rec height = function
  | Empty -> 0
  | Node (_, l, r) -> 1 + max (height l) (height r)

let rec print_tree depth = function
  | Empty -> ()
  | Node (v, l, r) ->
    print_tree (depth + 1) r;
    print_endline (String.make (4 * depth) ' ' ^ v);
    print_tree (depth + 1) l

let rec has_dup = function
  | [] -> false
  | x :: xs -> List.mem x xs || has_dup xs

let read_keys () =
  match In_channel.input_line stdin with
  | None -> raise (Bad "нужно две строки с обходами")
  | Some line ->
    String.split_on_char ' ' (String.trim line)
    |> List.concat_map (String.split_on_char '\t')
    |> List.filter (fun s -> s <> "")

let show l = String.concat " " l

let () =
  let mode = if Array.length Sys.argv = 2 then Sys.argv.(1) else "" in
  if mode <> "pre-in" && mode <> "in-post" then usage ();
  try
    let a = read_keys () in
    let b = read_keys () in
    if a = [] then raise (Bad "обходы пусты");
    if List.length a <> List.length b then raise (Bad "обходы разной длины");
    if has_dup a || has_dup b then raise (Bad "ключи должны быть уникальны");
    if not (same a b) then raise (Bad "обходы содержат разные элементы");
    let t = if mode = "pre-in" then build_pre_in a b else build_in_post a b in
    print_endline "дерево:";
    print_tree 0 t;
    print_newline ();
    print_endline ("прямой:       " ^ show (preorder t));
    print_endline ("симметричный: " ^ show (inorder t));
    print_endline ("обратный:     " ^ show (postorder t));
    print_endline ("в ширину:     " ^ show (levels t));
    Printf.printf "узлов: %d, высота: %d\n" (List.length a) (height t);
    let ok =
      if mode = "pre-in" then preorder t = a && inorder t = b
      else inorder t = a && postorder t = b
    in
    print_endline ("обходы совпадают с исходными: " ^ if ok then "да" else "нет")
  with Bad msg -> fail msg
