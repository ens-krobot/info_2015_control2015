
let filter_map f l =
  let rec loop f l acc =
    match l with
    | [] -> List.rev acc
    | h :: t ->
      match f h with
      | None -> loop f t acc
      | Some h -> loop f t (h :: acc)
  in
  loop f l []
