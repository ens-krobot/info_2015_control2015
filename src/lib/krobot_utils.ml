
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

let array_rev a =
  Array.mapi (fun i _ -> a.(Array.length a - i - 1)) a

type urg_filters =
  { urg_up_filter_file : bool array;
    urg_down_filter_file : bool array }

let read_urg_filter_file ~filename : urg_filters option =
  if not (Sys.file_exists filename) then begin
    Printf.printf "can't find urg filter file %s\n%!" filename;
    None
  end
  else begin
    let ic = open_in filename in
    let lines = ref [] in
    let lines_curr = ref [] in
    begin try while true do
          match input_char ic with
          | 't' -> lines_curr := true :: !lines_curr
          | 'f' -> lines_curr := false :: !lines_curr
          | '\n' -> lines := (Array.of_list (List.rev !lines_curr)) :: !lines;
          | _ -> failwith (Printf.sprintf "malformed urg filter file %s" filename);
        done
      with End_of_file -> ()
    end;
    match !lines with
    | [] -> None
    | [urg_up_filter_file; urg_down_filter_file] -> Some { urg_down_filter_file; urg_up_filter_file }
    | _ :: _ -> None
  end
