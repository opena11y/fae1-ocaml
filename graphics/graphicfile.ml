open Jpeg
open Gif
open Png

type filetype =
    Jpeg
  | Gif
  | Png
  | Unknown;;

type graphic_file = {
  mutable name : string;
  mutable dimensions : int * int;
  mutable colors : int;
};;

let create n d c =
  {name = n; dimensions = d; colors = c};;

let get_graphic_file_name g = g.name;;
let get_graphic_file_dimensions g = g.dimensions;;
let get_graphic_file_colors g = g.colors;;

let get_filetype_as_string ft =
  match ft with
      Jpeg -> "JPEG"
    | Gif -> "GIF"
    | Png -> "PNG"
    | Unknown -> "Unknown";;

(* sigs takes the form filetype * byte list *)
let rec match_channel_to_lists ch sigs =
  try
    if sigs = []
    then Unknown
    else (
      let next_byte = input_byte ch in
      let f a b =
        let (ft, f_sig) = b in
          if (List.hd f_sig) = next_byte || (List.hd f_sig) = -1
          then a@[(ft,List.tl f_sig)]
          else a
      in
      let new_sigs = List.fold_left f [] sigs in
      let g a b =
        match b with
            (ft,[]) -> ft
          | _ -> a
      in
        match List.fold_left g Unknown new_sigs with
            Unknown -> match_channel_to_lists ch new_sigs
          | ft -> ft
    )
  with End_of_file -> (print_string "End of file reached."; Unknown);;

let get_file_type ch =
  match_channel_to_lists ch [(Jpeg,Jpeg.file_signature ());
                             (Gif,Gif.file_signature ());
                             (Png,Png.file_signature ())];;

let open_graphic_file filename =
  try
    let ch = open_in_bin filename in
    let file_type = get_file_type ch in
      seek_in ch 0;
      (file_type, ch)
  with Sys_error e -> (
    print_string ("Unable to open file.");
    (Unknown, stdin)
  );;

let get_dimensions ft ch =
  match ft with
      Jpeg -> Jpeg.get_dimensions ch
    | Gif -> Gif.get_dimensions ch
    | Png -> Png.get_dimensions ch
    | _ -> (0,0);;

let create_from_file name filename =
  try
    let (ft, ch) = open_graphic_file filename in
    let d = get_dimensions ft ch in
      create name d 0
  with _ -> (create "" (0,0) 0);;
