open Core
open Syndic

type t = {
  title : string;
  authors : string list;
  summary : string;
  published : Time_float_unix.t;
  pdf_link : Uri.t option;
}
[@@deriving show]

let string_of_text_construct : Atom.text_construct -> string = function
  | Atom.Text s -> s
  | _ -> assert false

let entry_date (e : Atom.entry) =
  Option.value_map ~default:e.updated ~f:(fun x -> x) e.published

let make_time d =
  let convert_month m =
    match m with
    | Date.Jan -> Month.Jan
    | Date.Feb -> Month.Feb
    | Date.Mar -> Month.Mar
    | Date.Apr -> Month.Apr
    | Date.May -> Month.May
    | Date.Jun -> Month.Jun
    | Date.Jul -> Month.Jul
    | Date.Aug -> Month.Aug
    | Date.Sep -> Month.Sep
    | Date.Oct -> Month.Oct
    | Date.Nov -> Month.Nov
    | Date.Dec -> Month.Dec
  in

  let date =
    Core.Date.create_exn ~y:(Date.year d)
      ~m:(convert_month (Date.month d))
      ~d:(Date.day d)
  in
  let ofday =
    Time_float_unix.Ofday.create ~hr:(Date.hour d) ~min:(Date.minute d)
      ~sec:(Float.to_int (Date.second d)) ()
  in
  Time_float_unix.of_date_ofday ~zone:Time_float_unix.Zone.utc date ofday

let make_entry (e : Atom.entry) =
  let open Atom in
  let get_pdf_uri links =
    let uris = List.map links ~f:(fun l -> l.href) in
    List.find uris ~f:(fun uri ->   String.is_substring (Uri.to_string uri) ~substring:"pdf")
  in
  let title = string_of_text_construct e.title in
  let main_author, secondary_authors = e.authors in
  let authors =
    List.map (main_author :: secondary_authors) ~f:(fun a -> a.name)
  in
  let summary =
    Option.value_map ~default:"N/A"
      ~f:(fun s -> string_of_text_construct s)
      e.summary
  in
  let published = make_time (entry_date e) in
  let link = get_pdf_uri e.links in
  { title; authors; summary; published; pdf_link = link }

let parse str = 
  let feed = Atom.parse (Xmlm.make_input (`String (0, str))) in
  List.map feed.entries ~f:make_entry
