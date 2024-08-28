
type t = {
  title : string;
  authors : string list;
  summary : string;
  published : Time_float_unix.t;
  pdf_link : Uri.t option;
}

val parse : string -> t list

val show : t -> string
