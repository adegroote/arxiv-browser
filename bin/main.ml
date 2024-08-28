open Core
open Async
open Cohttp_async

let fetch_arxiv_data category =
  let uri =
    Uri.of_string
      (Printf.sprintf
         "http://export.arxiv.org/api/query?search_query=cat:%s&sortBy=submittedDate"
         category)
  in
  Client.get uri >>= fun (_, body) ->
  Body.to_string body >>| fun body_str ->
  Parse.parse body_str

let print_entry e = Format.printf "%s\n" (Parse.show e)

let main category = fetch_arxiv_data category >>| fun entries -> List.iter entries ~f:print_entry

let command =
  Command.async_spec ~summary:"Fetch recent Arxiv submissions"
    Command.Spec.(
      empty
      +> flag "-c"
           (optional_with_default "cs.AI" string)
           ~doc:"CATEGORY Arxiv category to browse (default: cs.AI)")
    (fun category () -> main category)

let () = Command_unix.run command
