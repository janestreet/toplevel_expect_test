open Ppxlib
open Compiler_modules
open Core
open Poly
open Ppx_expect_runtime
open Mlt_parser

[%%if host_is_i386]

[@@@ocaml.alert "-deprecated"]

[%%endif]

module Clflags = Ocaml_common.Clflags
module Compmisc = Ocaml_common.Compmisc
module Printast = Ocaml_common.Printast
module Warnings = Ocaml_common.Warnings
module Unix = Caml_unix

let parse_contents ~fname contents =
  let lexbuf = Lexing.from_string contents in
  lexbuf.lex_curr_p <- { pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  Ocaml_common.Location.input_name := fname;
  Parse.use_file lexbuf
;;

let make_corrected = ref false
let suppress_diff_and_errors_for_testing = ref false
let reset_line_numbers = ref false
let reset_line_numbers_after_expect = ref false
let line_numbers_delta = ref 0
let start_of_chunk = ref None

let update_line_numbers_delta loc =
  reset_line_numbers := false;
  line_numbers_delta := 1 - loc.loc_start.pos_lnum
;;

let () =
  Stdlib.Hashtbl.add
    Toploop.directive_table
    "suppress_diff_and_errors_for_testing"
    (Directive_none (fun () -> suppress_diff_and_errors_for_testing := true))
;;

let () =
  Stdlib.Hashtbl.add
    Toploop.directive_table
    "reset_line_numbers"
    (Directive_none (fun () -> reset_line_numbers := true))
;;

let () =
  Stdlib.Hashtbl.add
    Toploop.directive_table
    "reset_line_numbers_after_expect"
    (Directive_bool
       (fun x ->
         reset_line_numbers_after_expect := x;
         if x then Option.iter !start_of_chunk ~f:update_line_numbers_delta))
;;

let print_line_numbers = ref false

let () =
  Stdlib.Hashtbl.add
    Toploop.directive_table
    "print_line_numbers"
    (Directive_bool (fun x -> print_line_numbers := x))
;;

let print_line_number ppf line =
  if !print_line_numbers
  then Format.fprintf ppf "%d" line
  else Format.pp_print_string ppf "_"
;;

let print_column_numbers = ref false

let () =
  Stdlib.Hashtbl.add
    Toploop.directive_table
    "print_column_numbers"
    (Directive_bool (fun x -> print_column_numbers := x))
;;

let print_column_number ppf column =
  if !print_line_numbers || !print_column_numbers
  then Format.fprintf ppf "%d" column
  else Format.pp_print_string ppf "_"
;;

[%%if ocaml_version < (4, 08, 0)]

let print_loc ppf (loc : Location.t) =
  let line = loc.loc_start.pos_lnum in
  let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  Format.fprintf ppf "Line %a" print_line_number line;
  if startchar >= 0
  then
    Format.fprintf
      ppf
      ", characters %a-%a"
      print_column_number
      startchar
      print_column_number
      endchar;
  Format.fprintf ppf ":@."
;;

let rec error_reporter
  ppf
  ({ loc; msg; sub; if_highlight = _ } : Ocaml_common.Location.error)
  =
  print_loc ppf loc;
  Format.fprintf ppf "Error: %s" msg;
  List.iter sub ~f:(fun err -> Format.fprintf ppf "@\n@[<2>%a@]" error_reporter err)
;;

[%%endif]
[%%if ocaml_version < (4, 06, 0)]

let warning_printer loc ppf w =
  if Warnings.is_active w
  then (
    print_loc ppf loc;
    Format.fprintf ppf "Warning %a@." Warnings.print w)
;;

[%%elif ocaml_version < (4, 08, 0)]

let warning_printer loc ppf w =
  match Warnings.report w with
  | `Inactive -> ()
  | `Active { Warnings.number; message; is_error; sub_locs = _ } ->
    print_loc ppf loc;
    if is_error
    then Format.fprintf ppf "Error (Warning %d): %s@." number message
    else Format.fprintf ppf "Warning %d: %s@." number message
;;

[%%elif ocaml_version >= (4, 08, 0)]

let warning_reporter = Ocaml_common.Location.default_warning_reporter
let alert_reporter = Ocaml_common.Location.default_alert_reporter

[%%endif]
[%%if ocaml_version >= (4, 08, 0)]

let report_printer () =
  let printer = Ocaml_common.Location.default_report_printer () in
  let print_loc _ _report ppf loc =
    let line = loc.loc_start.pos_lnum in
    let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
    let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
    Format.fprintf ppf "Line %a" print_line_number line;
    if startchar >= 0
    then
      Format.fprintf
        ppf
        ", characters %a-%a"
        print_column_number
        startchar
        print_column_number
        endchar;
    Format.fprintf ppf ":@."
  in
  { printer with
    Ocaml_common.Location.pp_main_loc = print_loc
  ; pp_submsg_loc = print_loc
  }
;;

[%%endif]

type var_and_value = V : 'a ref * 'a -> var_and_value

let protect_vars =
  let set_vars l = List.iter l ~f:(fun (V (r, v)) -> r := v) in
  fun vars ~f ->
    let backup = List.map vars ~f:(fun (V (r, _)) -> V (r, !r)) in
    set_vars vars;
    protect ~finally:(fun () -> set_vars backup) ~f
;;

[%%if ocaml_version < (4, 08, 0)]

let capture_compiler_stuff ppf ~f =
  protect_vars
    [ V (Ocaml_common.Location.formatter_for_warnings, ppf)
    ; V (Ocaml_common.Location.warning_printer, warning_printer)
    ; V (Ocaml_common.Location.error_reporter, error_reporter)
    ]
    ~f
;;

[%%else]

let capture_compiler_stuff ppf ~f =
  protect_vars
    [ V (Ocaml_common.Location.formatter_for_warnings, ppf)
    ; V (Ocaml_common.Location.warning_reporter, warning_reporter)
    ; V (Ocaml_common.Location.report_printer, report_printer)
    ; V (Ocaml_common.Location.alert_reporter, alert_reporter)
    ]
    ~f
;;

[%%endif]

let apply_rewriters = function
  | Ptop_dir _ as x -> x
  | Ptop_def s -> Ptop_def (Driver.map_structure s)
;;

let verbose = ref false

let () =
  Stdlib.Hashtbl.add
    Toploop.directive_table
    "verbose"
    (Directive_bool (fun x -> verbose := x))
;;

let shift_line_numbers =
  object
    inherit [int] Ast_traverse.map_with_context
    method! position delta pos = { pos with pos_lnum = pos.pos_lnum + delta }
  end
;;

let exec_phrase ppf phrase =
  (match phrase with
   | Ptop_def ({ pstr_loc = loc; _ } :: _) ->
     (match !start_of_chunk with
      | Some _ -> ()
      | None ->
        start_of_chunk := Some loc;
        if !reset_line_numbers_after_expect then update_line_numbers_delta loc);
     if !reset_line_numbers then update_line_numbers_delta loc
   | _ -> ());
  let phrase =
    match !line_numbers_delta with
    | 0 -> phrase
    | n -> shift_line_numbers#toplevel_phrase n phrase
  in
  let phrase = apply_rewriters phrase in
  let module Js = Ppxlib_ast.Selected_ast in
  let ocaml_phrase = Js.to_ocaml Toplevel_phrase phrase in
  if !Clflags.dump_parsetree then Printast.top_phrase ppf ocaml_phrase;
  if !Clflags.dump_source then Pprintast.top_phrase ppf phrase;
  Toploop.execute_phrase !verbose ppf ocaml_phrase
;;

let redirect ~f =
  let stdout_backup = Unix.dup Unix.stdout in
  let stderr_backup = Unix.dup Unix.stderr in
  let filename = Stdlib.Filename.temp_file "expect-test" "stdout" in
  let fd_out = Unix.openfile filename [ O_WRONLY; O_CREAT; O_TRUNC ] 0o600 in
  Unix.dup2 fd_out Unix.stdout;
  Unix.dup2 fd_out Unix.stderr;
  let ic = In_channel.create filename in
  let read_up_to = ref 0 in
  let capture buf =
    Out_channel.flush stdout;
    Out_channel.flush stderr;
    let pos = Unix.lseek fd_out 0 SEEK_CUR in
    let len = pos - !read_up_to in
    read_up_to := pos;
    Stdlib.Buffer.add_channel buf ic len
  in
  protect
    ~f:(fun () -> f ~capture)
    ~finally:(fun () ->
      In_channel.close ic;
      Unix.close fd_out;
      Unix.dup2 stdout_backup Unix.stdout;
      Unix.dup2 stderr_backup Unix.stderr;
      Unix.close stdout_backup;
      Unix.close stderr_backup;
      Stdlib.Sys.remove filename)
;;

let eval_expect_file fname ~file_contents ~capture =
  (* 4.03: Warnings.reset_fatal (); *)
  let chunks, trailing_code =
    parse_contents ~fname file_contents |> split_chunks ~fname
  in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  reset_line_numbers := false;
  reset_line_numbers_after_expect := false;
  line_numbers_delta := 0;
  start_of_chunk := None;
  let exec_phrases phrases =
    start_of_chunk := None;
    (* So that [%expect_exact] nodes look nice *)
    Buffer.add_char buf '\n';
    List.iter phrases ~f:(fun phrase ->
      let snap = Ocaml_common.Btype.snapshot () in
      match exec_phrase ppf phrase with
      | (_ : bool) -> ()
      | exception exn ->
        Location.report_exception ppf exn;
        Ocaml_common.Btype.backtrack snap);
    Format.pp_print_flush ppf ();
    let len = Buffer.length buf in
    if len > 0 && Buffer.nth buf (len - 1) <> '\n'
    then (* So that [%expect_exact] nodes look nice *)
      Buffer.add_char buf '\n';
    capture buf;
    if Buffer.nth buf (len - 1) <> '\n' then Buffer.add_char buf '\n';
    let s = Buffer.contents buf in
    Buffer.clear buf;
    s
  in
  let failure_ref = ref false in
  let results =
    capture_compiler_stuff ppf ~f:(fun () ->
      List.map chunks ~f:(fun chunk ->
        let actual = exec_phrases chunk.phrases in
        (match
           Test_node.For_mlt.record_and_return_number_of_lines_in_correction
             ~expect_node_formatting
             ~failure_ref
             ~test_output_raw:actual
             chunk.test_node
         with
         | None -> ()
         | Some correction_lines ->
           let original_lines =
             let { loc_start = { pos_lnum = start; _ }
                 ; loc_end = { pos_lnum = end_; _ }
                 ; loc_ghost = _
                 }
               =
               chunk.test_node_loc
             in
             end_ - start + 1
           in
           line_numbers_delta := !line_numbers_delta + correction_lines - original_lines);
        chunk, actual, chunk.test_node))
  in
  let trailing =
    match trailing_code with
    | None -> None
    | Some (phrases, loc_start, part) ->
      let actual, test_node =
        capture_compiler_stuff ppf ~f:(fun () ->
          let actual = exec_phrases phrases in
          let trailing_pos =
            match List.last phrases with
            | None -> loc_start
            | Some (Ptop_dir { pdir_loc; _ }) -> pdir_loc.loc_end
            | Some (Ptop_def structure) -> (List.last_exn structure).pstr_loc.loc_end
          in
          let test_node =
            Test_node.Create.expect
              { start_bol =
                  trailing_pos.pos_cnum
                  (* We let start_bol=start_pos so that the trailing tests get indented
                   flush with the left margin. *)
              ; start_pos = trailing_pos.pos_cnum
              ; end_pos = trailing_pos.pos_cnum
              }
              (Payload.default "")
          in
          let (_ : int option) =
            Test_node.For_mlt.record_and_return_number_of_lines_in_correction
              ~expect_node_formatting
              ~failure_ref
              ~test_output_raw:actual
              test_node
          in
          actual, test_node)
      in
      Some (actual, test_node, part)
  in
  not !failure_ref, results, trailing
;;

let interpret_results_for_diffing (_, results, trailing) =
  let test_nodes = List.map results ~f:(fun (_, _, test_node) -> test_node) in
  match trailing with
  | Some (_, test_node, _) ->
    Some (Test_node.For_mlt.loc test_node), test_node :: test_nodes
  | None -> None, test_nodes
;;

module T = Toplevel_expect_test_types

(* Take a part of a file, trimming spaces at the beginning as well as ';;' *)
let sub_file file_contents ~start ~stop =
  let rec loop start =
    if start >= stop
    then start
    else (
      match file_contents.[start] with
      | ' ' | '\t' | '\n' -> loop (start + 1)
      | ';' when start + 1 < stop && file_contents.[start + 1] = ';' -> loop (start + 2)
      | _ -> start)
  in
  let start = loop start in
  String.sub file_contents ~pos:start ~len:(stop - start)
;;

let generate_doc_for_sexp_output ~fname:_ ~file_contents (matched, results, trailing) =
  let rev_contents =
    List.rev_map results ~f:(fun (chunk, resp, _) ->
      let loc = chunk.phrases_loc in
      ( chunk.part
      , { T.Chunk.ocaml_code =
            sub_file
              file_contents
              ~start:loc.loc_start.pos_cnum
              ~stop:loc.loc_end.pos_cnum
        ; toplevel_response = resp
        } ))
  in
  let rev_contents =
    match trailing with
    | None -> rev_contents
    | Some (resp, test_node, part) ->
      ( part
      , { ocaml_code =
            sub_file
              file_contents
              ~start:(Test_node.For_mlt.loc test_node).start_pos
              ~stop:(String.length file_contents)
        ; toplevel_response = resp
        } )
      :: rev_contents
  in
  let parts =
    List.group (List.rev rev_contents) ~break:(fun (a, _) (b, _) -> a <> b)
    |> List.map ~f:(function chunks ->
         { T.Part.name = Option.bind (List.hd chunks) ~f:fst |> Option.value ~default:""
         ; chunks = List.map chunks ~f:snd
         })
  in
  { T.Document.parts; matched }
;;

let diff_command = ref None

let process_expect_file fname ~use_color ~in_place ~sexp_output ~use_absolute_path =
  (* Captures the working directory before running the user code, which might change it *)
  let cwd = Stdlib.Sys.getcwd () in
  let file_contents = In_channel.read_all fname in
  let result = redirect ~f:(eval_expect_file fname ~file_contents) in
  Stdlib.Sys.chdir cwd;
  if !suppress_diff_and_errors_for_testing then diff_command := Some "-";
  if sexp_output
  then (
    let doc = generate_doc_for_sexp_output ~fname ~file_contents result in
    Format.printf "%a@." Sexp.pp_hum (T.Document.sexp_of_t doc));
  let result =
    match
      interpret_results_for_diffing result
      |> Write_corrected_file.f
           ~use_color
           ~in_place
           ~diff_command:!diff_command
           ~diff_path_prefix:(Option.some_if use_absolute_path cwd)
           ~filename:fname
           ~with_:(fun ~original_file_contents (trailing_loc, xs) ->
           List.concat_map
             xs
             ~f:
               (Test_node.For_mlt.to_diffs
                  ~expect_node_formatting
                  ~cr_for_multiple_outputs:
                    Ppx_expect_runtime.For_external.default_cr_for_multiple_outputs
                  ~original_file_contents)
           |> List.map ~f:(fun (loc, patch) ->
                match trailing_loc with
                | Some trailing_loc when Compact_loc.equal trailing_loc loc ->
                  (* Make trailing output expect blocks start on a new line. *)
                  loc, "\n" ^ patch
                | _ -> loc, patch))
    with
    | Success -> true
    | Failure | Error -> !suppress_diff_and_errors_for_testing
  in
  let corrected_fname = fname ^ ".corrected" in
  if !suppress_diff_and_errors_for_testing
     && (not !make_corrected)
     && Stdlib.Sys.file_exists corrected_fname
  then Stdlib.Sys.remove corrected_fname;
  result
;;

let setup_env () =
  (* Same as what run-tests.py does, to get repeatable output *)
  List.iter
    ~f:(fun (k, v) -> Unix.putenv k v)
    [ "LANG", "C"
    ; "LC_ALL", "C"
    ; "LANGUAGE", "C"
    ; "TZ", "GMT"
    ; "EMAIL", "Foo Bar <foo.bar@example.com>"
    ; "CDPATH", ""
    ; "COLUMNS", "80"
    ; "GREP_OPTIONS", ""
    ; "http_proxy", ""
    ; "no_proxy", ""
    ; "NO_PROXY", ""
    ; "TERM", "xterm"
    ]
;;

[%%if ocaml_version < (4, 08, 0)]

let warnings = "@a-4-29-40-41-42-44-45-48-58"
let enable_all_alerts_as_errors () = ()

[%%elif ocaml_version < (4, 10, 0)]

let warnings = "@a-4-29-40-41-42-44-45-48-58-60-66"
let enable_all_alerts_as_errors () = Warnings.parse_alert_option "@all"

[%%else]

let warnings = "@a-4-29-40-41-42-44-45-48-58-60-66-67"
let enable_all_alerts_as_errors () = Warnings.parse_alert_option "@all"

[%%endif]
[%%if ocaml_version >= (5, 0, 0)]

let set_unsafe_string () = ()

[%%else]

let set_unsafe_string () = Clflags.unsafe_string := Toplevel_backend.unsafe_string ()

[%%endif]

let setup_config () =
  Clflags.real_paths := false;
  Clflags.strict_sequence := true;
  Clflags.strict_formats := true;
  set_unsafe_string ();
  Clflags.include_dirs := "+bigarray" :: "+unix" :: !Clflags.include_dirs;
  let (_ : Warnings.alert option) = Warnings.parse_options false warnings in
  enable_all_alerts_as_errors ()
;;

let use_color = ref true
let in_place = ref false
let sexp_output = ref false
let use_absolute_path = ref false

[%%if ocaml_version < (4, 09, 0)]

let init_path () = Compmisc.init_path true

[%%else]

let init_path () = Compmisc.init_path ()

[%%endif]

let main fname =
  let cmd_line =
    Array.sub
      Stdlib.Sys.argv
      ~pos:!Arg.current
      ~len:(Array.length Stdlib.Sys.argv - !Arg.current)
  in
  setup_env ();
  setup_config ();
  Sys_unix.override_argv cmd_line;
  Toploop.set_paths ();
  init_path ();
  Toploop.toplevel_env := Compmisc.initial_env ();
  Sys.interactive := false;
  Toplevel_backend.init ~native:true (module Topdirs);
  let success =
    process_expect_file
      fname
      ~use_color:!use_color
      ~in_place:!in_place
      ~sexp_output:!sexp_output
      ~use_absolute_path:!use_absolute_path
  in
  exit (if success then 0 else 1)
;;

let args =
  Arg.align
    [ "-no-color", Clear use_color, " Produce colored diffs"
    ; "-in-place", Set in_place, " Overwirte file in place"
    ; ( "-make-corrected"
      , Set make_corrected
      , " Keep the .corrected file, even if [-in-place] is passed" )
    ; "-diff-cmd", String (fun s -> diff_command := Some s), " Diff command"
    ; "-sexp", Set sexp_output, " Output the result as a s-expression instead of diffing"
    ; "-absolute-path", Set use_absolute_path, " Use absolute path in diff-error message"
    ]
;;

let main () =
  let usage =
    Printf.sprintf
      "Usage: %s [OPTIONS] FILE [ARGS]\n"
      (Filename.basename Stdlib.Sys.argv.(0))
  in
  try
    Arg.parse args main (usage ^ "\nOptions are:");
    Out_channel.output_string Out_channel.stderr usage;
    exit 2
  with
  | exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
;;
