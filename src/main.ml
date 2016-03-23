open Toplevel_expect_test_glue.Compiler_modules
open Core_kernel.Std
open Ppx_core.Std
open Parsetree
open Expect_test_common.Std
open Expect_test_matcher.Std

let expect =
  Extension.Expert.declare "expect"
    Extension.Context.structure_item
    (Ppx_expect_payload.pattern ())
    (Ppx_expect_payload.make ~is_exact:false)

let expect_exact =
  Extension.Expert.declare "expect_exact"
    Extension.Context.structure_item
    (Ppx_expect_payload.pattern ())
    (Ppx_expect_payload.make ~is_exact:true)

let extensions = [expect; expect_exact]

type chunk =
  { phrases     : toplevel_phrase list
  ; expectation : Fmt.t Cst.t Expectation.t
  }

let split_chunks phrases =
  let rec loop phrases code_acc acc =
    match phrases with
    | [] ->
      if code_acc = [] then
        (List.rev acc, None)
      else
        (List.rev acc, Some (List.rev code_acc))
    | phrase :: phrases ->
      match phrase with
      | Ptop_def [] -> loop phrases code_acc acc
      | Ptop_def [{pstr_desc = Pstr_extension(ext, attrs); pstr_loc = loc}] -> begin
          match Extension.Expert.convert extensions ext ~loc with
          | None -> loop phrases (phrase :: code_acc) acc
          | Some f ->
            assert_no_attributes attrs;
            let e =
              { phrases     = List.rev code_acc
              ; expectation = Expectation.map_pretty (f ~extension_id_loc:(fst ext).loc)
                                ~f:Lexer.parse_pretty
              }
            in
            loop phrases [] (e :: acc)
        end
      | _ -> loop phrases (phrase :: code_acc) acc
  in
  loop phrases [] []
;;

let parse_contents ~fname contents =
  let lexbuf = Lexing.from_string contents in
  Location.init lexbuf fname;
  Location.input_name := fname;
  Parse.use_file lexbuf
;;

let reset_line_numbers = ref false
let line_numbers_delta = ref 0
let () =
  Caml.Hashtbl.add Toploop.directive_table
    "reset_line_numbers"
    (Directive_none (fun () -> reset_line_numbers := true))
;;

let print_line_numbers = ref false
let () =
  Caml.Hashtbl.add Toploop.directive_table
    "print_line_numbers"
    (Directive_bool (fun x -> print_line_numbers := x))
;;

let print_line_number ppf line =
  if !print_line_numbers then
    Format.fprintf ppf "%d" line
  else
    Format.pp_print_string ppf "_"
;;

let print_loc ppf (loc : Location.t) =
  let line = loc.loc_start.pos_lnum in
  let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  Format.fprintf ppf "Line %a" print_line_number line;
  if startchar >= 0 then
    Format.fprintf ppf ", characters %d-%d" startchar endchar;
  Format.fprintf ppf ":@.";
;;

let rec error_reporter ppf ({loc; msg; sub; if_highlight=_} : Location.error) =
  print_loc ppf loc;
  Format.pp_print_string ppf msg;
  List.iter sub ~f:(fun err ->
    Format.fprintf ppf "@\n@[<2>%a@]" error_reporter err)
;;

let warning_printer loc ppf w =
  if Warnings.is_active w then begin
    print_loc ppf loc;
    Format.fprintf ppf "Warning %a@." Warnings.print w
  end
;;

type var_and_value = V : 'a ref * 'a -> var_and_value

let protect_vars =
  let set_vars l = List.iter l ~f:(fun (V (r, v)) -> r := v) in
  fun vars ~f ->
    let backup = List.map vars ~f:(fun (V (r, _)) -> V (r, !r)) in
    set_vars vars;
    protect ~finally:(fun () -> set_vars backup) ~f
;;

let capture_compiler_stuff ppf ~f =
  protect_vars
    [ V (Location.formatter_for_warnings , ppf            )
    ; V (Location.warning_printer        , warning_printer)
    ; V (Location.error_reporter         , error_reporter )
    ]
    ~f
;;

let apply_rewriters = function
  | Ptop_dir _ as x -> x
  | Ptop_def s -> Ptop_def (Ppx_driver.map_structure s)
;;

let verbose = ref false
let () =
  Caml.Hashtbl.add Toploop.directive_table
    "verbose"
    (Directive_bool (fun x -> verbose := x))
;;

let shift_line_numbers = object
  inherit [int] Ast_traverse.map_with_context
  method! lexing_position delta pos =
    { pos with pos_lnum  = pos.pos_lnum + delta }
end

let exec_phrase ppf phrase =
  if !reset_line_numbers then begin
    match phrase with
    | Ptop_def (st :: _) ->
      reset_line_numbers := false;
      line_numbers_delta := 1 - st.pstr_loc.loc_start.pos_lnum
    | _ -> ()
  end;
  let phrase =
    match !line_numbers_delta with
    | 0 -> phrase
    | n -> shift_line_numbers#toplevel_phrase n phrase
  in
  let phrase = apply_rewriters phrase in
  if !Clflags.dump_parsetree then Printast. top_phrase ppf phrase;
  if !Clflags.dump_source    then Pprintast.top_phrase ppf phrase;
  Toploop.execute_phrase !verbose ppf phrase
;;

let count_newlines : _ Cst.t Expectation.Body.t -> int =
  let count s = String.count s ~f:(Char.(=) '\n') in
  function
  | Exact s -> count s
  | Pretty cst ->
    match cst with
    | Empty       e -> count e
    | Single_line s -> count s.trailing_spaces
    | Multi_lines m ->
      List.length m.lines - 1 +
      count m.leading_spaces  +
      count m.trailing_spaces
;;

let canonicalize_cst : 'a Cst.t -> 'a Cst.t = function
  | Empty _ -> Empty "\n"
  | Single_line s ->
    Multi_lines
      { leading_spaces  = "\n"
      ; trailing_spaces = "\n"
      ; indentation     = ""
      ; lines           =
          [ Not_blank
              { trailing_blanks = ""
              ; orig            = s.orig
              ; data            = s.data
              }
          ]
      }
  | Multi_lines m ->
    Multi_lines
      { leading_spaces  = "\n"
      ; trailing_spaces = "\n"
      ; indentation     = ""
      ; lines           = List.map m.lines ~f:Cst.Line.strip
      }
;;

let reconcile ~actual ~expect : _ Reconcile.Result.t =
  match
    Reconcile.expectation_body
      ~expect
      ~actual
      ~default_indent:0
      ~pad_single_line:false
  with
  | Match -> Match
  | Correction c -> Correction (Expectation.Body.map_pretty c ~f:canonicalize_cst)
;;

let eval_expect_file fname ~file_contents =
  (* 4.03: Warnings.reset_fatal (); *)
  let chunks, trailing_code = parse_contents ~fname file_contents |> split_chunks in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  reset_line_numbers := false;
  line_numbers_delta := 0;
  let exec_phrases phrases =
    (* So that [%expect_exact] nodes look nice *)
    Buffer.add_char buf '\n';
    List.iter phrases ~f:(fun phrase ->
      match exec_phrase ppf phrase with
      | (_ : bool) -> ()
      | exception exn ->
        Location.report_exception ppf exn);
    Format.pp_print_flush ppf ();
    let len = Buffer.length buf in
    if len > 0 && Buffer.nth buf (len - 1) <> '\n' then
      (* So that [%expect_exact] nodes look nice *)
      Buffer.add_char buf '\n';
    let s = Buffer.contents buf in
    Buffer.clear buf;
    s
  in
  let corrections =
    capture_compiler_stuff ppf ~f:(fun () ->
      List.filter_map chunks ~f:(fun chunk ->
        let actual = exec_phrases chunk.phrases in
        match reconcile ~actual ~expect:chunk.expectation.body with
        | Match -> None
        | Correction correction ->
          line_numbers_delta :=
            !line_numbers_delta +
            count_newlines correction -
            count_newlines chunk.expectation.body;
          Some (chunk.expectation,
                Matcher.Test_correction.Correction correction)))
  in
  let trailing_output =
    match trailing_code with
    | None -> Reconcile.Result.Match
    | Some phrases ->
      capture_compiler_stuff ppf ~f:(fun () ->
        let actual = exec_phrases phrases in
        reconcile ~actual ~expect:(Pretty Cst.empty)
      )
  in
  Matcher.Test_correction.make
    ~location:{ filename    = File.Name.of_string fname
              ; line_number = 1
              ; line_start  = 0
              ; start_pos   = 0
              ; end_pos     = String.length file_contents
              }
    ~corrections
    ~trailing_output
;;

let process_expect_file fname ~use_color =
  let file_contents = In_channel.read_all fname in
  let result = eval_expect_file fname ~file_contents in
  let corrected_fname = fname ^ ".corrected" in
  match result with
  | Correction correction ->
    Matcher.write_corrected [correction]
      ~file:corrected_fname ~file_contents ~mode:Toplevel_expect_test;
    Matcher.print_diff ~file1:fname ~file2:corrected_fname ~use_color;
    false
  | Match ->
    if Sys.file_exists corrected_fname then
      Sys.remove corrected_fname;
    true
;;

let override_sys_argv args =
  let len = Array.length args in
  assert (len <= Array.length Sys.argv);
  Array.blit ~src:args ~src_pos:0 ~dst:Sys.argv ~dst_pos:0 ~len;
  Obj.truncate (Obj.repr Sys.argv) len;
  Arg.current := 0;
;;

let setup_env () =
  (* Same as what run-tests.py does, to get repeatable output *)
  List.iter ~f:(fun (k, v) -> Unix.putenv k v)
    [ "LANG"        , "C"
    ; "LC_ALL"      , "C"
    ; "LANGUAGE"    , "C"
    ; "TZ"          , "GMT"
    ; "EMAIL"       , "Foo Bar <foo.bar@example.com>"
    ; "CDPATH"      , ""
    ; "COLUMNS"     , "80"
    ; "GREP_OPTIONS", ""
    ; "http_proxy"  , ""
    ; "no_proxy"    , ""
    ; "NO_PROXY"    , ""
    ; "TERM"        , "xterm"
    ]

let setup_config () =
  Clflags.real_paths      := false;
  Clflags.strict_sequence := true;
  Clflags.strict_formats  := true;
  Warnings.parse_options false "@a-4-29-40-41-42-44-45-48";
;;

let main fname cmd_line ~use_color =
  setup_env ();
  setup_config ();
  override_sys_argv (Array.of_list cmd_line);
  Toploop.set_paths ();
  Compmisc.init_path true;
  Toploop.toplevel_env := Compmisc.initial_env ();
  Sys.interactive := false;
  Toplevel_expect_test_glue.init ();
  let success = process_expect_file fname ~use_color in
  exit (if success then 0 else 1)
;;

let main () =
  try
    match Array.to_list Sys.argv with
    | _ :: "-no-color" :: (fname :: _ as cmd_line) ->
      main fname cmd_line ~use_color:false
    | _ :: (fname :: _ as cmd_line) ->
      main fname cmd_line ~use_color:true
    | _ ->
      eprintf "Usage: %s FILE [ARGS]\n" (Filename.basename Sys.executable_name);
      exit 2
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
;;
