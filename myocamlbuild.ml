(* OASIS_START *)
(* OASIS_STOP *)
# 3 "myocamlbuild.ml"

(* Temporary hacks *)
let js_hacks = function
  | After_rules ->
    rule "Generate a cmxs from a cmxa"
      ~dep:"%.cmxa"
      ~prod:"%.cmxs"
      ~insert:`top
      (fun env _ ->
         Cmd (S [ !Options.ocamlopt
                ; A "-shared"
                ; A "-linkall"
                ; A "-I"; A (Pathname.dirname (env "%"))
                ; A (env "%.cmxa")
                ; A "-o"
                ; A (env "%.cmxs")
            ]));

    (* Pass -predicates to ocamldep *)
    pflag ["ocaml"; "ocamldep"] "predicate" (fun s -> S [A "-predicates"; A s])
  | _ -> ()

let dispatch = function
  | Before_rules ->
    (* I'm fed up of all this... *)
    flag ["ocaml"; "link"; "another_ridiculous_hack"] & S[A "-package"; A "compiler-libs.toplevel,findlib"]
  | _ ->
    ()

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    dispatch hook;
    js_hacks hook;
    Ppx_driver_ocamlbuild.dispatch hook;
    dispatch_default hook)
