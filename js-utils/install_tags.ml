let package_name = "toplevel_expect_test"

let sections =
  [ ("lib",
    [ ("built_lib_toplevel_expect_test", None)
    ; ("built_lib_toplevel_expect_test_glue", None)
    ],
    [ ("META", None)
    ])
  ; ("bin",
    [ ("built_exec_ocaml-expect", Some "ocaml-expect")
    ],
    [])
  ]
