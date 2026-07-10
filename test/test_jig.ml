open Jig_core

let contains ~affix haystack =
  let affix_length = String.length affix in
  let haystack_length = String.length haystack in
  let rec scan index =
    if index + affix_length > haystack_length then false
    else if String.sub haystack index affix_length = affix then true
    else scan (index + 1)
  in
  scan 0

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then (
    mkdir_p (Filename.dirname dir);
    (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()))

let write_file path content =
  mkdir_p (Filename.dirname path);
  Out_channel.with_open_text path (fun channel ->
      Out_channel.output_string channel content)

let make_temp_root () =
  let root =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "jig-test-%d-%d" (Unix.getpid ()) (Random.int 1_000_000))
  in
  let mkdir path = Unix.mkdir path 0o755 in
  mkdir root;
  mkdir (Filename.concat root ".jig");
  mkdir (Filename.concat root ".jig/workflows");
  mkdir (Filename.concat root ".jig/skills");
  root

let add_skill root name body =
  let dir = Filename.concat root (Filename.concat ".jig/skills" name) in
  Unix.mkdir dir 0o755;
  write_file (Filename.concat dir "SKILL.md") body

(* Workflow parsing *)

let test_workflow_parses () =
  let yaml = "name: hello\nsteps:\n  - skill: say-hi\n  - skill: say-bye\n" in
  match Workflow.of_string yaml with
  | Error message -> Alcotest.fail message
  | Ok workflow ->
      Alcotest.(check string) "name" "hello" workflow.Workflow.name;
      Alcotest.(check (list string))
        "steps"
        [ "say-hi"; "say-bye" ]
        (Workflow.referenced_skills workflow)

let full_schema_yaml =
  {|name: bugfix
steps:
  - skill: reproduce-issue
    on_fail: escalate
  - skill: write-failing-test
    with:
      spec: docs/specs/login.md
  - retry:
      max_iterations: 3
      on_exhausted: escalate
      steps:
        - skill: implement-fix
        - skill: run-tests
          until: pass
  - skill: open-pr
|}

let test_workflow_parses_full_schema () =
  match Workflow.of_string full_schema_yaml with
  | Error message -> Alcotest.fail message
  | Ok workflow -> (
      Alcotest.(check (list string))
        "all skills seen"
        [
          "reproduce-issue";
          "write-failing-test";
          "implement-fix";
          "run-tests";
          "open-pr";
        ]
        (Workflow.referenced_skills workflow);
      match workflow.Workflow.entries with
      | [ Workflow.Step first; Workflow.Step second; Workflow.Retry retry; _ ]
        ->
          Alcotest.(check bool) "on_fail parsed" true
            (first.Workflow.on_fail = Some Workflow.Escalate);
          Alcotest.(check (list (pair string string)))
            "with inputs parsed"
            [ ("spec", "docs/specs/login.md") ]
            second.Workflow.inputs;
          Alcotest.(check int) "max_iterations" 3 retry.Workflow.max_iterations;
          Alcotest.(check bool) "on_exhausted parsed" true
            (retry.Workflow.on_exhausted = Workflow.Escalate);
          Alcotest.(check bool) "until pass on last retry step" true
            (match List.rev retry.Workflow.retry_steps with
            | last :: _ -> last.Workflow.until_pass
            | [] -> false)
      | _ -> Alcotest.fail "expected step/step/retry/step entries")

let expect_workflow_error ~mentions yaml =
  match Workflow.of_string yaml with
  | Ok _ -> Alcotest.fail (Printf.sprintf "expected an error mentioning %S" mentions)
  | Error message ->
      Alcotest.(check bool)
        (Printf.sprintf "error mentions %S" mentions)
        true
        (contains ~affix:mentions message)


(* forEach items files *)

let write_items root name content =
  let path = Filename.concat root name in
  write_file path content;
  path

let expect_items_error ~mentions path =
  match Items.load ~path with
  | Ok _ -> Alcotest.fail (Printf.sprintf "expected an error mentioning %S" mentions)
  | Error message ->
      Alcotest.(check bool)
        (Printf.sprintf "error mentions %S" mentions)
        true
        (contains ~affix:mentions message)

let test_items_parses_tsv () =
  let root = make_temp_root () in
  let path =
    write_items root "ports.tsv" "name\tspec\nalpha\tspecs/a.md\nbeta\tspecs/b.md\n"
  in
  match Items.load ~path with
  | Error message -> Alcotest.fail message
  | Ok items ->
      Alcotest.(check int) "two items" 2 (List.length items);
      Alcotest.(check (list (pair string string)))
        "bindings keyed by header, in order"
        [ ("name", "alpha"); ("spec", "specs/a.md") ]
        (List.hd items);
      Alcotest.(check string) "first column is the key" "alpha"
        (Items.key (List.hd items))

let test_items_parses_json () =
  let root = make_temp_root () in
  let path =
    write_items root "ports.json"
      {|[{"name": "alpha", "spec": "specs/a.md"}, {"name": "beta", "spec": "specs/b.md"}]|}
  in
  match Items.load ~path with
  | Error message -> Alcotest.fail message
  | Ok items ->
      Alcotest.(check int) "two items" 2 (List.length items);
      Alcotest.(check string) "key of the second" "beta"
        (Items.key (List.nth items 1))

let test_items_tolerates_crlf_tsv () =
  let root = make_temp_root () in
  let path =
    write_items root "ports.tsv" "name\tspec\r\nalpha\tspecs/a.md\r\n"
  in
  match Items.load ~path with
  | Error message -> Alcotest.fail message
  | Ok items ->
      Alcotest.(check (list (pair string string)))
        "carriage returns trimmed"
        [ ("name", "alpha"); ("spec", "specs/a.md") ]
        (List.hd items)

let test_items_rejects_header_only_tsv () =
  let root = make_temp_root () in
  expect_items_error ~mentions:"no items"
    (write_items root "ports.tsv" "name\tspec\n")

let test_items_rejects_empty_json_array () =
  let root = make_temp_root () in
  expect_items_error ~mentions:"no items" (write_items root "ports.json" "[]")

let test_items_rejects_ragged_tsv_row () =
  let root = make_temp_root () in
  expect_items_error ~mentions:"header has 2"
    (write_items root "ports.tsv" "name\tspec\nalpha\n")

let test_items_rejects_duplicate_columns () =
  let root = make_temp_root () in
  expect_items_error ~mentions:"duplicate column"
    (write_items root "ports.tsv" "name\tname\nalpha\tbeta\n")

let test_items_rejects_duplicate_keys () =
  let root = make_temp_root () in
  expect_items_error ~mentions:"duplicate item key"
    (write_items root "ports.tsv" "name\tspec\nalpha\tone\nalpha\ttwo\n")

let test_items_rejects_non_string_json_value () =
  let root = make_temp_root () in
  expect_items_error ~mentions:"must be a string"
    (write_items root "ports.json" {|[{"name": "alpha", "count": 3}]|})

let test_items_rejects_ragged_json_objects () =
  let root = make_temp_root () in
  expect_items_error ~mentions:"same keys"
    (write_items root "ports.json"
       {|[{"name": "alpha", "spec": "a"}, {"name": "beta"}]|})

let test_items_rejects_unknown_extension () =
  let root = make_temp_root () in
  expect_items_error ~mentions:".tsv"
    (write_items root "ports.csv" "name\nalpha\n")

let test_retry_requires_max_iterations () =
  expect_workflow_error ~mentions:"max_iterations"
    "name: x\nsteps:\n  - retry:\n      on_exhausted: escalate\n      steps:\n        - skill: a\n"

let test_retry_requires_on_exhausted () =
  expect_workflow_error ~mentions:"on_exhausted"
    "name: x\nsteps:\n  - retry:\n      max_iterations: 3\n      steps:\n        - skill: a\n"

let test_unknown_top_level_key_rejected () =
  expect_workflow_error ~mentions:"timeout"
    "name: x\ntimeout: 5\nsteps:\n  - skill: a\n"

let test_unknown_step_key_rejected () =
  expect_workflow_error ~mentions:"model"
    "name: x\nsteps:\n  - skill: a\n    model: opus\n"

let test_until_outside_retry_rejected () =
  expect_workflow_error ~mentions:"until"
    "name: x\nsteps:\n  - skill: a\n    until: pass\n"

let test_nested_retry_rejected () =
  expect_workflow_error ~mentions:"nest"
    "name: x\nsteps:\n  - retry:\n      max_iterations: 2\n      on_exhausted: abort\n      steps:\n        - retry:\n            max_iterations: 2\n            on_exhausted: abort\n            steps:\n              - skill: a\n"

let test_invalid_on_fail_value_rejected () =
  expect_workflow_error ~mentions:"give-up"
    "name: x\nsteps:\n  - skill: a\n    on_fail: give-up\n"

let test_with_inputs_parse_in_file_order () =
  match
    Workflow.of_string
      "name: x\n\
       steps:\n\
      \  - skill: a\n\
      \    with:\n\
      \      spec: docs/spec.md\n\
      \      fixture: test/f.tsv\n"
  with
  | Error message -> Alcotest.fail message
  | Ok workflow -> (
      match workflow.Workflow.entries with
      | [ Workflow.Step step ] ->
          Alcotest.(check (list (pair string string)))
            "inputs in file order"
            [ ("spec", "docs/spec.md"); ("fixture", "test/f.tsv") ]
            step.Workflow.inputs
      | _ -> Alcotest.fail "expected a single step")

let test_with_allowed_inside_retry () =
  match
    Workflow.of_string
      "name: x\n\
       steps:\n\
      \  - retry:\n\
      \      max_iterations: 2\n\
      \      on_exhausted: escalate\n\
      \      steps:\n\
      \        - skill: a\n\
      \          with:\n\
      \            spec: s.md\n\
      \        - skill: b\n\
      \          until: pass\n"
  with
  | Error message -> Alcotest.fail message
  | Ok workflow -> (
      match workflow.Workflow.entries with
      | [ Workflow.Retry retry ] -> (
          match retry.Workflow.retry_steps with
          | [ first; second ] ->
              Alcotest.(check (list (pair string string)))
                "first retry step carries its inputs"
                [ ("spec", "s.md") ]
                first.Workflow.inputs;
              Alcotest.(check (list (pair string string)))
                "second retry step has none" [] second.Workflow.inputs
          | _ -> Alcotest.fail "expected two retry steps")
      | _ -> Alcotest.fail "expected a retry entry")

let test_context_parses () =
  match
    Workflow.of_string
      "name: x\ncontext: |\n  keep the tokens\n  no visual change\nsteps:\n  - skill: a\n"
  with
  | Error message -> Alcotest.fail message
  | Ok workflow ->
      Alcotest.(check (option string))
        "context parsed"
        (Some "keep the tokens\nno visual change\n")
        workflow.Workflow.context

let test_context_absent_is_none () =
  match Workflow.of_string "name: x\nsteps:\n  - skill: a\n" with
  | Error message -> Alcotest.fail message
  | Ok workflow ->
      Alcotest.(check (option string))
        "no context" None workflow.Workflow.context

let test_context_rejects_non_string () =
  expect_workflow_error ~mentions:"context"
    "name: x\ncontext:\n  - a\nsteps:\n  - skill: a\n"

let test_with_rejects_non_string_value () =
  expect_workflow_error ~mentions:"quote"
    "name: x\nsteps:\n  - skill: a\n    with:\n      count: 3\n"

let test_with_rejects_non_mapping () =
  expect_workflow_error ~mentions:"mapping"
    "name: x\nsteps:\n  - skill: a\n    with:\n      - spec\n"

let test_with_rejects_duplicate_keys () =
  expect_workflow_error ~mentions:"duplicate"
    "name: x\nsteps:\n  - skill: a\n    with:\n      spec: one.md\n      spec: two.md\n"

let for_each_yaml =
  "name: fanout\n\
   steps:\n\
  \  - forEach:\n\
  \      items: ports.tsv\n\
  \      as: port\n\
  \      steps:\n\
  \        - retry:\n\
  \            max_iterations: 3\n\
  \            on_exhausted: escalate\n\
  \            steps:\n\
  \              - skill: implement-port\n\
  \                with:\n\
  \                  spec: \"{{ port.spec }}\"\n\
  \              - skill: run-tests\n\
  \                until: pass\n\
  \  - skill: open-pr\n"

let test_for_each_parses () =
  match Workflow.of_string for_each_yaml with
  | Error message -> Alcotest.fail message
  | Ok workflow -> (
      Alcotest.(check (list string))
        "referenced skills recurse into the body"
        [ "implement-port"; "run-tests"; "open-pr" ]
        (Workflow.referenced_skills workflow);
      match workflow.Workflow.entries with
      | [ Workflow.For_each for_each; Workflow.Step _ ] ->
          Alcotest.(check string) "items file" "ports.tsv"
            for_each.Workflow.items_file;
          Alcotest.(check string) "binding name" "port" for_each.Workflow.var;
          Alcotest.(check (list string))
            "referenced columns" [ "spec" ]
            (Workflow.for_each_columns for_each)
      | _ -> Alcotest.fail "expected forEach + step entries")

let test_for_each_requires_items () =
  expect_workflow_error ~mentions:"items"
    "name: x\nsteps:\n  - forEach:\n      as: port\n      steps:\n        - skill: a\n"

let test_for_each_requires_as () =
  expect_workflow_error ~mentions:"as"
    "name: x\nsteps:\n  - forEach:\n      items: p.tsv\n      steps:\n        - skill: a\n"

let test_for_each_requires_steps () =
  expect_workflow_error ~mentions:"steps"
    "name: x\nsteps:\n  - forEach:\n      items: p.tsv\n      as: port\n"

let test_for_each_rejects_nesting () =
  expect_workflow_error ~mentions:"nest"
    "name: x\n\
     steps:\n\
    \  - forEach:\n\
    \      items: p.tsv\n\
    \      as: port\n\
    \      steps:\n\
    \        - forEach:\n\
    \            items: q.tsv\n\
    \            as: other\n\
    \            steps:\n\
    \              - skill: a\n"

let test_for_each_rejected_inside_retry () =
  expect_workflow_error ~mentions:"forEach"
    "name: x\n\
     steps:\n\
    \  - retry:\n\
    \      max_iterations: 2\n\
    \      on_exhausted: abort\n\
    \      steps:\n\
    \        - forEach:\n\
    \            items: p.tsv\n\
    \            as: port\n\
    \            steps:\n\
    \              - skill: a\n"

let test_for_each_rejects_dotted_as () =
  expect_workflow_error ~mentions:"letters"
    "name: x\nsteps:\n  - forEach:\n      items: p.tsv\n      as: a.b\n      steps:\n        - skill: a\n"

let test_for_each_rejects_bad_items_extension () =
  expect_workflow_error ~mentions:".tsv"
    "name: x\nsteps:\n  - forEach:\n      items: p.csv\n      as: port\n      steps:\n        - skill: a\n"

let test_for_each_rejects_empty_steps () =
  expect_workflow_error ~mentions:"empty"
    "name: x\nsteps:\n  - forEach:\n      items: p.tsv\n      as: port\n      steps: []\n"

let test_for_each_rejects_unbound_variable () =
  expect_workflow_error ~mentions:"binds"
    "name: x\n\
     steps:\n\
    \  - forEach:\n\
    \      items: p.tsv\n\
    \      as: port\n\
    \      steps:\n\
    \        - skill: a\n\
    \          with:\n\
    \            spec: \"{{ other.spec }}\"\n"

let test_for_each_rejects_malformed_placeholder () =
  expect_workflow_error ~mentions:"placeholder"
    "name: x\n\
     steps:\n\
    \  - forEach:\n\
    \      items: p.tsv\n\
    \      as: port\n\
    \      steps:\n\
    \        - skill: a\n\
    \          with:\n\
    \            spec: \"{{ port }}\"\n"

let test_for_each_rejects_interpolated_skill () =
  expect_workflow_error ~mentions:"only allowed in with"
    "name: x\n\
     steps:\n\
    \  - forEach:\n\
    \      items: p.tsv\n\
    \      as: port\n\
    \      steps:\n\
    \        - skill: \"{{ port.name }}\"\n"

let test_non_positive_max_iterations_rejected () =
  expect_workflow_error ~mentions:"positive"
    "name: x\nsteps:\n  - retry:\n      max_iterations: 0\n      on_exhausted: abort\n      steps:\n        - skill: a\n"

let test_workflow_rejects_missing_name () =
  match Workflow.of_string "steps:\n  - skill: say-hi\n" with
  | Ok _ -> Alcotest.fail "expected an error for missing name"
  | Error message ->
      Alcotest.(check bool) "mentions name" true
        (String.length message > 0
        && contains ~affix:"name" message)

let test_workflow_rejects_empty_steps () =
  match Workflow.of_string "name: hello\nsteps: []\n" with
  | Ok _ -> Alcotest.fail "expected an error for empty steps"
  | Error _ -> ()

(* Config parsing *)

let test_config_parses () =
  match Config.of_string "harness:\n  - claude\n  - -p\n" with
  | Error message -> Alcotest.fail message
  | Ok config ->
      Alcotest.(check (list string))
        "harness" [ "claude"; "-p" ] config.Config.harness

let test_config_rejects_missing_harness () =
  match Config.of_string "other: true\n" with
  | Ok _ -> Alcotest.fail "expected an error for missing harness"
  | Error _ -> ()

(* Run records *)

let test_run_id_format () =
  let id = Run.make_id ~workflow:"bugfix" ~time:0.0 ~pid:42 in
  Alcotest.(check bool)
    "contains workflow name" true
    (contains ~affix:"-bugfix-" id);
  Alcotest.(check bool) "ends with pid" true (contains ~affix:"-42" id)

let test_run_ids_differ_across_processes () =
  let first = Run.make_id ~workflow:"bugfix" ~time:0.0 ~pid:1 in
  let second = Run.make_id ~workflow:"bugfix" ~time:0.0 ~pid:2 in
  Alcotest.(check bool) "same second, different pid" true (first <> second)

let test_store_saves_run () =
  let root = make_temp_root () in
  let run =
    {
      Run.id = "2026-07-02-hello-120000-1";
      workflow = "hello";
      task = "test";
      status = Run.Paused;
      error = None;
      position = { Run.entry_index = 2; iterations_used = 1; for_each = None };
      workspace = Some "/somewhere/worktree";
      steps = [];
      started_at = "2026-07-02T12:00:00Z";
      finished_at = None;
    }
  in
  let runs_dir = Project.runs_dir ~root in
  match Store.Filesystem.save ~runs_dir run with
  | Error message -> Alcotest.fail message
  | Ok path -> (
      Alcotest.(check bool) "run file exists" true (Sys.file_exists path);
      match Store.Filesystem.load ~runs_dir ~id:run.Run.id with
      | Error message -> Alcotest.fail message
      | Ok loaded ->
          Alcotest.(check string) "id roundtrips" run.Run.id loaded.Run.id;
          Alcotest.(check string) "status roundtrips" "paused"
            (Run.string_of_status loaded.Run.status);
          Alcotest.(check int) "entry index roundtrips" 2
            loaded.Run.position.Run.entry_index;
          Alcotest.(check int) "iterations roundtrip" 1
            loaded.Run.position.Run.iterations_used;
          Alcotest.(check bool) "workspace roundtrips" true
            (loaded.Run.workspace = Some "/somewhere/worktree");
          Alcotest.(check bool) "no finished_at while paused" true
            (loaded.Run.finished_at = None))

(* Handoff parsing *)

let handoff_block ?(status = "pass") ?(summary = "") () =
  Printf.sprintf "```handoff\nstatus: %s\nsummary: %S\n```\n" status summary

let test_handoff_parses () =
  let output =
    "some agent chatter\n\n```handoff\nstatus: pass\nartifacts:\n  - \
     src/foo.ml\nsummary: did the thing\n```\n"
  in
  match Handoff.parse output with
  | Error message -> Alcotest.fail message
  | Ok handoff ->
      Alcotest.(check string) "status" "pass"
        (Handoff.string_of_status handoff.Handoff.status);
      Alcotest.(check (list string))
        "artifacts" [ "src/foo.ml" ] handoff.Handoff.artifacts;
      Alcotest.(check string) "summary" "did the thing" handoff.Handoff.summary

let test_handoff_empty_artifacts_key () =
  match
    Handoff.parse
      "```handoff\nstatus: escalate\nartifacts:\nsummary: which wording?\n```\n"
  with
  | Error message -> Alcotest.fail message
  | Ok handoff ->
      Alcotest.(check (list string))
        "bare artifacts key means none" [] handoff.Handoff.artifacts;
      Alcotest.(check string) "status" "escalate"
        (Handoff.string_of_status handoff.Handoff.status)

let test_handoff_empty_artifact_entries () =
  (match
     Handoff.parse
       "```handoff\nstatus: escalate\nartifacts:\n  - []\nsummary: stuck\n```\n"
   with
  | Error message -> Alcotest.fail message
  | Ok handoff ->
      Alcotest.(check (list string))
        "an empty-list entry means none" [] handoff.Handoff.artifacts);
  (match
     Handoff.parse
       "```handoff\nstatus: pass\nartifacts:\n  - src/real.ml\n  -\n```\n"
   with
  | Error message -> Alcotest.fail message
  | Ok handoff ->
      Alcotest.(check (list string))
        "paths survive next to empty entries" [ "src/real.ml" ]
        handoff.Handoff.artifacts);
  match
    Handoff.parse "```handoff\nstatus: pass\nartifacts:\n  - 3\n```\n"
  with
  | Ok _ -> Alcotest.fail "a numeric artifact entry must stay an error"
  | Error message ->
      Alcotest.(check bool) "names the artifacts rule" true
        (contains ~affix:"artifacts" message)

let test_handoff_last_block_wins () =
  let output =
    "```handoff\nstatus: fail\n```\nrevised:\n```handoff\nstatus: pass\n```\n"
  in
  match Handoff.parse output with
  | Error message -> Alcotest.fail message
  | Ok handoff ->
      Alcotest.(check string) "status" "pass"
        (Handoff.string_of_status handoff.Handoff.status)

let test_handoff_missing_block () =
  match Handoff.parse "just prose, no block" with
  | Ok _ -> Alcotest.fail "expected an error for missing handoff block"
  | Error message ->
      Alcotest.(check bool) "mentions handoff" true
        (contains ~affix:"handoff" message)

let test_handoff_unknown_status () =
  match Handoff.parse "```handoff\nstatus: maybe\n```\n" with
  | Ok _ -> Alcotest.fail "expected an error for unknown status"
  | Error message ->
      Alcotest.(check bool) "mentions the status" true
        (contains ~affix:"maybe" message)

(* End-to-end through the default (subprocess) executor *)

let setup_project root ~harness =
  write_file
    (Filename.concat root ".jig/config.yaml")
    (Printf.sprintf "harness:\n%s"
       (String.concat ""
          (List.map (fun part -> Printf.sprintf "  - %S\n" part) harness)));
  write_file
    (Filename.concat root ".jig/workflows/hello.yaml")
    "name: hello\nsteps:\n  - skill: say-hi\n";
  add_skill root "say-hi" "# Say hi\n\nGreet the user.\n"

(* Echoes the prompt, then emits a passing handoff - the shape of a
   well-behaved harness. The prompt arrives as $0 of the sh script. *)
let passing_harness =
  [ "sh"; "-c"; "echo \"$0\"; printf '```handoff\\nstatus: pass\\n```\\n'" ]

let test_end_to_end_pass () =
  let root = make_temp_root () in
  setup_project root ~harness:passing_harness;
  match
    Runner.Default.execute_run ~root ~workflow_name:"hello" ~task:"say hi" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, path) ->
      Alcotest.(check string) "status" "completed"
        (Run.string_of_status run.Run.status);
      Alcotest.(check int) "one step" 1 (List.length run.Run.steps);
      Alcotest.(check bool) "record persisted" true (Sys.file_exists path)

let test_missing_handoff_is_distinct () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "echo" ];
  match
    Runner.Default.execute_run ~root ~workflow_name:"hello" ~task:"say hi" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      Alcotest.(check string) "status" "failed"
        (Run.string_of_status run.Run.status);
      match run.Run.steps with
      | [ step ] ->
          Alcotest.(check string) "outcome" "invalid-handoff"
            (Run.string_of_outcome step.Run.outcome);
          Alcotest.(check bool) "reason recorded" true
            (step.Run.handoff_error <> None)
      | _ -> Alcotest.fail "expected exactly one step")

let test_end_to_end_fail () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "false" ];
  match
    Runner.Default.execute_run ~root ~workflow_name:"hello" ~task:"say hi" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "status" "failed"
        (Run.string_of_status run.Run.status)

let test_missing_skill_fails_validation () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "echo" ];
  write_file
    (Filename.concat root ".jig/workflows/broken.yaml")
    "name: broken\nsteps:\n  - skill: does-not-exist\n";
  match
    Runner.Default.execute_run ~root ~workflow_name:"broken" ~task:"x" ~isolated:false ()
  with
  | Ok _ -> Alcotest.fail "expected an error for a missing skill"
  | Error message ->
      Alcotest.(check bool) "mentions the skill" true
        (contains ~affix:"does-not-exist" message);
      Alcotest.(check bool) "refused before running: no runs dir" false
        (Sys.file_exists (Project.runs_dir ~root))

let test_validate_reports_missing_skills () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "echo" ];
  write_file
    (Filename.concat root ".jig/workflows/broken.yaml")
    "name: broken\nsteps:\n  - skill: say-hi\n  - skill: ghost-one\n  - skill: ghost-two\n";
  let jig_dir = Filename.concat root ".jig" in
  match
    Result.bind
      (Workflow.load ~path:(Filename.concat jig_dir "workflows/broken.yaml"))
      (fun workflow -> Validate.workflow ~workflow_dir:(Filename.concat jig_dir "workflows") ~jig_dir ~skill_paths:[] workflow)
  with
  | Ok () -> Alcotest.fail "expected validation to fail"
  | Error message ->
      Alcotest.(check bool) "lists first missing skill" true
        (contains ~affix:"ghost-one" message);
      Alcotest.(check bool) "lists second missing skill" true
        (contains ~affix:"ghost-two" message)

let test_workflow_rejects_name_with_slash () =
  match Workflow.of_string "name: team/deploy\nsteps:\n  - skill: x\n" with
  | Ok _ -> Alcotest.fail "expected an error for a name containing '/'"
  | Error message ->
      Alcotest.(check bool) "mentions the name" true
        (contains ~affix:"team/deploy" message)

let test_step_output_is_recorded () =
  let root = make_temp_root () in
  setup_project root ~harness:passing_harness;
  match
    Runner.Default.execute_run ~root ~workflow_name:"hello" ~task:"say hi" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      match run.Run.steps with
      | [ step ] ->
          Alcotest.(check bool)
            "stdout captured the prompt" true
            (contains ~affix:"say hi" step.Run.stdout)
      | _ -> Alcotest.fail "expected exactly one step")

(* Port swappability: the runner accepts any Executor implementation. The
   scripted executor records every prompt and replays queued outputs, which
   also lets the threading tests observe what each step was told. *)

module Scripted_executor : sig
  include Executor.S

  val reset : outputs:string list -> unit
  val prompts : unit -> string list
end = struct
  let recorded_prompts : string list ref = ref []
  let queued_outputs : string list ref = ref []

  let reset ~outputs =
    recorded_prompts := [];
    queued_outputs := outputs

  let prompts () = !recorded_prompts

  let execute ~command:_ ~cwd:_ ~prompt =
    recorded_prompts := !recorded_prompts @ [ prompt ];
    let stdout =
      match !queued_outputs with
      | [] -> handoff_block ()
      | next :: rest ->
          queued_outputs := rest;
          next
    in
    Ok { Executor.exit_code = 0; stdout; stderr = "" }
end

module Scripted_runner =
  Runner.Make (Scripted_executor) (Model_provider.Default) (Metering.Noop)
    (Store.Filesystem)

let setup_three_step_project root =
  setup_project root ~harness:[ "irrelevant" ];
  write_file
    (Filename.concat root ".jig/workflows/pipeline.yaml")
    "name: pipeline\nsteps:\n  - skill: one\n  - skill: two\n  - skill: three\n";
  List.iter (fun name -> add_skill root name ("# " ^ name ^ "\n")) [ "one"; "two"; "three" ]

let test_executor_is_swappable () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  Scripted_executor.reset ~outputs:[];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"hello" ~task:"say hi" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "status" "completed"
        (Run.string_of_status run.Run.status)

let test_module_form_workflow_runs () =
  let root = make_temp_root () in
  setup_project root ~harness:passing_harness;
  (* A module-directory workflow with a co-located AGENTS.md and items. *)
  write_file
    (Filename.concat root ".jig/workflows/mod/workflow.yaml")
    "name: mod\n\
     steps:\n\
    \  - forEach:\n\
    \      items: items.tsv\n\
    \      as: it\n\
    \      steps:\n\
    \        - skill: say-hi\n";
  write_file
    (Filename.concat root ".jig/workflows/mod/items.tsv")
    "name\nalpha\nbeta\n";
  write_file
    (Filename.concat root ".jig/workflows/mod/AGENTS.md")
    "the constant framing\n";
  Scripted_executor.reset ~outputs:[ handoff_block (); handoff_block () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"mod" ~task:"t"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "status" "completed"
        (Run.string_of_status run.Run.status);
      Alcotest.(check int) "two items ran" 2 (List.length run.Run.steps);
      List.iter
        (fun prompt ->
          Alcotest.(check bool) "AGENTS.md became the context" true
            (contains ~affix:"Context:\nthe constant framing" prompt))
        (Scripted_executor.prompts ())

let test_list_shows_module_and_flat_forms () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  write_file
    (Filename.concat root ".jig/workflows/mod/workflow.yaml")
    "name: mod\nsteps:\n  - skill: say-hi\n";
  let jig_dir = Filename.concat root ".jig" in
  match Project.list_workflows ~jig_dir with
  | Error message -> Alcotest.fail message
  | Ok entries ->
      let names =
        List.filter_map
          (function
            | Project.Listed { workflow_name; _ } -> Some workflow_name
            | Project.Unparseable _ -> None)
          entries
      in
      Alcotest.(check bool) "flat hello is listed" true
        (List.mem "hello" names);
      Alcotest.(check bool) "module mod is listed" true
        (List.mem "mod" names)

let test_agents_md_and_inline_context_conflict () =
  let root = make_temp_root () in
  write_file
    (Filename.concat root ".jig/workflows/mod/workflow.yaml")
    "name: mod\ncontext: inline\nsteps:\n  - skill: say-hi\n";
  write_file (Filename.concat root ".jig/workflows/mod/AGENTS.md") "file\n";
  match
    Workflow.load ~path:(Filename.concat root ".jig/workflows/mod/workflow.yaml")
  with
  | Ok _ -> Alcotest.fail "expected a conflict error"
  | Error message ->
      Alcotest.(check bool) "names the double source" true
        (contains ~affix:"AGENTS.md" message)

let write_notify_config root ~notify_command =
  write_file
    (Filename.concat root ".jig/config.yaml")
    (Printf.sprintf
       "harness:\n  - irrelevant\nnotify:\n  - sh\n  - -c\n  - %S\n"
       notify_command)

let test_notify_fires_when_run_stops () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  write_notify_config root
    ~notify_command:"printf %s {run_id} > notified-{status}";
  Scripted_executor.reset ~outputs:[];
  (match
     Scripted_runner.execute_run ~root ~workflow_name:"hello" ~task:"x"
       ~isolated:false ()
   with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      let marker = Filename.concat root "notified-completed" in
      Alcotest.(check bool) "notify fired on completion" true
        (Sys.file_exists marker);
      Alcotest.(check string) "run id substituted" run.Run.id
        (String.trim (In_channel.with_open_text marker In_channel.input_all)));
  Scripted_executor.reset
    ~outputs:[ handoff_block ~status:"escalate" ~summary:"need you" () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"hello" ~task:"x"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok _ ->
      Alcotest.(check bool) "notify fired on pause" true
        (Sys.file_exists (Filename.concat root "notified-paused"))

let test_notify_failure_is_harmless () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  write_notify_config root ~notify_command:"exit 1";
  Scripted_executor.reset ~outputs:[];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"hello" ~task:"x"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "run outcome unaffected by failing notify"
        "completed"
        (Run.string_of_status run.Run.status)

let test_execute_run_honors_given_run_id () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  Scripted_executor.reset ~outputs:[];
  match
    Scripted_runner.execute_run ~run_id:"pre-issued-id" ~root
      ~workflow_name:"hello" ~task:"say hi" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "run carries the pre-issued id" "pre-issued-id"
        run.Run.id;
      Alcotest.(check bool) "record lands under the pre-issued id" true
        (Sys.file_exists
           (Filename.concat root ".jig/runs/pre-issued-id.json"))

let index_of ~affix text =
  let affix_length = String.length affix in
  let text_length = String.length text in
  let rec scan i =
    if i + affix_length > text_length then None
    else if String.sub text i affix_length = affix then Some i
    else scan (i + 1)
  in
  scan 0

let setup_inputs_project root =
  setup_project root ~harness:[ "irrelevant" ];
  write_file
    (Filename.concat root ".jig/workflows/bound.yaml")
    "name: bound\n\
     steps:\n\
    \  - skill: one\n\
    \  - skill: two\n\
    \    with:\n\
    \      spec: docs/spec.md\n";
  List.iter (fun name -> add_skill root name ("# " ^ name ^ "\n")) [ "one"; "two" ]

let test_prompt_carries_step_inputs () =
  let root = make_temp_root () in
  setup_inputs_project root;
  Scripted_executor.reset
    ~outputs:[ handoff_block ~summary:"first done" (); handoff_block () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"bound" ~task:"t"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok _ -> (
      match Scripted_executor.prompts () with
      | [ first; second ] -> (
          Alcotest.(check bool) "unbound step has no inputs section" false
            (contains ~affix:"Step inputs" first);
          Alcotest.(check bool) "bound step carries the section" true
            (contains
               ~affix:
                 "Step inputs (bound by the workflow; treat each value as a \
                  literal):"
               second);
          Alcotest.(check bool) "bound step carries the value" true
            (contains ~affix:"spec: docs/spec.md" second);
          match
            (index_of ~affix:"Step inputs" second,
             index_of ~affix:"Previous handoff" second)
          with
          | Some inputs_at, Some handoff_at ->
              Alcotest.(check bool) "inputs render before the handoff" true
                (inputs_at < handoff_at)
          | _ -> Alcotest.fail "expected both sections in the second prompt")
      | prompts ->
          Alcotest.fail
            (Printf.sprintf "expected 2 prompts, got %d" (List.length prompts)))

let test_retry_steps_carry_their_own_inputs () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  write_file
    (Filename.concat root ".jig/workflows/looped.yaml")
    "name: looped\n\
     steps:\n\
    \  - retry:\n\
    \      max_iterations: 2\n\
    \      on_exhausted: abort\n\
    \      steps:\n\
    \        - skill: one\n\
    \          with:\n\
    \            target: alpha\n\
    \        - skill: two\n\
    \          until: pass\n\
    \          with:\n\
    \            target: beta\n";
  List.iter (fun name -> add_skill root name ("# " ^ name ^ "\n")) [ "one"; "two" ];
  Scripted_executor.reset ~outputs:[ handoff_block (); handoff_block () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"looped" ~task:"t"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok _ -> (
      match Scripted_executor.prompts () with
      | [ first; second ] ->
          Alcotest.(check bool) "first sees its own value" true
            (contains ~affix:"target: alpha" first);
          Alcotest.(check bool) "first does not see the second's" false
            (contains ~affix:"target: beta" first);
          Alcotest.(check bool) "second sees its own value" true
            (contains ~affix:"target: beta" second)
      | prompts ->
          Alcotest.fail
            (Printf.sprintf "expected 2 prompts, got %d" (List.length prompts)))


(* forEach at runtime *)

let fanout_workflow_yaml =
  "name: fanout\n\
   steps:\n\
  \  - forEach:\n\
  \      items: ports.tsv\n\
  \      as: port\n\
  \      steps:\n\
  \        - skill: port-one\n\
  \          with:\n\
  \            spec: \"{{ port.spec }}\"\n\
  \        - skill: port-two\n"

let setup_fanout_project ?(items = "name\tspec\nalpha\tspecs/a.md\nbeta\tspecs/b.md\n")
    ?(workflow = fanout_workflow_yaml) root =
  setup_project root ~harness:[ "irrelevant" ];
  write_file (Filename.concat root ".jig/workflows/ports.tsv") items;
  write_file (Filename.concat root ".jig/workflows/fanout.yaml") workflow;
  List.iter
    (fun name -> add_skill root name ("# " ^ name ^ "\n"))
    [ "port-one"; "port-two" ]

let step_item_keys run =
  List.map (fun step -> step.Run.item_key) run.Run.steps

let test_lifecycle_events_fire_across_step_kinds () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  write_file
    (Filename.concat root ".jig/workflows/ports.tsv")
    "name\tspec\nalpha\ta.md\nbeta\tb.md\n";
  write_file
    (Filename.concat root ".jig/workflows/mixed.yaml")
    "name: mixed\n\
     steps:\n\
    \  - skill: one\n\
    \  - retry:\n\
    \      max_iterations: 2\n\
    \      on_exhausted: escalate\n\
    \      steps:\n\
    \        - skill: two\n\
    \          until: pass\n\
    \  - forEach:\n\
    \      items: ports.tsv\n\
    \      as: port\n\
    \      steps:\n\
    \        - skill: three\n";
  List.iter
    (fun name -> add_skill root name ("# " ^ name ^ "\n"))
    [ "one"; "two"; "three" ];
  Scripted_executor.reset
    ~outputs:
      [ handoff_block (); handoff_block (); handoff_block (); handoff_block () ];
  let events = ref [] in
  let on_event e = events := e :: !events in
  match
    Scripted_runner.execute_run ~on_event ~root ~workflow_name:"mixed"
      ~task:"t" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok _ ->
      let seq =
        List.rev_map
          (function
            | Runner.Step_started { skill; item_key; _ } ->
                Printf.sprintf "start:%s:%s" skill
                  (Option.value item_key ~default:"-")
            | Runner.Items_resolved { item_keys; _ } ->
                "items:" ^ String.concat "," item_keys
            | Runner.Step_finished record ->
                Printf.sprintf "finish:%s:%s" record.Run.skill
                  (Option.value record.Run.item_key ~default:"-"))
          !events
      in
      Alcotest.(check (list string))
        "started/finished per step, items before the forEach body, item keys \
         on forEach steps"
        [
          "start:one:-";
          "finish:one:-";
          "start:two:-";
          "finish:two:-";
          "items:alpha,beta";
          "start:three:alpha";
          "finish:three:alpha";
          "start:three:beta";
          "finish:three:beta";
        ]
        seq

let test_for_each_runs_body_per_item () =
  let root = make_temp_root () in
  setup_fanout_project root;
  Scripted_executor.reset
    ~outputs:
      [ handoff_block (); handoff_block (); handoff_block (); handoff_block () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"fanout" ~task:"port"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      Alcotest.(check string) "status" "completed"
        (Run.string_of_status run.Run.status);
      Alcotest.(check (list (option string)))
        "step provenance carries the item keys"
        [ Some "alpha"; Some "alpha"; Some "beta"; Some "beta" ]
        (step_item_keys run);
      (match Yojson.Safe.Util.member "for_each"
               (Yojson.Safe.Util.member "position" (Run.to_json run))
       with
      | `Null -> ()
      | _ -> Alcotest.fail "completed run must carry no snapshot");
      match Scripted_executor.prompts () with
      | [ first; _; third; _ ] ->
          Alcotest.(check bool) "first prompt binds item 1's value" true
            (contains ~affix:"spec: specs/a.md" first);
          Alcotest.(check bool) "first prompt states the item position" true
            (contains ~affix:"item 1 of 2 (alpha)" first);
          Alcotest.(check bool) "third prompt binds item 2's value" true
            (contains ~affix:"spec: specs/b.md" third);
          Alcotest.(check bool) "third prompt states the item position" true
            (contains ~affix:"item 2 of 2 (beta)" third)
      | prompts ->
          Alcotest.fail
            (Printf.sprintf "expected 4 prompts, got %d" (List.length prompts)))

let retry_fanout_workflow_yaml =
  "name: fanout\n\
   steps:\n\
  \  - forEach:\n\
  \      items: ports.tsv\n\
  \      as: port\n\
  \      steps:\n\
  \        - retry:\n\
  \            max_iterations: 2\n\
  \            on_exhausted: escalate\n\
  \            steps:\n\
  \              - skill: port-one\n\
  \                with:\n\
  \                  spec: \"{{ port.spec }}\"\n\
  \              - skill: port-two\n\
  \                until: pass\n"

let test_for_each_isolates_handoffs_between_items () =
  let root = make_temp_root () in
  setup_fanout_project root;
  Scripted_executor.reset
    ~outputs:
      [
        handoff_block ~summary:"alpha-one done" ();
        handoff_block ~summary:"alpha-two done" ();
        handoff_block ~summary:"beta-one done" ();
        handoff_block ~summary:"beta-two done" ();
      ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"fanout" ~task:"port"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok _ -> (
      match Scripted_executor.prompts () with
      | [ _alpha_one; alpha_two; beta_one; _beta_two ] ->
          Alcotest.(check bool)
            "within item 1, step 2 sees step 1's handoff" true
            (contains ~affix:"alpha-one done" alpha_two);
          Alcotest.(check bool)
            "item 2's first step does NOT see item 1's handoff" false
            (contains ~affix:"alpha-two done" beta_one);
          Alcotest.(check bool)
            "item 2's first step has no previous handoff" false
            (contains ~affix:"Previous handoff" beta_one)
      | prompts ->
          Alcotest.fail
            (Printf.sprintf "expected 4 prompts, got %d" (List.length prompts)))

let test_for_each_retry_loops_within_one_item () =
  let root = make_temp_root () in
  setup_fanout_project ~workflow:retry_fanout_workflow_yaml root;
  Scripted_executor.reset
    ~outputs:
      [
        (* item 1, attempt 1: implement passes, gate fails *)
        handoff_block ();
        handoff_block ~status:"fail" ~summary:"gate red" ();
        (* item 1, attempt 2: implement passes, gate passes *)
        handoff_block ();
        handoff_block ();
        (* item 2, one attempt *)
        handoff_block ();
        handoff_block ();
      ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"fanout" ~task:"port"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "status" "completed"
        (Run.string_of_status run.Run.status);
      Alcotest.(check (list (option string)))
        "the retry looped within item 1 before item 2 ran"
        [
          Some "alpha";
          Some "alpha";
          Some "alpha";
          Some "alpha";
          Some "beta";
          Some "beta";
        ]
        (step_item_keys run)

let pause_fanout_at_item_two root =
  setup_fanout_project root;
  Scripted_executor.reset
    ~outputs:
      [
        handoff_block ();
        handoff_block ();
        handoff_block ~status:"escalate" ~summary:"stuck on beta" ();
      ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"fanout" ~task:"port"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> run

let test_for_each_escalate_pauses_at_the_item () =
  let root = make_temp_root () in
  let run = pause_fanout_at_item_two root in
  Alcotest.(check string) "status" "paused"
    (Run.string_of_status run.Run.status);
  let position = Yojson.Safe.Util.member "position" (Run.to_json run) in
  let state = Yojson.Safe.Util.member "for_each" position in
  Alcotest.(check int) "paused at item 2" 1
    (Yojson.Safe.Util.to_int (Yojson.Safe.Util.member "item_index" state));
  Alcotest.(check int) "paused at the first body step" 0
    (Yojson.Safe.Util.to_int (Yojson.Safe.Util.member "body_index" state))

let test_for_each_resume_uses_the_snapshot () =
  let root = make_temp_root () in
  let paused = pause_fanout_at_item_two root in
  (* Edit the items file while paused: the run must not notice. *)
  write_file
    (Filename.concat root ".jig/workflows/ports.tsv")
    "name\tspec\nalpha\tspecs/a.md\nbeta\tspecs/CHANGED.md\n";
  Scripted_executor.reset ~outputs:[ handoff_block (); handoff_block () ];
  match
    Scripted_runner.resume_run ~root ~run_id:paused.Run.id ~guidance:None ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      Alcotest.(check string) "status" "completed"
        (Run.string_of_status run.Run.status);
      match Scripted_executor.prompts () with
      | first :: _ ->
          Alcotest.(check bool) "resumed at item 2" true
            (contains ~affix:"item 2 of 2 (beta)" first);
          Alcotest.(check bool) "snapshot value survives the file edit" true
            (contains ~affix:"spec: specs/b.md" first);
          Alcotest.(check bool) "edited value is not seen" false
            (contains ~affix:"CHANGED" first)
      | [] -> Alcotest.fail "expected the resume to execute steps")

let test_for_each_skip_completes_the_item_step_only () =
  let root = make_temp_root () in
  let paused = pause_fanout_at_item_two root in
  Scripted_executor.reset ~outputs:[ handoff_block () ];
  match
    Scripted_runner.resume_run ~root ~run_id:paused.Run.id ~guidance:None
      ~skip:true ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      Alcotest.(check string) "status" "completed"
        (Run.string_of_status run.Run.status);
      let skipped = List.nth run.Run.steps 3 in
      Alcotest.(check string) "synthetic pass names the paused skill"
        "port-one" skipped.Run.skill;
      Alcotest.(check (option string))
        "synthetic pass carries the item key" (Some "beta")
        skipped.Run.item_key;
      match Scripted_executor.prompts () with
      | [ only ] ->
          Alcotest.(check bool)
            "automation continued with the item's next step" true
            (contains ~affix:"item 2 of 2 (beta)" only)
      | prompts ->
          Alcotest.fail
            (Printf.sprintf "expected 1 prompt, got %d" (List.length prompts)))

let test_for_each_missing_column_fails_before_any_step () =
  let root = make_temp_root () in
  setup_fanout_project ~items:"name\nalpha\nbeta\n" root;
  Scripted_executor.reset ~outputs:[];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"fanout" ~task:"port"
      ~isolated:false ()
  with
  | Ok _ -> Alcotest.fail "expected validation to fail"
  | Error message ->
      Alcotest.(check bool) "names the missing column" true
        (contains ~affix:"spec" message);
      Alcotest.(check int) "no step executed" 0
        (List.length (Scripted_executor.prompts ()))

let test_for_each_missing_items_file_fails_at_entry () =
  let root = make_temp_root () in
  setup_fanout_project root;
  Sys.remove (Filename.concat root ".jig/workflows/ports.tsv");
  Scripted_executor.reset ~outputs:[];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"fanout" ~task:"port"
      ~isolated:false ()
  with
  | Ok _ -> Alcotest.fail "expected the run to fail"
  | Error message ->
      Alcotest.(check bool) "names the items file" true
        (contains ~affix:"ports.tsv" message);
      Alcotest.(check int) "no body step ran" 0
        (List.length (Scripted_executor.prompts ()))

(* An absent items file is not an up-front validation error: a producing
   step may create it before the forEach entry. *)
let test_validate_defers_absent_items () =
  let root = make_temp_root () in
  setup_fanout_project root;
  Sys.remove (Filename.concat root ".jig/workflows/ports.tsv");
  let jig_dir = Filename.concat root ".jig" in
  match Project.resolve_workflow ~jig_dir ~name:"fanout" with
  | Error message -> Alcotest.fail message
  | Ok (path, workflow_dir) -> (
      match Workflow.load ~path with
      | Error message -> Alcotest.fail message
      | Ok workflow -> (
          match
            Validate.workflow ~workflow_dir ~jig_dir ~skill_paths:[] workflow
          with
          | Ok () -> ()
          | Error message ->
              Alcotest.fail
                (Printf.sprintf "expected deferral, got: %s" message)))

let test_run_json_roundtrips_for_each_position () =
  let bindings = [ ("name", "alpha"); ("spec", "specs/a.md") ] in
  let run =
    {
      Run.id = "roundtrip";
      workflow = "fanout";
      task = "port";
      status = Run.Paused;
      error = None;
      position =
        {
          Run.entry_index = 0;
          iterations_used = 1;
          for_each =
            Some { Run.item_index = 1; body_index = 0; items = [ bindings ] };
        };
      workspace = None;
      steps =
        [
          {
            Run.skill = "port-one";
            outcome = Run.Pass;
            exit_code = 0;
            cost = Metering.Unknown_cost;
            stdout = "";
            stderr = "";
            handoff = None;
            handoff_error = None;
            session_id = None;
            item_index = Some 1;
            item_key = Some "alpha";
            started_at = "";
            finished_at = "";
          };
        ];
      started_at = "";
      finished_at = None;
    }
  in
  match Run.of_json (Run.to_json run) with
  | Error message -> Alcotest.fail message
  | Ok loaded -> (
      (match loaded.Run.position.Run.for_each with
      | Some state ->
          Alcotest.(check int) "item cursor" 1 state.Run.item_index;
          Alcotest.(check int) "body cursor" 0 state.Run.body_index;
          Alcotest.(check bool) "snapshot round-trips" true
            (state.Run.items = [ bindings ])
      | None -> Alcotest.fail "expected the for_each state to survive");
      let step = List.hd loaded.Run.steps in
      Alcotest.(check (option int)) "step item_index" (Some 1)
        step.Run.item_index;
      Alcotest.(check (option string))
        "step item_key" (Some "alpha") step.Run.item_key)

let test_old_run_json_still_loads () =
  let json =
    Yojson.Safe.from_string
      {|{"id": "old", "workflow": "hello", "task": "t", "status": "paused",
         "position": {"entry_index": 1, "iterations_used": 0},
         "steps": [{"skill": "say-hi", "outcome": "pass", "exit_code": 0,
                    "cost_usd": "unknown", "stdout": "", "stderr": "",
                    "started_at": "", "finished_at": ""}],
         "started_at": ""}|}
  in
  match Run.of_json json with
  | Error message -> Alcotest.fail message
  | Ok run ->
      Alcotest.(check bool) "no for_each state" true
        (run.Run.position.Run.for_each = None);
      let step = List.hd run.Run.steps in
      Alcotest.(check (option int)) "no item_index" None step.Run.item_index;
      Alcotest.(check (option string)) "no item_key" None step.Run.item_key

let test_context_renders_into_every_prompt () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  write_file
    (Filename.concat root ".jig/workflows/ports.tsv")
    "name\tspec\nalpha\ta.md\nbeta\tb.md\n";
  write_file
    (Filename.concat root ".jig/workflows/framed.yaml")
    "name: framed\n\
     context: |\n\
    \  invariant framing here\n\
     steps:\n\
    \  - skill: one\n\
    \  - forEach:\n\
    \      items: ports.tsv\n\
    \      as: port\n\
    \      steps:\n\
    \        - skill: two\n";
  List.iter (fun name -> add_skill root name ("# " ^ name ^ "\n")) [ "one"; "two" ];
  Scripted_executor.reset
    ~outputs:[ handoff_block (); handoff_block (); handoff_block () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"framed" ~task:"t"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok _ ->
      let prompts = Scripted_executor.prompts () in
      Alcotest.(check int) "one plain step plus two items" 3
        (List.length prompts);
      List.iter
        (fun prompt ->
          Alcotest.(check bool) "every prompt carries the context preamble"
            true
            (contains ~affix:"Context:\ninvariant framing here" prompt))
        prompts

let test_handoffs_thread_between_steps () =
  let root = make_temp_root () in
  setup_three_step_project root;
  Scripted_executor.reset
    ~outputs:
      [
        handoff_block ~summary:"first step done" ();
        handoff_block ~summary:"second step done" ();
        handoff_block ~summary:"third step done" ();
      ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      Alcotest.(check string) "status" "completed"
        (Run.string_of_status run.Run.status);
      Alcotest.(check int) "three steps ran" 3 (List.length run.Run.steps);
      match Scripted_executor.prompts () with
      | [ first; second; third ] ->
          Alcotest.(check bool) "first step has no previous handoff" false
            (contains ~affix:"Previous handoff" first);
          Alcotest.(check bool) "second step sees first handoff" true
            (contains ~affix:"first step done" second);
          Alcotest.(check bool) "third step sees second handoff" true
            (contains ~affix:"second step done" third);
          Alcotest.(check bool) "third step does not see first handoff" false
            (contains ~affix:"first step done" third)
      | prompts ->
          Alcotest.fail
            (Printf.sprintf "expected 3 prompts, got %d" (List.length prompts)))

let test_prompt_states_workflow_position () =
  let root = make_temp_root () in
  setup_three_step_project root;
  Scripted_executor.reset
    ~outputs:[ handoff_block (); handoff_block (); handoff_block () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok _ -> (
      match Scripted_executor.prompts () with
      | [ first; second; third ] ->
          Alcotest.(check bool) "first step knows its position" true
            (contains ~affix:"Workflow: pipeline - step 1 of 3 (one); next: two."
               first);
          Alcotest.(check bool) "second step names its successor" true
            (contains ~affix:"step 2 of 3 (two); next: three." second);
          Alcotest.(check bool) "final step knows it is last" true
            (contains ~affix:"step 3 of 3 (three); this is the final step."
               third)
      | prompts ->
          Alcotest.fail
            (Printf.sprintf "expected 3 prompts, got %d" (List.length prompts)))

let test_retry_prompt_states_group_position () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  write_file
    (Filename.concat root ".jig/workflows/gated.yaml")
    "name: gated\n\
     steps:\n\
    \  - retry:\n\
    \      max_iterations: 2\n\
    \      on_exhausted: escalate\n\
    \      steps:\n\
    \        - skill: implement-fix\n\
    \        - skill: run-tests\n\
    \          until: pass\n\
    \  - skill: open-pr\n";
  List.iter
    (fun name -> add_skill root name ("# " ^ name ^ "\n"))
    [ "implement-fix"; "run-tests"; "open-pr" ];
  Scripted_executor.reset
    ~outputs:[ handoff_block (); handoff_block (); handoff_block () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"gated" ~task:"go"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok _ -> (
      match Scripted_executor.prompts () with
      | [ group_first; group_gate; last ] ->
          Alcotest.(check bool) "group step carries the entry position" true
            (contains ~affix:"step 1 of 2 (implement-fix); next: open-pr."
               group_first);
          Alcotest.(check bool) "gate step shares the entry position" true
            (contains ~affix:"step 1 of 2 (run-tests); next: open-pr."
               group_gate);
          Alcotest.(check bool) "entry after the group is final" true
            (contains ~affix:"step 2 of 2 (open-pr); this is the final step."
               last)
      | prompts ->
          Alcotest.fail
            (Printf.sprintf "expected 3 prompts, got %d" (List.length prompts)))

let test_prompt_carries_handoff_protocol () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  Scripted_executor.reset ~outputs:[];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"hello" ~task:"x"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok _ -> (
      match Scripted_executor.prompts () with
      | [ prompt ] ->
          Alcotest.(check bool) "protocol present" true
            (contains ~affix:"end your reply with a fenced handoff block"
               prompt);
          Alcotest.(check bool)
            "example status cannot parse as a real handoff" true
            (contains ~affix:"status: <pass | fail | escalate>" prompt);
          Alcotest.(check bool) "workspace ownership stated" true
            (contains ~affix:"the runner owns isolation" prompt)
      | prompts ->
          Alcotest.fail
            (Printf.sprintf "expected 1 prompt, got %d" (List.length prompts)))

let test_fail_handoff_short_circuits () =
  let root = make_temp_root () in
  setup_three_step_project root;
  Scripted_executor.reset
    ~outputs:[ handoff_block ~status:"fail" ~summary:"cannot reproduce" () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "status" "failed"
        (Run.string_of_status run.Run.status);
      Alcotest.(check int) "only one step ran" 1 (List.length run.Run.steps);
      Alcotest.(check int) "no further harness calls" 1
        (List.length (Scripted_executor.prompts ()))

let test_escalate_handoff_pauses_run () =
  let root = make_temp_root () in
  setup_three_step_project root;
  Scripted_executor.reset
    ~outputs:[ handoff_block ~status:"escalate" ~summary:"need a human" () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "status" "paused"
        (Run.string_of_status run.Run.status);
      Alcotest.(check int) "run stopped at the escalating step" 1
        (List.length run.Run.steps);
      Alcotest.(check bool) "paused runs have no finished_at" true
        (run.Run.finished_at = None)

let test_skip_marks_paused_entry_done () =
  let root = make_temp_root () in
  setup_three_step_project root;
  Scripted_executor.reset
    ~outputs:[ handoff_block ~status:"escalate" ~summary:"stuck on one" () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (paused_run, _) -> (
      Scripted_executor.reset
        ~outputs:
          [
            handoff_block ~summary:"two done" ();
            handoff_block ~summary:"three done" ();
          ];
      match
        Scripted_runner.resume_run ~root ~run_id:paused_run.Run.id
          ~guidance:(Some "wired it up by hand") ~skip:true ()
      with
      | Error message -> Alcotest.fail message
      | Ok (run, _) -> (
          Alcotest.(check string) "completed" "completed"
            (Run.string_of_status run.Run.status);
          Alcotest.(check int)
            "escalated, human-skipped, then two agent steps" 4
            (List.length run.Run.steps);
          let skipped = List.nth run.Run.steps 1 in
          Alcotest.(check string) "skipped record names the paused skill"
            "one" skipped.Run.skill;
          Alcotest.(check bool) "no cost on the human pass" true
            (skipped.Run.cost = Metering.Unknown_cost);
          Alcotest.(check string) "skipped record is empty of output" ""
            skipped.Run.stdout;
          match Scripted_executor.prompts () with
          | first_after_skip :: _ ->
              Alcotest.(check bool) "human handoff threads onward" true
                (contains ~affix:"A human completed this step manually."
                   first_after_skip);
              Alcotest.(check bool) "guidance text rides in the summary" true
                (contains ~affix:"wired it up by hand" first_after_skip)
          | [] -> Alcotest.fail "expected prompts after the skip"))

let setup_retry_project root ~max_iterations ~on_exhausted =
  setup_project root ~harness:[ "irrelevant" ];
  write_file
    (Filename.concat root ".jig/workflows/fixloop.yaml")
    (Printf.sprintf
       "name: fixloop\n\
        steps:\n\
       \  - retry:\n\
       \      max_iterations: %d\n\
       \      on_exhausted: %s\n\
       \      steps:\n\
       \        - skill: implement-fix\n\
       \        - skill: run-tests\n\
       \          until: pass\n"
       max_iterations on_exhausted);
  List.iter
    (fun name -> add_skill root name ("# " ^ name ^ "\n"))
    [ "implement-fix"; "run-tests" ]

let test_retry_until_pass () =
  let root = make_temp_root () in
  setup_retry_project root ~max_iterations:3 ~on_exhausted:"escalate";
  Scripted_executor.reset
    ~outputs:
      [
        handoff_block ~summary:"first fix attempt" ();
        handoff_block ~status:"fail" ~summary:"tests still red" ();
        handoff_block ~summary:"second fix attempt" ();
        handoff_block ~summary:"tests green" ();
      ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"fixloop" ~task:"fix it" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "status" "completed"
        (Run.string_of_status run.Run.status);
      Alcotest.(check int) "two iterations, four steps" 4
        (List.length run.Run.steps);
      let third_prompt = List.nth (Scripted_executor.prompts ()) 2 in
      Alcotest.(check bool)
        "second iteration sees the failing handoff" true
        (contains ~affix:"tests still red" third_prompt)

let test_skip_advances_past_retry_group () =
  let root = make_temp_root () in
  setup_retry_project root ~max_iterations:2 ~on_exhausted:"escalate";
  Scripted_executor.reset
    ~outputs:
      [
        handoff_block ();
        handoff_block ~status:"fail" ();
        handoff_block ();
        handoff_block ~status:"fail" ();
      ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"fixloop" ~task:"fix it"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (paused_run, _) -> (
      Scripted_executor.reset ~outputs:[];
      match
        Scripted_runner.resume_run ~root ~run_id:paused_run.Run.id
          ~guidance:None ~skip:true ()
      with
      | Error message -> Alcotest.fail message
      | Ok (run, _) ->
          Alcotest.(check string) "whole group marked done" "completed"
            (Run.string_of_status run.Run.status);
          Alcotest.(check int) "no further harness calls" 0
            (List.length (Scripted_executor.prompts ()));
          let skipped = List.nth run.Run.steps 4 in
          Alcotest.(check string)
            "skipped record names the group's gate skill" "run-tests"
            skipped.Run.skill)

let test_invalid_handoff_text_threads_into_retry () =
  let root = make_temp_root () in
  setup_retry_project root ~max_iterations:3 ~on_exhausted:"escalate";
  Scripted_executor.reset
    ~outputs:
      [
        "did some work but still wiring the component test";
        handoff_block ~summary:"resumed from partial work" ();
        handoff_block ~summary:"tests green" ();
      ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"fixloop" ~task:"fix it"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      Alcotest.(check string) "status" "completed"
        (Run.string_of_status run.Run.status);
      match Scripted_executor.prompts () with
      | [ _; retried; after_valid ] ->
          Alcotest.(check bool) "retry is told the block was missing" true
            (contains ~affix:"without a handoff block" retried);
          Alcotest.(check bool) "retry sees the invalid attempt's text" true
            (contains ~affix:"still wiring the component test" retried);
          Alcotest.(check bool) "valid handoff supersedes degraded text" false
            (contains ~affix:"still wiring the component test" after_valid);
          Alcotest.(check bool) "valid handoff threads normally" true
            (contains ~affix:"resumed from partial work" after_valid)
      | prompts ->
          Alcotest.fail
            (Printf.sprintf "expected 3 prompts, got %d" (List.length prompts)))

let test_retry_exhausted_escalates () =
  let root = make_temp_root () in
  setup_retry_project root ~max_iterations:2 ~on_exhausted:"escalate";
  Scripted_executor.reset
    ~outputs:
      [
        handoff_block ();
        handoff_block ~status:"fail" ();
        handoff_block ();
        handoff_block ~status:"fail" ();
      ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"fixloop" ~task:"fix it" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "status" "paused"
        (Run.string_of_status run.Run.status);
      Alcotest.(check int) "both iterations recorded" 4
        (List.length run.Run.steps);
      Alcotest.(check int) "iterations used persisted" 2
        run.Run.position.Run.iterations_used

let test_retry_exhausted_aborts () =
  let root = make_temp_root () in
  setup_retry_project root ~max_iterations:1 ~on_exhausted:"abort";
  Scripted_executor.reset
    ~outputs:[ handoff_block (); handoff_block ~status:"fail" () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"fixloop" ~task:"fix it" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "status" "aborted"
        (Run.string_of_status run.Run.status)

let setup_on_fail_project root ~on_fail =
  setup_project root ~harness:[ "irrelevant" ];
  write_file
    (Filename.concat root ".jig/workflows/guarded.yaml")
    (Printf.sprintf
       "name: guarded\nsteps:\n  - skill: fragile\n    on_fail: %s\n" on_fail);
  add_skill root "fragile" "# fragile\n"

let test_on_fail_escalate_pauses () =
  let root = make_temp_root () in
  setup_on_fail_project root ~on_fail:"escalate";
  Scripted_executor.reset
    ~outputs:[ handoff_block ~status:"fail" ~summary:"cannot proceed" () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"guarded" ~task:"x" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "status" "paused"
        (Run.string_of_status run.Run.status)

let test_on_fail_abort_aborts () =
  let root = make_temp_root () in
  setup_on_fail_project root ~on_fail:"abort";
  Scripted_executor.reset ~outputs:[ handoff_block ~status:"fail" () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"guarded" ~task:"x" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "status" "aborted"
        (Run.string_of_status run.Run.status)

let test_resume_continues_from_paused_step () =
  let root = make_temp_root () in
  setup_three_step_project root;
  Scripted_executor.reset
    ~outputs:
      [
        handoff_block ~summary:"one done" ();
        handoff_block ~status:"escalate" ~summary:"stuck on two" ();
      ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (paused_run, _) -> (
      Alcotest.(check string) "paused" "paused"
        (Run.string_of_status paused_run.Run.status);
      Scripted_executor.reset
        ~outputs:
          [
            handoff_block ~summary:"two done after help" ();
            handoff_block ~summary:"three done" ();
          ];
      match
        Scripted_runner.resume_run ~root ~run_id:paused_run.Run.id
          ~guidance:(Some "focus on the flaky assertion") ()
      with
      | Error message -> Alcotest.fail message
      | Ok (run, _) -> (
          Alcotest.(check string) "completed after resume" "completed"
            (Run.string_of_status run.Run.status);
          Alcotest.(check int) "four steps total" 4
            (List.length run.Run.steps);
          match Scripted_executor.prompts () with
          | first_resumed :: second_resumed :: _ ->
              Alcotest.(check bool) "guidance in first resumed prompt" true
                (contains ~affix:"focus on the flaky assertion" first_resumed);
              Alcotest.(check bool) "escalating handoff threaded" true
                (contains ~affix:"stuck on two" first_resumed);
              Alcotest.(check bool) "guidance consumed after one step" false
                (contains ~affix:"focus on the flaky assertion" second_resumed)
          | prompts ->
              Alcotest.fail
                (Printf.sprintf "expected 2 resumed prompts, got %d"
                   (List.length prompts))))

let test_resume_refuses_completed_runs () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  Scripted_executor.reset ~outputs:[];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"hello" ~task:"x" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      match
        Scripted_runner.resume_run ~root ~run_id:run.Run.id ~guidance:None ()
      with
      | Ok _ -> Alcotest.fail "expected resume of a completed run to fail"
      | Error message ->
          Alcotest.(check bool) "names the status" true
            (contains ~affix:"completed" message))

let test_record_persisted_incrementally () =
  let root = make_temp_root () in
  setup_three_step_project root;
  Scripted_executor.reset
    ~outputs:[ handoff_block ~status:"escalate" ~summary:"early stop" () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      let runs_dir = Project.runs_dir ~root in
      match Store.Filesystem.load ~runs_dir ~id:run.Run.id with
      | Error message -> Alcotest.fail message
      | Ok loaded ->
          Alcotest.(check string) "paused state persisted" "paused"
            (Run.string_of_status loaded.Run.status);
          Alcotest.(check int) "position persisted" 0
            loaded.Run.position.Run.entry_index)

let test_list_workflows_reports_valid_and_invalid () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  write_file
    (Filename.concat root ".jig/workflows/two.yaml")
    "name: two\nsteps:\n  - skill: a\n  - skill: b\n";
  write_file
    (Filename.concat root ".jig/workflows/broken.yaml")
    "name: broken\nbogus: true\nsteps:\n  - skill: a\n";
  match Project.list_workflows ~jig_dir:(Filename.concat root ".jig") with
  | Error message -> Alcotest.fail message
  | Ok entries ->
      let names =
        List.filter_map
          (function
            | Project.Listed listing -> Some listing.Project.workflow_name
            | Project.Unparseable _ -> None)
          entries
      in
      Alcotest.(check (list string)) "valid workflows" [ "hello"; "two" ] names;
      let invalid =
        List.filter_map
          (function
            | Project.Unparseable { file; _ } -> Some file
            | Project.Listed _ -> None)
          entries
      in
      Alcotest.(check (list string)) "invalid reported" [ "broken.yaml" ] invalid

let test_store_load_hints_version_on_garbage () =
  let root = make_temp_root () in
  let runs_dir = Project.runs_dir ~root in
  Unix.mkdir runs_dir 0o755;
  write_file (Filename.concat runs_dir "old-record.json") "{\"id\": 42}";
  match Store.Filesystem.load ~runs_dir ~id:"old-record" with
  | Ok _ -> Alcotest.fail "expected a parse failure"
  | Error message ->
      Alcotest.(check bool) "mentions version" true
        (contains ~affix:"different jig version" message)

let test_last_handoff_picks_latest () =
  let handoff summary =
    Some { Handoff.status = Handoff.Pass; artifacts = []; summary }
  in
  let step name h =
    {
      Run.skill = name;
      outcome = Run.Pass;
      exit_code = 0;
      cost = Metering.Unknown_cost;
      stdout = "";
      stderr = "";
      handoff = h;
      handoff_error = None;
      session_id = None;
      item_index = None;
      item_key = None;
      started_at = "";
      finished_at = "";
    }
  in
  let run =
    {
      Run.id = "x";
      workflow = "w";
      task = "t";
      status = Run.Completed;
      error = None;
      position = { Run.entry_index = 0; iterations_used = 0; for_each = None };
      workspace = None;
      steps =
        [ step "a" (handoff "first"); step "b" None; step "c" (handoff "last") ];
      started_at = "";
      finished_at = None;
    }
  in
  match Run.last_handoff run with
  | Some found -> Alcotest.(check string) "latest wins" "last" found.Handoff.summary
  | None -> Alcotest.fail "expected a handoff"

(* Isolation: worktree per run + config wrapper *)

let run_shell root command =
  let full = Printf.sprintf "cd %s && %s" (Filename.quote root) command in
  Alcotest.(check int) (Printf.sprintf "shell ok: %s" command) 0 (Sys.command full)

let setup_git_project root ~harness =
  setup_project root ~harness;
  run_shell root
    "git init -q && git config user.email jig@test && git config user.name \
     jig && git add -A && git commit -qm fixture"

let escalate_then_touch_harness =
  (* Touches a marker in its cwd, then escalates - proves where steps run
     and leaves the run paused so the worktree must survive. *)
  [
    "sh";
    "-c";
    "touch marker-from-step; printf '```handoff\\nstatus: escalate\\nsummary: \
     checkpoint\\n```\\n'";
  ]

let test_config_wrapper_prepends () =
  match
    Config.of_string "harness:\n  - claude\n  - -p\nwrapper:\n  - srt\n"
  with
  | Error message -> Alcotest.fail message
  | Ok config -> (
      match Model_provider.Default.resolve ~config ~skill:"any" with
      | Error message -> Alcotest.fail message
      | Ok command ->
          Alcotest.(check (list string))
            "wrapper prepended"
            [ "srt"; "claude"; "-p" ]
            command)

let test_isolated_run_uses_worktree_and_resume_reuses_it () =
  let root = make_temp_root () in
  setup_git_project root ~harness:escalate_then_touch_harness;
  match
    Runner.Default.execute_run ~root ~workflow_name:"hello" ~task:"iso"
      ~isolated:true ()
  with
  | Error message -> Alcotest.fail message
  | Ok (paused_run, _) -> (
      Alcotest.(check string) "paused" "paused"
        (Run.string_of_status paused_run.Run.status);
      let workspace =
        match paused_run.Run.workspace with
        | Some path -> path
        | None -> Alcotest.fail "expected an isolated workspace"
      in
      Alcotest.(check bool) "workspace path has no git-internal segment" false
        (contains ~affix:"/.git/" workspace);
      (let sibling =
         Filename.concat (Filename.dirname root)
           (Filename.basename root ^ "-worktrees")
       in
       Alcotest.(check bool) "workspace lives in the sibling worktrees dir"
         true
         (String.length workspace > String.length sibling
         && String.sub workspace 0 (String.length sibling) = sibling));
      Alcotest.(check bool) "worktree survives the pause" true
        (Sys.file_exists workspace);
      Alcotest.(check bool) "marker written in the worktree" true
        (Sys.file_exists (Filename.concat workspace "marker-from-step"));
      Alcotest.(check bool) "original checkout untouched" false
        (Sys.file_exists (Filename.concat root "marker-from-step"));
      (* Make the harness pass now, then resume: same worktree, then cleanup. *)
      write_file
        (Filename.concat root ".jig/config.yaml")
        "harness:\n  - sh\n  - -c\n  - \"printf \
         '```handoff\\\\nstatus: pass\\\\n```\\\\n'\"\n";
      match
        Runner.Default.resume_run ~root ~run_id:paused_run.Run.id
          ~guidance:None ()
      with
      | Error message -> Alcotest.fail message
      | Ok (finished, _) ->
          Alcotest.(check string) "completed" "completed"
            (Run.string_of_status finished.Run.status);
          Alcotest.(check bool) "worktree cleaned up after completion" false
            (Sys.file_exists workspace))

let test_concurrent_isolated_runs_do_not_interfere () =
  let root = make_temp_root () in
  setup_git_project root ~harness:escalate_then_touch_harness;
  let start () =
    match
      Runner.Default.execute_run ~root ~workflow_name:"hello" ~task:"iso"
        ~isolated:true ()
    with
    | Error message -> Alcotest.fail message
    | Ok (run, _) -> run
  in
  let first = start () in
  let second = start () in
  Alcotest.(check bool) "distinct run ids" true (first.Run.id <> second.Run.id);
  let workspace run =
    match run.Run.workspace with
    | Some path -> path
    | None -> Alcotest.fail "expected a workspace"
  in
  Alcotest.(check bool) "distinct worktrees" true
    (workspace first <> workspace second);
  Alcotest.(check bool) "both worktrees alive while paused" true
    (Sys.file_exists (workspace first) && Sys.file_exists (workspace second))

let test_resume_honors_recorded_workspace_location () =
  let root = make_temp_root () in
  setup_git_project root
    ~harness:[ "sh"; "-c"; "printf '```handoff\\nstatus: pass\\n```\\n'" ];
  (* A workspace recorded under the pre-relocation layout: still resumable. *)
  let old_workspace =
    Filename.concat root ".git/jig-worktrees/legacy-run"
  in
  run_shell root
    ("git worktree add --detach " ^ Filename.quote old_workspace);
  let legacy_run =
    {
      Run.id = "legacy-run";
      workflow = "hello";
      task = "iso";
      status = Run.Paused;
      error = None;
      position = { Run.entry_index = 0; iterations_used = 0; for_each = None };
      workspace = Some old_workspace;
      steps = [];
      started_at = "2026-07-03T00:00:00Z";
      finished_at = None;
    }
  in
  (match
     Store.Filesystem.save ~runs_dir:(Project.runs_dir ~root) legacy_run
   with
  | Error message -> Alcotest.fail message
  | Ok _ -> ());
  match
    Runner.Default.resume_run ~root ~run_id:"legacy-run" ~guidance:None ()
  with
  | Error message -> Alcotest.fail message
  | Ok (finished, _) ->
      Alcotest.(check string) "completed" "completed"
        (Run.string_of_status finished.Run.status);
      Alcotest.(check bool) "legacy worktree cleaned up" false
        (Sys.file_exists old_workspace)

(* Run visibility *)

(* Snapshots the store from inside the harness call: whatever the executor
   sees is what `jig status` would report while the step is executing. *)
let probe_runs_dir = ref ""
let probe_seen : Run.t list ref = ref []

module Probe_executor : Executor.S = struct
  let execute ~command:_ ~cwd:_ ~prompt:_ =
    (match Store.Filesystem.list_runs ~runs_dir:!probe_runs_dir with
    | Ok runs -> probe_seen := runs
    | Error _ -> probe_seen := []);
    Ok { Executor.exit_code = 0; stdout = handoff_block (); stderr = "" }
end

module Probe_runner =
  Runner.Make (Probe_executor) (Model_provider.Default) (Metering.Noop)
    (Store.Filesystem)

let test_run_record_visible_during_first_step () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  probe_runs_dir := Project.runs_dir ~root;
  probe_seen := [];
  match
    Probe_runner.execute_run ~root ~workflow_name:"hello" ~task:"say hi"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok _ -> (
      match !probe_seen with
      | [ run ] ->
          Alcotest.(check string) "in-flight status" "running"
            (Run.string_of_status run.Run.status);
          Alcotest.(check int) "no steps completed yet" 0
            (List.length run.Run.steps)
      | runs ->
          Alcotest.fail
            (Printf.sprintf
               "expected the in-flight run in the store during step 1, found \
                %d runs"
               (List.length runs)))

let test_on_step_fires_per_step_in_order () =
  let root = make_temp_root () in
  setup_three_step_project root;
  Scripted_executor.reset
    ~outputs:
      [
        handoff_block ~summary:"a" ();
        handoff_block ~summary:"b" ();
        handoff_block ~summary:"c" ();
      ];
  let seen = ref [] in
  match
    Scripted_runner.execute_run
      ~on_step:(fun step -> seen := step.Run.skill :: !seen)
      ~root ~workflow_name:"pipeline" ~task:"build" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok _ ->
      Alcotest.(check (list string))
        "one callback per step, in order"
        [ "one"; "two"; "three" ]
        (List.rev !seen)

let test_list_runs_newest_first () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  Scripted_executor.reset ~outputs:[];
  let run () =
    match
      Scripted_runner.execute_run ~root ~workflow_name:"hello" ~task:"x"
        ~isolated:false ()
    with
    | Error message -> Alcotest.fail message
    | Ok (run, _) -> run.Run.id
  in
  let first = run () in
  let second = run () in
  match Store.Filesystem.list_runs ~runs_dir:(Project.runs_dir ~root) with
  | Error message -> Alcotest.fail message
  | Ok runs ->
      Alcotest.(check (list string))
        "newest first" [ second; first ]
        (List.map (fun run -> run.Run.id) runs)

let test_list_runs_empty_when_none () =
  let root = make_temp_root () in
  match Store.Filesystem.list_runs ~runs_dir:(Project.runs_dir ~root) with
  | Error message -> Alcotest.fail message
  | Ok runs -> Alcotest.(check int) "no runs" 0 (List.length runs)

(* skill_paths: resolution from external libraries *)

let write_config_with_skill_paths root ~skill_paths =
  write_file
    (Filename.concat root ".jig/config.yaml")
    (Printf.sprintf "harness:\n  - irrelevant\nskill_paths:\n%s"
       (String.concat ""
          (List.map (fun path -> Printf.sprintf "  - %S\n" path) skill_paths)))

let make_external_library () =
  let library =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "jig-library-%d-%d" (Unix.getpid ())
         (Random.int 1_000_000))
  in
  Unix.mkdir library 0o755;
  let dir = Filename.concat library "external-skill" in
  Unix.mkdir dir 0o755;
  write_file (Filename.concat dir "SKILL.md")
    "# External skill\n\nThe library version.\n";
  library

let test_external_skill_resolves_and_runs () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  let library = make_external_library () in
  write_config_with_skill_paths root ~skill_paths:[ library ];
  write_file
    (Filename.concat root ".jig/workflows/uses-external.yaml")
    "name: uses-external\nsteps:\n  - skill: external-skill\n";
  Scripted_executor.reset ~outputs:[];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"uses-external"
      ~task:"x" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      Alcotest.(check string) "completed" "completed"
        (Run.string_of_status run.Run.status);
      match Scripted_executor.prompts () with
      | [ prompt ] ->
          Alcotest.(check bool) "external skill body used verbatim" true
            (contains ~affix:"The library version." prompt)
      | _ -> Alcotest.fail "expected one prompt")

let test_repo_skill_shadows_external () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  let library = make_external_library () in
  write_config_with_skill_paths root ~skill_paths:[ library ];
  add_skill root "external-skill" "# Shadow\n\nThe repo version.\n";
  write_file
    (Filename.concat root ".jig/workflows/uses-external.yaml")
    "name: uses-external\nsteps:\n  - skill: external-skill\n";
  Scripted_executor.reset ~outputs:[];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"uses-external"
      ~task:"x" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok _ -> (
      match Scripted_executor.prompts () with
      | [ prompt ] ->
          Alcotest.(check bool) "repo version wins" true
            (contains ~affix:"The repo version." prompt);
          Alcotest.(check bool) "library version shadowed" false
            (contains ~affix:"The library version." prompt)
      | _ -> Alcotest.fail "expected one prompt")

let test_missing_skill_error_lists_searched_paths () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  let library = make_external_library () in
  write_config_with_skill_paths root ~skill_paths:[ library ];
  write_file
    (Filename.concat root ".jig/workflows/ghost.yaml")
    "name: ghost\nsteps:\n  - skill: nowhere-to-be-found\n";
  match
    Scripted_runner.execute_run ~root ~workflow_name:"ghost" ~task:"x"
      ~isolated:false ()
  with
  | Ok _ -> Alcotest.fail "expected a resolution error"
  | Error message ->
      Alcotest.(check bool) "lists the repo skills dir" true
        (contains ~affix:".jig/skills" message);
      Alcotest.(check bool) "lists the external library" true
        (contains ~affix:library message)

let test_skill_paths_expand_home () =
  let fake_home = make_temp_root () in
  let original_home = Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" fake_home;
  Fun.protect
    ~finally:(fun () ->
      match original_home with
      | Some home -> Unix.putenv "HOME" home
      | None -> ())
    (fun () ->
      match
        Config.of_string "harness:\n  - x\nskill_paths:\n  - ~/my-skills\n"
      with
      | Error message -> Alcotest.fail message
      | Ok config ->
          Alcotest.(check (list string))
            "tilde expanded"
            [ Filename.concat fake_home "my-skills" ]
            config.Config.skill_paths)

(* Init: the embedded starter set *)

let make_bare_temp_dir () =
  let root =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "jig-init-%d-%d" (Unix.getpid ()) (Random.int 1_000_000))
  in
  Unix.mkdir root 0o755;
  root

let test_init_scaffolds_valid_project () =
  let root = make_bare_temp_dir () in
  match Init.scaffold ~root ~preset:Init.Custom ~skill_paths:[] with
  | Error message -> Alcotest.fail message
  | Ok written -> (
      Alcotest.(check bool) "several files written" true
        (List.length written >= 7);
      let jig_dir = Filename.concat root ".jig" in
      match
        Result.bind
          (Workflow.load
             ~path:(Filename.concat jig_dir "workflows/bugfix.yaml"))
          (fun workflow -> Validate.workflow ~workflow_dir:(Filename.concat jig_dir "workflows") ~jig_dir ~skill_paths:[] workflow)
      with
      | Error message -> Alcotest.fail message
      | Ok () -> ())

let test_init_refuses_existing_jig_dir () =
  let root = make_bare_temp_dir () in
  Unix.mkdir (Filename.concat root ".jig") 0o755;
  match Init.scaffold ~root ~preset:Init.Custom ~skill_paths:[] with
  | Ok _ -> Alcotest.fail "expected init to refuse an existing .jig"
  | Error message ->
      Alcotest.(check bool) "names the conflict" true
        (contains ~affix:".jig" message)

let scaffolded_config root =
  In_channel.with_open_text
    (Filename.concat root ".jig/config.yaml")
    In_channel.input_all

let test_init_claude_preset_is_well_formed () =
  let root = make_bare_temp_dir () in
  match
    Init.scaffold ~root ~preset:Init.Claude
      ~skill_paths:[ "/some/library"; "/another/library" ]
  with
  | Error message -> Alcotest.fail message
  | Ok _ -> (
      match Config.of_string (scaffolded_config root) with
      | Error message -> Alcotest.fail message
      | Ok config ->
          Alcotest.(check string) "harness head" "claude"
            (List.hd config.Config.harness);
          Alcotest.(check string)
            "last element is single-valued (prompt-safe)" "json"
            (List.nth config.Config.harness
               (List.length config.Config.harness - 1));
          (let rec denies_worktree_tools = function
             | "--disallowedTools" :: tools :: _ ->
                 contains ~affix:"EnterWorktree" tools
                 && contains ~affix:"ExitWorktree" tools
             | _ :: rest -> denies_worktree_tools rest
             | [] -> false
           in
           Alcotest.(check bool) "harness worktree tools denied" true
             (denies_worktree_tools config.Config.harness));
          Alcotest.(check (list string))
            "attach reopens the step's session"
            [ "claude"; "--resume"; "{session_id}" ]
            config.Config.attach;
          Alcotest.(check (list string))
            "attach_headless collects the handoff"
            [
              "claude"; "-p"; "--resume"; "{session_id}"; "--output-format";
              "json";
            ]
            config.Config.attach_headless;
          Alcotest.(check (list string))
            "skill_paths kept in order"
            [ "/some/library"; "/another/library" ]
            config.Config.skill_paths)

let test_init_codex_preset_is_well_formed () =
  let root = make_bare_temp_dir () in
  match Init.scaffold ~root ~preset:Init.Codex ~skill_paths:[] with
  | Error message -> Alcotest.fail message
  | Ok _ -> (
      match Config.of_string (scaffolded_config root) with
      | Error message -> Alcotest.fail message
      | Ok config ->
          Alcotest.(check (list string))
            "codex exec invocation"
            [ "codex"; "exec"; "--sandbox"; "workspace-write"; "--json" ]
            config.Config.harness)

let test_init_custom_matches_template () =
  let root = make_bare_temp_dir () in
  match Init.scaffold ~root ~preset:Init.Custom ~skill_paths:[] with
  | Error message -> Alcotest.fail message
  | Ok _ ->
      Alcotest.(check string)
        "bare init writes the template config verbatim"
        (List.assoc "config.yaml" Template_data.files)
        (scaffolded_config root)

(* Metering *)

let claude_style_output ?(cost = 0.05) ?(handoff = handoff_block ()) ?session
    () =
  let session_field =
    match session with
    | Some id -> [ ("session_id", `String id) ]
    | None -> []
  in
  Yojson.Safe.to_string
    (`Assoc
       ([
          ("type", `String "result");
          ("result", `String ("all done\n\n" ^ handoff));
          ("total_cost_usd", `Float cost);
          ( "usage",
            `Assoc [ ("input_tokens", `Int 900); ("output_tokens", `Int 120) ]
          );
        ]
       @ session_field))

let pause_pipeline_at_step_one root =
  setup_three_step_project root;
  Scripted_executor.reset
    ~outputs:[ handoff_block ~status:"escalate" ~summary:"stuck on one" () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> run

let elicited_result stdout = { Executor.exit_code = 0; stdout; stderr = "" }

let test_continue_attached_pass_drives_the_run () =
  let root = make_temp_root () in
  let paused = pause_pipeline_at_step_one root in
  Scripted_executor.reset
    ~outputs:
      [
        handoff_block ~summary:"two done" ();
        handoff_block ~summary:"three done" ();
      ];
  match
    Scripted_runner.continue_attached ~root ~run_id:paused.Run.id
      ~exec_result:
        (elicited_result
           (claude_style_output
              ~handoff:(handoff_block ~summary:"resolved with the human" ())
              ~session:"sess-att" ()))
      ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      Alcotest.(check string) "run drove to completion" "completed"
        (Run.string_of_status run.Run.status);
      Alcotest.(check int) "escalated, elicited, then two agent steps" 4
        (List.length run.Run.steps);
      let elicited_step = List.nth run.Run.steps 1 in
      Alcotest.(check string) "elicited record names the paused skill" "one"
        elicited_step.Run.skill;
      Alcotest.(check (option string))
        "elicited record keeps the session id" (Some "sess-att")
        elicited_step.Run.session_id;
      match Scripted_executor.prompts () with
      | first_after :: _ ->
          Alcotest.(check bool) "elicited handoff threads onward" true
            (contains ~affix:"resolved with the human" first_after)
      | [] -> Alcotest.fail "expected the run to keep driving")


let test_continue_attached_pass_advances_the_item_cursor () =
  let root = make_temp_root () in
  let paused = pause_fanout_at_item_two root in
  Scripted_executor.reset ~outputs:[ handoff_block () ];
  match
    Scripted_runner.continue_attached ~root ~run_id:paused.Run.id
      ~exec_result:
        (elicited_result
           (claude_style_output
              ~handoff:(handoff_block ~summary:"human fixed beta" ())
              ~session:"sess-item" ()))
      ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      Alcotest.(check string) "run drove to completion" "completed"
        (Run.string_of_status run.Run.status);
      let elicited_step = List.nth run.Run.steps 3 in
      Alcotest.(check string) "elicited record names the paused skill"
        "port-one" elicited_step.Run.skill;
      Alcotest.(check (option string))
        "elicited record carries the item key" (Some "beta")
        elicited_step.Run.item_key;
      match Scripted_executor.prompts () with
      | [ only ] ->
          Alcotest.(check bool)
            "automation continued with the item's next step" true
            (contains ~affix:"item 2 of 2 (beta)" only)
      | prompts ->
          Alcotest.fail
            (Printf.sprintf "expected 1 prompt, got %d" (List.length prompts)))

let test_continue_attached_escalate_stays_paused () =
  let root = make_temp_root () in
  let paused = pause_pipeline_at_step_one root in
  Scripted_executor.reset ~outputs:[];
  match
    Scripted_runner.continue_attached ~root ~run_id:paused.Run.id
      ~exec_result:
        (elicited_result
           (claude_style_output
              ~handoff:
                (handoff_block ~status:"escalate" ~summary:"still blocked" ())
              ()))
      ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "run stays paused" "paused"
        (Run.string_of_status run.Run.status);
      Alcotest.(check int) "the elicited attempt is recorded" 2
        (List.length run.Run.steps);
      Alcotest.(check int) "no further harness calls" 0
        (List.length (Scripted_executor.prompts ()))

let test_continue_attached_invalid_leaves_run_untouched () =
  let root = make_temp_root () in
  let paused = pause_pipeline_at_step_one root in
  match
    Scripted_runner.continue_attached ~root ~run_id:paused.Run.id
      ~exec_result:(elicited_result "no handoff block here")
      ()
  with
  | Ok _ -> Alcotest.fail "an unparseable elicitation must be an error"
  | Error message -> (
      Alcotest.(check bool) "error mentions the handoff" true
        (contains ~affix:"handoff" message);
      match
        Store.Filesystem.load
          ~runs_dir:(Project.runs_dir ~root)
          ~id:paused.Run.id
      with
      | Error message -> Alcotest.fail message
      | Ok reloaded ->
          Alcotest.(check string) "run on disk still paused" "paused"
            (Run.string_of_status reloaded.Run.status);
          Alcotest.(check int) "no record was added" 1
            (List.length reloaded.Run.steps))

let test_parse_cost_known_and_unknown () =
  let known, usage = Metering.parse_cost (claude_style_output ()) in
  Alcotest.(check bool) "cost extracted" true (known = Metering.Cost_usd 0.05);
  Alcotest.(check bool) "usage extracted" true (usage <> None);
  let unknown, no_usage = Metering.parse_cost "plain text with no json" in
  Alcotest.(check bool) "plain text is unknown" true
    (unknown = Metering.Unknown_cost);
  Alcotest.(check bool) "no usage" true (no_usage = None)

let test_handoff_inside_json_envelope () =
  match Handoff.parse (claude_style_output ()) with
  | Error message -> Alcotest.fail message
  | Ok handoff ->
      Alcotest.(check string) "status" "pass"
        (Handoff.string_of_status handoff.Handoff.status)

module Metered_runner =
  Runner.Make (Scripted_executor) (Model_provider.Default) (Metering.Jsonl)
    (Store.Filesystem)

let test_metering_end_to_end () =
  let root = make_temp_root () in
  setup_three_step_project root;
  Scripted_executor.reset
    ~outputs:
      [
        claude_style_output ~cost:0.05 ();
        handoff_block () (* no structured output: unknown, not zero *);
        claude_style_output ~cost:0.02 ();
      ];
  match
    Metered_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "completed" "completed"
        (Run.string_of_status run.Run.status);
      let costs = List.map (fun step -> step.Run.cost) run.Run.steps in
      Alcotest.(check bool) "per-step costs recorded" true
        (costs
        = [ Metering.Cost_usd 0.05; Metering.Unknown_cost; Metering.Cost_usd 0.02 ]);
      let total, unknown = Run.cost_summary run in
      Alcotest.(check bool) "total sums known costs" true
        (abs_float (total -. 0.07) < 1e-9);
      Alcotest.(check int) "one step unknown" 1 unknown;
      let metering_lines =
        In_channel.with_open_text
          (Filename.concat root ".jig/runs/metering.jsonl")
          In_channel.input_all
        |> String.split_on_char '\n'
        |> List.filter (fun line -> String.trim line <> "")
      in
      Alcotest.(check int) "one jsonl record per step" 3
        (List.length metering_lines);
      let second = Yojson.Safe.from_string (List.nth metering_lines 1) in
      Alcotest.(check string) "unknown is explicit in the log" "unknown"
        Yojson.Safe.Util.(second |> member "cost_usd" |> to_string)

let test_cost_roundtrips_through_store () =
  let root = make_temp_root () in
  setup_three_step_project root;
  Scripted_executor.reset ~outputs:[ claude_style_output ~cost:0.31 () ];
  match
    Metered_runner.execute_run ~root ~workflow_name:"hello" ~task:"x"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      match
        Store.Filesystem.load ~runs_dir:(Project.runs_dir ~root)
          ~id:run.Run.id
      with
      | Error message -> Alcotest.fail message
      | Ok loaded -> (
          match loaded.Run.steps with
          | [ step ] ->
              Alcotest.(check bool) "cost survives the roundtrip" true
                (step.Run.cost = Metering.Cost_usd 0.31)
          | _ -> Alcotest.fail "expected one step"))

let test_parse_session_id () =
  Alcotest.(check (option string))
    "captured from structured output" (Some "sess-123")
    (Metering.parse_session_id (claude_style_output ~session:"sess-123" ()));
  Alcotest.(check (option string))
    "absent field stays absent" None
    (Metering.parse_session_id (claude_style_output ()));
  Alcotest.(check (option string))
    "plain text has no session" None
    (Metering.parse_session_id (handoff_block ()))

let test_session_id_roundtrips_through_store () =
  let root = make_temp_root () in
  setup_three_step_project root;
  Scripted_executor.reset
    ~outputs:[ claude_style_output ~session:"sess-42" () ];
  match
    Metered_runner.execute_run ~root ~workflow_name:"hello" ~task:"x"
      ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      match
        Store.Filesystem.load ~runs_dir:(Project.runs_dir ~root)
          ~id:run.Run.id
      with
      | Error message -> Alcotest.fail message
      | Ok loaded -> (
          match loaded.Run.steps with
          | [ step ] ->
              Alcotest.(check (option string))
                "session id survives the roundtrip" (Some "sess-42")
                step.Run.session_id
          | _ -> Alcotest.fail "expected one step"))

let test_attach_command_substitution () =
  (match Attach.command ~attach:[] ~session_id:"abc" with
  | Ok _ -> Alcotest.fail "empty attach config must be an error"
  | Error message ->
      Alcotest.(check bool) "error names the config key" true
        (contains ~affix:"attach" message));
  match
    Attach.command
      ~attach:[ "claude"; "--resume"; "{session_id}"; "pre-{session_id}-post" ]
      ~session_id:"sess-9"
  with
  | Error message -> Alcotest.fail message
  | Ok command ->
      Alcotest.(check (list string))
        "placeholder replaced wherever it appears"
        [ "claude"; "--resume"; "sess-9"; "pre-sess-9-post" ]
        command

let test_run_record_contains_handoffs_in_order () =
  let root = make_temp_root () in
  setup_three_step_project root;
  Scripted_executor.reset
    ~outputs:
      [
        handoff_block ~summary:"alpha" ();
        handoff_block ~summary:"beta" ();
        handoff_block ~summary:"gamma" ();
      ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build" ~isolated:false ()
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      let summaries =
        List.map
          (fun step ->
            match step.Run.handoff with
            | Some handoff -> handoff.Handoff.summary
            | None -> "<none>")
          run.Run.steps
      in
      Alcotest.(check (list string))
        "handoffs in order" [ "alpha"; "beta"; "gamma" ] summaries

(* ---- progress renderer (pure) ---- *)

let progress_plan () =
  match
    Workflow.of_string
      "name: p\n\
       steps:\n\
      \  - skill: read-issue\n\
      \  - skill: prepare\n\
      \  - forEach:\n\
      \      items: items.tsv\n\
      \      as: it\n\
      \      steps:\n\
      \        - retry:\n\
      \            max_iterations: 2\n\
      \            on_exhausted: escalate\n\
      \            steps:\n\
      \              - skill: a\n\
      \              - skill: b\n\
      \                until: pass\n\
      \  - skill: report\n"
  with
  | Ok w -> Progress.init w.Workflow.entries
  | Error m -> Alcotest.fail m

let pos ?for_each entry_index =
  { Run.entry_index; iterations_used = 0; for_each }

let started ?item_key skill position = Runner.Step_started { skill; position; item_key }
let finished skill outcome =
  Runner.Step_finished
    {
      Run.skill;
      outcome;
      exit_code = 0;
      cost = Metering.Unknown_cost;
      stdout = "";
      stderr = "";
      handoff = None;
      handoff_error = None;
      session_id = None;
      item_index = None;
      item_key = None;
      started_at = "";
      finished_at = "";
    }

let loop_pos ~item_index = pos ~for_each:{ Run.item_index; body_index = 0; items = [] } 2

let test_progress_renders_mid_run () =
  let t = progress_plan () in
  let apply = Progress.apply t in
  apply (started "read-issue" (pos 0));
  apply (finished "read-issue" Run.Pass);
  apply (started "prepare" (pos 1));
  apply (finished "prepare" Run.Pass);
  apply (Runner.Items_resolved { entry_index = 2; item_keys = [ "alpha"; "beta" ] });
  apply (started ~item_key:"alpha" "a" (loop_pos ~item_index:0));
  apply (finished "a" Run.Pass);
  apply (started ~item_key:"alpha" "b" (loop_pos ~item_index:0));
  Alcotest.(check (list string))
    "done steps, active item expanded with its body, pending item collapsed"
    [
      "✓ read-issue";
      "✓ prepare";
      "@ forEach (0/2)";
      "  @ alpha";
      "    ✓ a";
      "    @ b";
      "  ○ beta";
      "○ report";
    ]
    (Progress.render ~working:"@" t)

let test_progress_renders_completed () =
  let t = progress_plan () in
  let apply = Progress.apply t in
  apply (started "read-issue" (pos 0));
  apply (finished "read-issue" Run.Pass);
  apply (started "prepare" (pos 1));
  apply (finished "prepare" Run.Pass);
  apply (Runner.Items_resolved { entry_index = 2; item_keys = [ "alpha"; "beta" ] });
  apply (started ~item_key:"alpha" "a" (loop_pos ~item_index:0));
  apply (finished "a" Run.Pass);
  apply (started ~item_key:"alpha" "b" (loop_pos ~item_index:0));
  apply (finished "b" Run.Pass);
  apply (started ~item_key:"beta" "a" (loop_pos ~item_index:1));
  apply (finished "a" Run.Pass);
  apply (started ~item_key:"beta" "b" (loop_pos ~item_index:1));
  apply (finished "b" Run.Pass);
  apply (started "report" (pos 3));
  apply (finished "report" Run.Pass);
  Progress.finalize t;
  Alcotest.(check (list string))
    "all done, items collapsed, no body expansion"
    [
      "✓ read-issue";
      "✓ prepare";
      "✓ forEach (2/2)";
      "  ✓ alpha";
      "  ✓ beta";
      "✓ report";
    ]
    (Progress.render ~working:"@" t)

let test_progress_spinner_cycles () =
  Alcotest.(check string) "first frame" "⠋" (Progress.spinner_frame 0);
  Alcotest.(check string) "mid frame" "⠸" (Progress.spinner_frame 3);
  Alcotest.(check string) "wraps at length" (Progress.spinner_frame 0)
    (Progress.spinner_frame 10)

let test_progress_marks_failure () =
  let t = progress_plan () in
  let apply = Progress.apply t in
  apply (started "read-issue" (pos 0));
  apply (finished "read-issue" Run.Fail);
  Alcotest.(check (list string))
    "a failed step shows the failure glyph"
    [ "✗ read-issue"; "○ prepare"; "○ forEach"; "○ report" ]
    (Progress.render ~working:"@" t)

let () =
  Random.self_init ();
  Alcotest.run "jig"
    [
      ( "workflow",
        [
          Alcotest.test_case "parses name and steps" `Quick test_workflow_parses;
          Alcotest.test_case "rejects missing name" `Quick
            test_workflow_rejects_missing_name;
          Alcotest.test_case "rejects empty steps" `Quick
            test_workflow_rejects_empty_steps;
          Alcotest.test_case "rejects name with slash" `Quick
            test_workflow_rejects_name_with_slash;
          Alcotest.test_case "parses the full frozen schema" `Quick
            test_workflow_parses_full_schema;
          Alcotest.test_case "retry requires max_iterations" `Quick
            test_retry_requires_max_iterations;
          Alcotest.test_case "retry requires on_exhausted" `Quick
            test_retry_requires_on_exhausted;
          Alcotest.test_case "rejects unknown top-level key" `Quick
            test_unknown_top_level_key_rejected;
          Alcotest.test_case "rejects unknown step key" `Quick
            test_unknown_step_key_rejected;
          Alcotest.test_case "rejects until outside retry" `Quick
            test_until_outside_retry_rejected;
          Alcotest.test_case "rejects nested retry" `Quick
            test_nested_retry_rejected;
          Alcotest.test_case "rejects invalid on_fail value" `Quick
            test_invalid_on_fail_value_rejected;
          Alcotest.test_case "rejects non-positive max_iterations" `Quick
            test_non_positive_max_iterations_rejected;
          Alcotest.test_case "parses with inputs in file order" `Quick
            test_with_inputs_parse_in_file_order;
          Alcotest.test_case "with is allowed inside retry steps" `Quick
            test_with_allowed_inside_retry;
          Alcotest.test_case "rejects non-string with value" `Quick
            test_with_rejects_non_string_value;
          Alcotest.test_case "rejects non-mapping with" `Quick
            test_with_rejects_non_mapping;
          Alcotest.test_case "rejects duplicate with keys" `Quick
            test_with_rejects_duplicate_keys;
          Alcotest.test_case "parses a context field" `Quick
            test_context_parses;
          Alcotest.test_case "absent context is none" `Quick
            test_context_absent_is_none;
          Alcotest.test_case "rejects a non-string context" `Quick
            test_context_rejects_non_string;
          Alcotest.test_case "parses a forEach block" `Quick
            test_for_each_parses;
          Alcotest.test_case "forEach requires items" `Quick
            test_for_each_requires_items;
          Alcotest.test_case "forEach requires as" `Quick
            test_for_each_requires_as;
          Alcotest.test_case "forEach requires steps" `Quick
            test_for_each_requires_steps;
          Alcotest.test_case "rejects nested forEach" `Quick
            test_for_each_rejects_nesting;
          Alcotest.test_case "rejects forEach inside retry" `Quick
            test_for_each_rejected_inside_retry;
          Alcotest.test_case "rejects dotted forEach as name" `Quick
            test_for_each_rejects_dotted_as;
          Alcotest.test_case "rejects non-tsv-or-json items path" `Quick
            test_for_each_rejects_bad_items_extension;
          Alcotest.test_case "rejects empty forEach steps" `Quick
            test_for_each_rejects_empty_steps;
          Alcotest.test_case "rejects an unbound placeholder variable" `Quick
            test_for_each_rejects_unbound_variable;
          Alcotest.test_case "rejects a malformed placeholder" `Quick
            test_for_each_rejects_malformed_placeholder;
          Alcotest.test_case "rejects interpolation in a skill name" `Quick
            test_for_each_rejects_interpolated_skill;
        ] );
      ( "progress",
        [
          Alcotest.test_case "renders a mid-run tree" `Quick
            test_progress_renders_mid_run;
          Alcotest.test_case "renders a completed tree" `Quick
            test_progress_renders_completed;
          Alcotest.test_case "marks a failed step" `Quick
            test_progress_marks_failure;
          Alcotest.test_case "spinner cycles and wraps" `Quick
            test_progress_spinner_cycles;
        ] );
      ( "items",
        [
          Alcotest.test_case "parses tsv with a header" `Quick
            test_items_parses_tsv;
          Alcotest.test_case "parses a json array of objects" `Quick
            test_items_parses_json;
          Alcotest.test_case "tolerates crlf tsv" `Quick
            test_items_tolerates_crlf_tsv;
          Alcotest.test_case "rejects a header-only tsv" `Quick
            test_items_rejects_header_only_tsv;
          Alcotest.test_case "rejects an empty json array" `Quick
            test_items_rejects_empty_json_array;
          Alcotest.test_case "rejects a ragged tsv row" `Quick
            test_items_rejects_ragged_tsv_row;
          Alcotest.test_case "rejects duplicate columns" `Quick
            test_items_rejects_duplicate_columns;
          Alcotest.test_case "rejects duplicate item keys" `Quick
            test_items_rejects_duplicate_keys;
          Alcotest.test_case "rejects non-string json values" `Quick
            test_items_rejects_non_string_json_value;
          Alcotest.test_case "rejects ragged json objects" `Quick
            test_items_rejects_ragged_json_objects;
          Alcotest.test_case "rejects an unknown extension" `Quick
            test_items_rejects_unknown_extension;
        ] );
      ( "config",
        [
          Alcotest.test_case "parses harness command" `Quick test_config_parses;
          Alcotest.test_case "rejects missing harness" `Quick
            test_config_rejects_missing_harness;
        ] );
      ( "run",
        [
          Alcotest.test_case "id embeds workflow name" `Quick test_run_id_format;
          Alcotest.test_case "ids differ across processes" `Quick
            test_run_ids_differ_across_processes;
          Alcotest.test_case "store persists a run record" `Quick
            test_store_saves_run;
        ] );
      ( "handoff",
        [
          Alcotest.test_case "parses status, artifacts, summary" `Quick
            test_handoff_parses;
          Alcotest.test_case "empty artifacts key means none" `Quick
            test_handoff_empty_artifacts_key;
          Alcotest.test_case "empty artifact entries mean none" `Quick
            test_handoff_empty_artifact_entries;
          Alcotest.test_case "last block wins" `Quick
            test_handoff_last_block_wins;
          Alcotest.test_case "missing block is an error" `Quick
            test_handoff_missing_block;
          Alcotest.test_case "unknown status is an error" `Quick
            test_handoff_unknown_status;
        ] );
      ( "runner",
        [
          Alcotest.test_case "one-step workflow completes" `Quick
            test_end_to_end_pass;
          Alcotest.test_case "failing harness fails the run" `Quick
            test_end_to_end_fail;
          Alcotest.test_case "missing handoff is a distinct outcome" `Quick
            test_missing_handoff_is_distinct;
          Alcotest.test_case "missing skill fails validation" `Quick
            test_missing_skill_fails_validation;
          Alcotest.test_case "validate lists all missing skills" `Quick
            test_validate_reports_missing_skills;
          Alcotest.test_case "step output is recorded" `Quick
            test_step_output_is_recorded;
          Alcotest.test_case "executor port is swappable" `Quick
            test_executor_is_swappable;
          Alcotest.test_case "a module-directory workflow runs" `Quick
            test_module_form_workflow_runs;
          Alcotest.test_case "list shows module and flat forms" `Quick
            test_list_shows_module_and_flat_forms;
          Alcotest.test_case "AGENTS.md and inline context conflict" `Quick
            test_agents_md_and_inline_context_conflict;
          Alcotest.test_case "execute_run honors a pre-issued run id" `Quick
            test_execute_run_honors_given_run_id;
          Alcotest.test_case "notify fires when a run stops" `Quick
            test_notify_fires_when_run_stops;
          Alcotest.test_case "notify failure is harmless" `Quick
            test_notify_failure_is_harmless;
          Alcotest.test_case "prompt carries the handoff protocol" `Quick
            test_prompt_carries_handoff_protocol;
          Alcotest.test_case "prompt states the workflow position" `Quick
            test_prompt_states_workflow_position;
          Alcotest.test_case "retry prompts share the entry position" `Quick
            test_retry_prompt_states_group_position;
          Alcotest.test_case "prompt carries step inputs" `Quick
            test_prompt_carries_step_inputs;
          Alcotest.test_case "retry steps carry their own inputs" `Quick
            test_retry_steps_carry_their_own_inputs;
          Alcotest.test_case "context renders into every prompt" `Quick
            test_context_renders_into_every_prompt;
          Alcotest.test_case "forEach runs the body once per item" `Quick
            test_for_each_runs_body_per_item;
          Alcotest.test_case "lifecycle events fire across step kinds" `Quick
            test_lifecycle_events_fire_across_step_kinds;
          Alcotest.test_case "forEach isolates handoffs between items" `Quick
            test_for_each_isolates_handoffs_between_items;
          Alcotest.test_case "forEach retry loops within one item" `Quick
            test_for_each_retry_loops_within_one_item;
          Alcotest.test_case "escalate inside forEach pauses at the item"
            `Quick test_for_each_escalate_pauses_at_the_item;
          Alcotest.test_case "resume uses the snapshot, not the edited file"
            `Quick test_for_each_resume_uses_the_snapshot;
          Alcotest.test_case "skip completes the current item step only"
            `Quick test_for_each_skip_completes_the_item_step_only;
          Alcotest.test_case "missing referenced column fails before any step"
            `Quick test_for_each_missing_column_fails_before_any_step;
          Alcotest.test_case "missing items file fails at entry" `Quick
            test_for_each_missing_items_file_fails_at_entry;
          Alcotest.test_case "validate defers an absent items file" `Quick
            test_validate_defers_absent_items;
          Alcotest.test_case "run json round-trips a forEach position" `Quick
            test_run_json_roundtrips_for_each_position;
          Alcotest.test_case "old run json without for_each still loads"
            `Quick test_old_run_json_still_loads;
          Alcotest.test_case "handoffs thread between steps" `Quick
            test_handoffs_thread_between_steps;
          Alcotest.test_case "fail handoff short-circuits" `Quick
            test_fail_handoff_short_circuits;
          Alcotest.test_case "escalate handoff pauses the run" `Quick
            test_escalate_handoff_pauses_run;
          Alcotest.test_case "record keeps handoffs in order" `Quick
            test_run_record_contains_handoffs_in_order;
        ] );
      ( "inspection",
        [
          Alcotest.test_case "list workflows reports valid and invalid" `Quick
            test_list_workflows_reports_valid_and_invalid;
          Alcotest.test_case "store load hints at version mismatch" `Quick
            test_store_load_hints_version_on_garbage;
          Alcotest.test_case "last handoff picks the latest" `Quick
            test_last_handoff_picks_latest;
        ] );
      ( "skill-paths",
        [
          Alcotest.test_case "external skill resolves and runs" `Quick
            test_external_skill_resolves_and_runs;
          Alcotest.test_case "repo skill shadows external" `Quick
            test_repo_skill_shadows_external;
          Alcotest.test_case "miss lists every searched path" `Quick
            test_missing_skill_error_lists_searched_paths;
          Alcotest.test_case "tilde expands against HOME" `Quick
            test_skill_paths_expand_home;
        ] );
      ( "visibility",
        [
          Alcotest.test_case "run record visible during first step" `Quick
            test_run_record_visible_during_first_step;
          Alcotest.test_case "on_step fires per step in order" `Quick
            test_on_step_fires_per_step_in_order;
          Alcotest.test_case "list runs newest-first" `Quick
            test_list_runs_newest_first;
          Alcotest.test_case "list runs empty when none" `Quick
            test_list_runs_empty_when_none;
        ] );
      ( "init",
        [
          Alcotest.test_case "scaffolds a valid project" `Quick
            test_init_scaffolds_valid_project;
          Alcotest.test_case "refuses an existing .jig" `Quick
            test_init_refuses_existing_jig_dir;
          Alcotest.test_case "claude preset is well-formed" `Quick
            test_init_claude_preset_is_well_formed;
          Alcotest.test_case "codex preset is well-formed" `Quick
            test_init_codex_preset_is_well_formed;
          Alcotest.test_case "bare init matches the template" `Quick
            test_init_custom_matches_template;
        ] );
      ( "metering",
        [
          Alcotest.test_case "parses known and unknown costs" `Quick
            test_parse_cost_known_and_unknown;
          Alcotest.test_case "handoff found inside a json envelope" `Quick
            test_handoff_inside_json_envelope;
          Alcotest.test_case "costs recorded per step and in jsonl" `Quick
            test_metering_end_to_end;
          Alcotest.test_case "cost roundtrips through the store" `Quick
            test_cost_roundtrips_through_store;
          Alcotest.test_case "session id parsed from harness output" `Quick
            test_parse_session_id;
          Alcotest.test_case "session id roundtrips through the store" `Quick
            test_session_id_roundtrips_through_store;
          Alcotest.test_case "attach command substitutes the session id"
            `Quick test_attach_command_substitution;
        ] );
      ( "isolation",
        [
          Alcotest.test_case "config wrapper prepends to the harness" `Quick
            test_config_wrapper_prepends;
          Alcotest.test_case "isolated run uses a worktree, resume reuses it"
            `Quick test_isolated_run_uses_worktree_and_resume_reuses_it;
          Alcotest.test_case "resume honors the recorded workspace location"
            `Quick test_resume_honors_recorded_workspace_location;
          Alcotest.test_case "concurrent isolated runs do not interfere"
            `Quick test_concurrent_isolated_runs_do_not_interfere;
        ] );
      ( "lifecycle",
        [
          Alcotest.test_case "retry loops until the until-step passes" `Quick
            test_retry_until_pass;
          Alcotest.test_case "invalid handoff text threads into the retry"
            `Quick test_invalid_handoff_text_threads_into_retry;
          Alcotest.test_case "exhausted retry escalates to paused" `Quick
            test_retry_exhausted_escalates;
          Alcotest.test_case "exhausted retry aborts" `Quick
            test_retry_exhausted_aborts;
          Alcotest.test_case "on_fail escalate pauses" `Quick
            test_on_fail_escalate_pauses;
          Alcotest.test_case "on_fail abort aborts" `Quick
            test_on_fail_abort_aborts;
          Alcotest.test_case "attached pass drives the run onward" `Quick
            test_continue_attached_pass_drives_the_run;
          Alcotest.test_case "attached pass advances the item cursor" `Quick
            test_continue_attached_pass_advances_the_item_cursor;
          Alcotest.test_case "attached escalate stays paused" `Quick
            test_continue_attached_escalate_stays_paused;
          Alcotest.test_case "unparseable elicitation leaves the run untouched"
            `Quick test_continue_attached_invalid_leaves_run_untouched;
          Alcotest.test_case "skip marks the paused entry done" `Quick
            test_skip_marks_paused_entry_done;
          Alcotest.test_case "skip advances past the whole retry group" `Quick
            test_skip_advances_past_retry_group;
          Alcotest.test_case "resume continues from the paused step" `Quick
            test_resume_continues_from_paused_step;
          Alcotest.test_case "resume refuses completed runs" `Quick
            test_resume_refuses_completed_runs;
          Alcotest.test_case "state persists incrementally" `Quick
            test_record_persisted_incrementally;
        ] );
    ]
