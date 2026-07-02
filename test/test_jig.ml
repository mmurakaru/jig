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

let write_file path content =
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
      | [ Workflow.Step first; _; Workflow.Retry retry; _ ] ->
          Alcotest.(check bool) "on_fail parsed" true
            (first.Workflow.on_fail = Some Workflow.Escalate);
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
      position = { Run.entry_index = 2; iterations_used = 1 };
      workspace = Some "/somewhere/worktree";
      steps = [];
      started_at = "2026-07-02T12:00:00Z";
      finished_at = None;
    }
  in
  let runs_dir = Filename.concat root "runs" in
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
    Runner.Default.execute_run ~root ~workflow_name:"hello" ~task:"say hi" ~isolated:false
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
    Runner.Default.execute_run ~root ~workflow_name:"hello" ~task:"say hi" ~isolated:false
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
    Runner.Default.execute_run ~root ~workflow_name:"hello" ~task:"say hi" ~isolated:false
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
    Runner.Default.execute_run ~root ~workflow_name:"broken" ~task:"x" ~isolated:false
  with
  | Ok _ -> Alcotest.fail "expected an error for a missing skill"
  | Error message ->
      Alcotest.(check bool) "mentions the skill" true
        (contains ~affix:"does-not-exist" message);
      Alcotest.(check bool) "refused before running: no runs dir" false
        (Sys.file_exists (Filename.concat root "runs"))

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
      (fun workflow -> Validate.workflow ~jig_dir workflow)
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
    Runner.Default.execute_run ~root ~workflow_name:"hello" ~task:"say hi" ~isolated:false
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
    Scripted_runner.execute_run ~root ~workflow_name:"hello" ~task:"say hi" ~isolated:false
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "status" "completed"
        (Run.string_of_status run.Run.status)

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
    Scripted_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build" ~isolated:false
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

let test_prompt_carries_handoff_protocol () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  Scripted_executor.reset ~outputs:[];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"hello" ~task:"x"
      ~isolated:false
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
            (contains ~affix:"status: <pass | fail | escalate>" prompt)
      | prompts ->
          Alcotest.fail
            (Printf.sprintf "expected 1 prompt, got %d" (List.length prompts)))

let test_fail_handoff_short_circuits () =
  let root = make_temp_root () in
  setup_three_step_project root;
  Scripted_executor.reset
    ~outputs:[ handoff_block ~status:"fail" ~summary:"cannot reproduce" () ];
  match
    Scripted_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build" ~isolated:false
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
    Scripted_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build" ~isolated:false
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "status" "paused"
        (Run.string_of_status run.Run.status);
      Alcotest.(check int) "run stopped at the escalating step" 1
        (List.length run.Run.steps);
      Alcotest.(check bool) "paused runs have no finished_at" true
        (run.Run.finished_at = None)

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
    Scripted_runner.execute_run ~root ~workflow_name:"fixloop" ~task:"fix it" ~isolated:false
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
    Scripted_runner.execute_run ~root ~workflow_name:"fixloop" ~task:"fix it" ~isolated:false
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
    Scripted_runner.execute_run ~root ~workflow_name:"fixloop" ~task:"fix it" ~isolated:false
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
    Scripted_runner.execute_run ~root ~workflow_name:"guarded" ~task:"x" ~isolated:false
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
    Scripted_runner.execute_run ~root ~workflow_name:"guarded" ~task:"x" ~isolated:false
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
    Scripted_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build" ~isolated:false
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
          ~guidance:(Some "focus on the flaky assertion")
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
    Scripted_runner.execute_run ~root ~workflow_name:"hello" ~task:"x" ~isolated:false
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      match
        Scripted_runner.resume_run ~root ~run_id:run.Run.id ~guidance:None
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
    Scripted_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build" ~isolated:false
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      let runs_dir = Filename.concat root "runs" in
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
  let runs_dir = Filename.concat root "runs" in
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
      position = { Run.entry_index = 0; iterations_used = 0 };
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
      ~isolated:true
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
          ~guidance:None
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
        ~isolated:true
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
  match Init.scaffold ~root with
  | Error message -> Alcotest.fail message
  | Ok written -> (
      Alcotest.(check bool) "several files written" true
        (List.length written >= 7);
      let jig_dir = Filename.concat root ".jig" in
      match
        Result.bind
          (Workflow.load
             ~path:(Filename.concat jig_dir "workflows/bugfix.yaml"))
          (fun workflow -> Validate.workflow ~jig_dir workflow)
      with
      | Error message -> Alcotest.fail message
      | Ok () -> ())

let test_init_refuses_existing_jig_dir () =
  let root = make_bare_temp_dir () in
  Unix.mkdir (Filename.concat root ".jig") 0o755;
  match Init.scaffold ~root with
  | Ok _ -> Alcotest.fail "expected init to refuse an existing .jig"
  | Error message ->
      Alcotest.(check bool) "names the conflict" true
        (contains ~affix:".jig" message)

(* Metering *)

let claude_style_output ?(cost = 0.05) ?(handoff = handoff_block ()) () =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("type", `String "result");
         ("result", `String ("all done\n\n" ^ handoff));
         ("total_cost_usd", `Float cost);
         ( "usage",
           `Assoc [ ("input_tokens", `Int 900); ("output_tokens", `Int 120) ] );
       ])

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
      ~isolated:false
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
          (Filename.concat root "runs/metering.jsonl")
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
      ~isolated:false
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      match
        Store.Filesystem.load ~runs_dir:(Filename.concat root "runs")
          ~id:run.Run.id
      with
      | Error message -> Alcotest.fail message
      | Ok loaded -> (
          match loaded.Run.steps with
          | [ step ] ->
              Alcotest.(check bool) "cost survives the roundtrip" true
                (step.Run.cost = Metering.Cost_usd 0.31)
          | _ -> Alcotest.fail "expected one step"))

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
    Scripted_runner.execute_run ~root ~workflow_name:"pipeline" ~task:"build" ~isolated:false
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
          Alcotest.test_case "prompt carries the handoff protocol" `Quick
            test_prompt_carries_handoff_protocol;
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
      ( "init",
        [
          Alcotest.test_case "scaffolds a valid project" `Quick
            test_init_scaffolds_valid_project;
          Alcotest.test_case "refuses an existing .jig" `Quick
            test_init_refuses_existing_jig_dir;
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
        ] );
      ( "isolation",
        [
          Alcotest.test_case "config wrapper prepends to the harness" `Quick
            test_config_wrapper_prepends;
          Alcotest.test_case "isolated run uses a worktree, resume reuses it"
            `Quick test_isolated_run_uses_worktree_and_resume_reuses_it;
          Alcotest.test_case "concurrent isolated runs do not interfere"
            `Quick test_concurrent_isolated_runs_do_not_interfere;
        ] );
      ( "lifecycle",
        [
          Alcotest.test_case "retry loops until the until-step passes" `Quick
            test_retry_until_pass;
          Alcotest.test_case "exhausted retry escalates to paused" `Quick
            test_retry_exhausted_escalates;
          Alcotest.test_case "exhausted retry aborts" `Quick
            test_retry_exhausted_aborts;
          Alcotest.test_case "on_fail escalate pauses" `Quick
            test_on_fail_escalate_pauses;
          Alcotest.test_case "on_fail abort aborts" `Quick
            test_on_fail_abort_aborts;
          Alcotest.test_case "resume continues from the paused step" `Quick
            test_resume_continues_from_paused_step;
          Alcotest.test_case "resume refuses completed runs" `Quick
            test_resume_refuses_completed_runs;
          Alcotest.test_case "state persists incrementally" `Quick
            test_record_persisted_incrementally;
        ] );
    ]
