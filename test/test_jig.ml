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
        (List.map (fun s -> s.Workflow.skill) workflow.Workflow.steps)

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
      status = Run.Completed;
      error = None;
      steps = [];
      started_at = "2026-07-02T12:00:00";
      finished_at = "2026-07-02T12:00:01";
    }
  in
  match Store.Filesystem.save ~runs_dir:(Filename.concat root "runs") run with
  | Error message -> Alcotest.fail message
  | Ok path ->
      Alcotest.(check bool) "run file exists" true (Sys.file_exists path);
      let json = Yojson.Safe.from_file path in
      let id = Yojson.Safe.Util.(json |> member "id" |> to_string) in
      Alcotest.(check string) "id roundtrips" "2026-07-02-hello-120000-1" id

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

let test_end_to_end_pass () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "echo" ];
  match
    Runner.Default.execute_run ~root ~workflow_name:"hello" ~task:"say hi"
  with
  | Error message -> Alcotest.fail message
  | Ok (run, path) ->
      Alcotest.(check string) "status" "completed"
        (Run.string_of_status run.Run.status);
      Alcotest.(check int) "one step" 1 (List.length run.Run.steps);
      Alcotest.(check bool) "record persisted" true (Sys.file_exists path)

let test_end_to_end_fail () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "false" ];
  match
    Runner.Default.execute_run ~root ~workflow_name:"hello" ~task:"say hi"
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "status" "failed"
        (Run.string_of_status run.Run.status)

let test_missing_skill_is_an_error () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "echo" ];
  write_file
    (Filename.concat root ".jig/workflows/broken.yaml")
    "name: broken\nsteps:\n  - skill: does-not-exist\n";
  match
    Runner.Default.execute_run ~root ~workflow_name:"broken" ~task:"x"
  with
  | Ok _ -> Alcotest.fail "expected an error for a missing skill"
  | Error message ->
      Alcotest.(check bool) "mentions the skill" true
        (contains ~affix:"does-not-exist" message);
      let runs_dir = Filename.concat root "runs" in
      let persisted = Sys.readdir runs_dir in
      Alcotest.(check int) "failed run record persisted" 1
        (Array.length persisted)

let test_workflow_rejects_name_with_slash () =
  match Workflow.of_string "name: team/deploy\nsteps:\n  - skill: x\n" with
  | Ok _ -> Alcotest.fail "expected an error for a name containing '/'"
  | Error message ->
      Alcotest.(check bool) "mentions the name" true
        (contains ~affix:"team/deploy" message)

let test_step_output_is_recorded () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "echo" ];
  match
    Runner.Default.execute_run ~root ~workflow_name:"hello" ~task:"say hi"
  with
  | Error message -> Alcotest.fail message
  | Ok (run, _) -> (
      match run.Run.steps with
      | [ step ] ->
          Alcotest.(check bool)
            "stdout captured the prompt" true
            (contains ~affix:"say hi" step.Run.stdout)
      | _ -> Alcotest.fail "expected exactly one step")

(* Port swappability: the runner accepts any Executor implementation *)

module Fake_executor : Executor.S = struct
  let execute ~command:_ ~prompt =
    Ok { Executor.exit_code = 0; stdout = prompt; stderr = "" }
end

module Fake_runner =
  Runner.Make (Fake_executor) (Model_provider.Default) (Metering.Noop)
    (Store.Filesystem)

let test_executor_is_swappable () =
  let root = make_temp_root () in
  setup_project root ~harness:[ "irrelevant" ];
  match Fake_runner.execute_run ~root ~workflow_name:"hello" ~task:"say hi" with
  | Error message -> Alcotest.fail message
  | Ok (run, _) ->
      Alcotest.(check string) "status" "completed"
        (Run.string_of_status run.Run.status)

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
      ( "runner",
        [
          Alcotest.test_case "one-step workflow completes" `Quick
            test_end_to_end_pass;
          Alcotest.test_case "failing harness fails the run" `Quick
            test_end_to_end_fail;
          Alcotest.test_case "missing skill is an error" `Quick
            test_missing_skill_is_an_error;
          Alcotest.test_case "step output is recorded" `Quick
            test_step_output_is_recorded;
          Alcotest.test_case "executor port is swappable" `Quick
            test_executor_is_swappable;
        ] );
    ]
