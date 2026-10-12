# Implementing a port

jig's runner is a functor over four module signatures - the ports. Swapping
an implementation never touches a workflow or a skill; port selection is
composition in code, configuration on disk.

```ocaml
module My_runner =
  Jig_core.Runner.Make (My_executor) (My_model_provider) (My_metering) (My_store)
```

## The four signatures

**Executor** - runs one step's harness invocation.

```ocaml
val execute :
  command:string list -> cwd:string -> prompt:string ->
  (Executor.exec_result, string) result
```

The default spawns a local subprocess (prompt appended as the final
argument, output captured via temp files). A Docker or microVM executor
implements the same signature; workspace isolation (`--isolated` worktrees)
and the config `wrapper` already live at this layer.

**ModelProvider** - resolves a step to a concrete harness command.

```ocaml
val resolve : config:Config.t -> skill:string -> (string list, string) result
```

The default returns `wrapper @ harness` from `.jig/config.yaml`. A smarter
provider could pick different harnesses or models per skill.

**Metering** - records one usage event per step invocation.

```ocaml
val record : runs_dir:string -> event:Metering.event -> (unit, string) result
```

The default appends JSONL to `.jig/runs/metering.jsonl`. A budget gate would
return `Error` when spend crosses a cap - the run stops with that message.

**Store** - persists and loads run records, and owns the lock that lets
one process at a time drive a run.

```ocaml
type run_lock
val save : runs_dir:string -> Run.t -> (string, string) result
val load : runs_dir:string -> id:string -> (Run.t, string) result
val list_runs : runs_dir:string -> (Store.run_listing, string) result
val acquire_run_lock : runs_dir:string -> id:string -> (run_lock, string) result
val release_run_lock : run_lock -> unit
```

The default writes `runs/<id>.json`. Anything that can round-trip
`Run.to_json`/`Run.of_json` works: SQLite, Postgres, an API.

`save` is an atomic full replacement: a reader sees the previous record or
the new one, never a mix. The default writes the record to a temp file per
process (`runs/<id>.json.tmp.<pid>`), fsyncs it, renames it over
`runs/<id>.json`, and fsyncs the directory.

`list_runs` returns the runs it could read (newest first) and, apart, the
records it could not. One unreadable record never fails the listing.

The runner takes `acquire_run_lock` before the first save of a run, start
or resume, and calls `release_run_lock` after the last. `acquire_run_lock`
returns `Error` while another live process holds the lock; that message is
what `jig run --resume` prints. The default lock is `runs/<id>.lock`, a JSON
file with the holder's pid, host, and start time. It is placed with a hard
link, so a lock file is never visible without its content. A lock whose pid
is dead on the same host is stale, and the next acquire takes it over. A
lock from another host is never taken over: its pid cannot be checked, so
the error names the lock file for a human to remove.

## Ground rules

- Errors are `result`, never exceptions across the port boundary.
- The runner re-saves the record after every step; `save` must be safe to
  call repeatedly with the same id, and a process killed during a save must
  leave the previous complete record.
- A Store that keeps records somewhere other processes can reach must make
  its run lock exclusive across all of them.
- Do not leak port specifics into the workflow schema - if your
  implementation needs a knob, it belongs in config or composition, not in
  `workflow.yaml`.
