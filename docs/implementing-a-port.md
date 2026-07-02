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

The default appends JSONL to `runs/metering.jsonl`. A budget gate would
return `Error` when spend crosses a cap - the run stops with that message.

**Store** - persists and loads run records.

```ocaml
val save : runs_dir:string -> Run.t -> (string, string) result
val load : runs_dir:string -> id:string -> (Run.t, string) result
```

The default writes `runs/<id>.json`. Anything that can round-trip
`Run.to_json`/`Run.of_json` works: SQLite, Postgres, an API.

## Ground rules

- Errors are `result`, never exceptions across the port boundary.
- The runner re-saves the record after every step; `save` must be safe to
  call repeatedly with the same id.
- Do not leak port specifics into the workflow schema - if your
  implementation needs a knob, it belongs in config or composition, not in
  `workflow.yaml`.
