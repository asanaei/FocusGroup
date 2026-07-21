# Build prompt history string for role-specific windows

Build prompt history string for role-specific windows

## Usage

``` r
make_prompt_history(
  log,
  n_recent = NULL,
  include_summary = NULL,
  max_tokens_history = 64000L
)
```

## Arguments

- log:

  A conversation log list.

- n_recent:

  Integer. Optional. Number of recent messages to include.

- include_summary:

  Character. Optional summary of earlier discussion.

- max_tokens_history:

  Integer. Approximate token ceiling when \`n_recent\` is \`NULL\`.

## Value

A character string with recent prompt history.
