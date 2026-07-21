# Parse a 0-10 integer score from free text

An explicit fraction form ("8/10", "8 out of 10") states the score
directly and wins outright. Otherwise scale-range mentions ("0-10", "0
to 10") are removed first, and the LAST remaining integer is taken:
models often echo the scale label ("Desire to talk score (0-10): 8") or
reason before answering, so the answer comes last once the range
endpoints are gone.

## Usage

``` r
parse_score_0_10(text)
```
