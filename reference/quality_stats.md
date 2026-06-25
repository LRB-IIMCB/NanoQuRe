# Quality Stats

Generates a table containing the most important quality metrics from a
sequencing summary: number of all reads, number of passed reads, N50
value, mean Q-score, longest read length, and mean length of passed
reads.

## Usage

``` r
quality_stats(seq_summary)
```

## Arguments

- seq_summary:

  A dataframe containing the sequencing summary

## Value

A dataframe with quality metrics for the sequencing run

## Examples

``` r
if (FALSE) { # \dontrun{
quality_stats(sample_data)
} # }
```
