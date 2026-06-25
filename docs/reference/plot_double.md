# Plot Length and Q-score Over Time

Generates two interactive plots showing how read length and mean Q score
change over the course of a sequencing run. Returns both as a named
list.

## Usage

``` r
plot_double(seq_summary)
```

## Arguments

- seq_summary:

  A dataframe containing the sequencing summary

## Value

A named list with two plotly objects:

- length_plot:

  Read length (min, mean, max) per hour

- qscore_plot:

  Mean Q score (min, mean, max) per hour

## Examples

``` r
if (FALSE) { # \dontrun{
result <- plot_double(sample_data)
result$length_plot
result$qscore_plot
} # }
```
