# Plot Cumulative Yield

Generates an interactive cumulative plot containing the number of
sequenced bases in Gb over time in hours sorted by pass/fail filtering
status.

## Usage

``` r
plot_cumulative_yield(seq_summary, max_points = 2000L)
```

## Arguments

- seq_summary:

  A dataframe containing the sequencing summary

- max_points:

  Maximum number of points plotted per pass/fail trace. Reads are binned
  by time so very large runs stay performant in the browser. Defaults to
  2000L.

## Value

plotly object

## Examples

``` r
if (FALSE) { # \dontrun{
plot_cumulative_yield(sample_data)
} # }
```
