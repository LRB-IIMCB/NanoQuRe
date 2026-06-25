# Plot Active Channels

Generates an interactive plot showing number of active channels during
sequencing time. Vertical dashed lines mark the time point at which 50
and 25 time in hours. If a threshold is not reached during the run, a
warning annotation is displayed on the plot prompting closer inspection.

## Usage

``` r
plot_active_channels(seq_summary, thresholds = c(0.5, 0.25))
```

## Arguments

- seq_summary:

  A dataframe containing the sequencing summary

- thresholds:

  Numeric vector of fractions at which to draw threshold lines. Defaults
  to c(0.50, 0.25).

## Value

plotly object

## Examples

``` r
if (FALSE) { # \dontrun{
plot_active_channels(sample_data)
} # }
```
