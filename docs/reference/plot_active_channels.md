# Plot Active Channels

Generates an interactive plot of the number of channels producing reads
over the course of the run. The run is divided into time windows
(`bin_width`, 10 minutes by default) and a channel counts as active in a
window when at least one of its reads overlaps that window. Vertical
dashed lines mark the first window after the reference point in which
the number of active channels falls to 50% and 25% of the reference,
where the reference is the highest count within the first hour of the
run. If a threshold is not reached, a note is displayed on the plot.

## Usage

``` r
plot_active_channels(seq_summary, thresholds = c(0.5, 0.25), bin_width = 1/6)
```

## Arguments

- seq_summary:

  A dataframe containing the sequencing summary

- thresholds:

  Numeric vector of fractions at which to draw threshold lines. Defaults
  to c(0.50, 0.25).

- bin_width:

  Width of each time window in hours. Defaults to 1/6 (10 minutes).

## Value

plotly object

## Details

The sequencing summary does not record pore (mux) scans or pauses, so
short periodic dips or gaps in the curve can reflect run control rather
than pore loss.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_active_channels(sample_data)
} # }
```
