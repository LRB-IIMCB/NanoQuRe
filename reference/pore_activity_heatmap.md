# Pore Activity Heatmap

Generates an interactive heatmap showing channel activity over time and
channel activity with respect to their disposition on the flow cell.

## Usage

``` r
pore_activity_heatmap(seq_summary, platform = "minion", bin_width = 1)
```

## Arguments

- seq_summary:

  A data frame containing the sequencing summary

- platform:

  Flow cell used for sequencing: `"minion"` or `"promethion"`. Defaults
  to `"minion"`.

- bin_width:

  Width of each time bin in hours (a single positive number) for the
  channel-by-time heatmap. Defaults to 1. Increase for very long runs to
  limit matrix size.

## Value

plotly object

## Details

The input must contain a single `run_id`. Only the MinION (512 channels)
and PromethION (3000 channels) layouts are supported. Flongle (126
channels) is not: Flongle data passed with `platform = "minion"` is
drawn on the MinION layout, so the spatial panel does not reflect the
physical positions of the channels.

Function contains helpers originally provided by Barbara Ottolini, from
Oxford Nanopore Technologies. For reference see:
https://github.com/sagrudd/nanopoRe/blob/master/R/FlowcellLayout.R

## Examples

``` r
if (FALSE) { # \dontrun{
pore_activity_heatmap(sample_data, platform = "minion")
} # }
```
