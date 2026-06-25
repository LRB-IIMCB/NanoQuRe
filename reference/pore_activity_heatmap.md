# Pore Activity Heatmap

Generates an interactive heatmap showing channel activity over time and
channel activity with respect to their disposition on the flowcell.

## Usage

``` r
pore_activity_heatmap(seq_summary, platform = "minion", bin_width = 1)
```

## Arguments

- seq_summary:

  A dataframe containing the sequencing summary

- platform:

  Flowcell used for sequencing: `"minion"` or `"promethion"`. Defaults
  to `"minion"`.

- bin_width:

  Width of each time bin in hours for the channel-by-time heatmap.
  Defaults to 1. Increase for very long runs to limit matrix size.

## Value

plotly object

## Details

Function contains helpers originally provided by Barbara Ottolini, from
Oxford Nanopore Technologies. For reference see:
https://github.com/sagrudd/nanopoRe/blob/master/R/FlowcellLayout.R

## Examples

``` r
if (FALSE) { # \dontrun{
pore_activity_heatmap(sample_data, platform = "minion")
} # }
```
