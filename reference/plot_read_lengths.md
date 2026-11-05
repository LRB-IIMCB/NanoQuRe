# Plot Read Lengths

Generates an interactive histogram of read lengths. Reads are counted in
fixed bins (50 bins per order of magnitude on the default log axis, 200
equal bins on a linear axis), so the plot size does not grow with the
number of reads. Two vertical lines represent the mean read length and
the N50 value calculated with
[`calculate_n50`](https://LRB-IIMCB.github.io/NanoQuRe/reference/calculate_n50.md).

## Usage

``` r
plot_read_lengths(
  seq_summary,
  upper_limit = NULL,
  y_limit = NULL,
  log_x = TRUE,
  weight = "reads"
)
```

## Arguments

- seq_summary:

  A dataframe containing the sequencing summary

- upper_limit:

  Upper limit of the x axis in nucleotides. If NULL (default), the full
  length range is shown. Reads beyond the limit are counted in an
  on-plot note.

- y_limit:

  Upper limit of the y axis. If NULL (default), the axis is scaled
  automatically.

- log_x:

  Logical; plot lengths on a log10 axis. Defaults to TRUE. Reads of
  length 0 cannot be shown on a log axis and are counted in an on-plot
  note.

- weight:

  What the bar height counts: `"reads"` (default, number of reads per
  bin) or `"bases"` (sum of read lengths per bin, i.e. where the
  sequenced bases are).

## Value

plotly object

## Examples

``` r
if (FALSE) { # \dontrun{
plot_read_lengths(sample_data)
plot_read_lengths(sample_data, weight = "bases")
} # }
```
