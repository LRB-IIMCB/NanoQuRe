# Plot Read Lengths

Generates an interactive plot with number of reads of each length in
bases. Two vertical lines represent the mean read length and the N50
value calculated with
[`calculate_n50`](https://LRB-IIMCB.github.io/NanoQuRe/reference/calculate_n50.md).

## Usage

``` r
plot_read_lengths(seq_summary, upper_limit = 4000, y_limit = NULL)
```

## Arguments

- seq_summary:

  A dataframe containing the sequencing summary

- upper_limit:

  Upper limit of the x axis. Defaults to 4000.

- y_limit:

  Upper limit of the y axis. If NULL (default), automatically set to the
  99.9th percentile of read counts multiplied by 1.1.

## Value

plotly object

## Examples

``` r
if (FALSE) { # \dontrun{
plot_read_lengths(sample_data)
} # }
```
