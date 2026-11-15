# Plot Quality Distribution

Generates an interactive plot with number of reads at each Q-score
sorted by pass/fail filtering status. Two vertical lines are drawn: the
user-defined Q-score cut-off (`qscore_cutoff`, by default 7) and the
basecaller threshold, i.e. the lowest Q-score among passing reads. The
basecaller threshold line is only drawn when both passing and failing
reads are present in the data.

## Usage

``` r
plot_quality_distribution(seq_summary, qscore_cutoff = 7)
```

## Arguments

- seq_summary:

  A data frame containing the sequencing summary

- qscore_cutoff:

  Numeric Q-score cut-off drawn as a vertical line. Defaults to 7.

## Value

plotly object

## Examples

``` r
if (FALSE) { # \dontrun{
plot_quality_distribution(sample_data)
} # }
```
