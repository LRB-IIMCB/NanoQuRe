# Plot Quality Distribution

Generates an interactive plot with number of reads at each Q score
sorted by pass/fail filtering status. Vertical line represents Q score
cut-off which is by default equal to 7. The basecaller threshold line is
only drawn when both passing and failing reads are present in the data.

## Usage

``` r
plot_quality_distribution(seq_summary, qscore_cutoff = 7)
```

## Arguments

- seq_summary:

  A dataframe containing the sequencing summary

- qscore_cutoff:

  Numeric parameter of Qscore cut-off

## Value

plotly object

## Examples

``` r
if (FALSE) { # \dontrun{
plot_quality_distribution(sample_data)
} # }
```
