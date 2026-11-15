# Sequencing Stats

Generates a table containing general run metrics from a sequencing
summary: sample ID, duration in hours, number of reads, total bases
sequenced in Gb, percentage of passed reads, and average sequencing
speed in nt/s. Missing values are removed from every sum; the speed is
`NA` when the total read duration is zero.

## Usage

``` r
sequencing_stats(seq_summary)
```

## Arguments

- seq_summary:

  A data frame containing the sequencing summary

## Value

A data frame with general run metrics

## Examples

``` r
if (FALSE) { # \dontrun{
sequencing_stats(sample_data)
} # }
```
