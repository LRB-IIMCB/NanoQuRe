# Sequencing Stats

Generates a table containing general run metrics from a sequencing
summary: sample ID, duration in hours, number of reads, total bases
sequenced in Gb, percentage of passed reads, and average sequencing
speed in bp/s.

## Usage

``` r
sequencing_stats(seq_summary)
```

## Arguments

- seq_summary:

  A dataframe containing the sequencing summary

## Value

A dataframe with general run metrics

## Examples

``` r
if (FALSE) { # \dontrun{
sequencing_stats(sample_data)
} # }
```
