# Calculate N50

Calculates the N50 value: the read length at which 50 bases are
contained in reads of that length or longer.

## Usage

``` r
calculate_n50(seq_summary)
```

## Arguments

- seq_summary:

  A dataframe containing the sequencing summary

## Value

Numeric N50 value in bases

## Examples

``` r
if (FALSE) { # \dontrun{
 seq_summary <- data.frame(
  sequence_length_template = c(1, 2, 3, 4, 10)
)
calculate_n50(seq_summary)
} # }
```
