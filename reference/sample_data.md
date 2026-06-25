# Example Sequencing Summary

A trimmed sequencing summary file produced by an ONT basecaller
(Guppy/Dorado), included for use in examples and vignettes. Contains 30
reads from a single MinION run.

## Usage

``` r
sample_data
```

## Format

A data frame with 30 rows and 8 columns:

- sample_id:

  character. Sample identifier.

- run_id:

  character. Unique run identifier assigned by the basecaller.

- channel:

  integer. Pore channel number (1–512 for MinION).

- start_time:

  numeric. Read start time in seconds from run start.

- duration:

  numeric. Read duration in seconds.

- sequence_length_template:

  numeric. Read length in bases.

- mean_qscore_template:

  numeric. Mean Q score of the read.

- passes_filtering:

  logical. Whether the read passed basecaller filters.

## Source

Oxford Nanopore Technologies sequencing summary output.
