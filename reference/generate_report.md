# Generate Report

Generates an HTML QC report from one or more sequencing summaries.
Automatically selects a single-sample or multi-sample template based on
the number of unique sample IDs present in the data.

## Usage

``` r
generate_report(
  ...,
  output_file = "NanoQuRe_Report.html",
  output_dir = getwd(),
  platform = "minion"
)
```

## Arguments

- ...:

  One or more data frames containing sequencing summaries, or paths to
  tab-separated sequencing summary files

- output_file:

  Name of the output HTML file (a file name without a directory
  component), default is "NanoQuRe_Report.html"

- output_dir:

  Directory the rendered report is written to. Defaults to the current
  working directory.

- platform:

  Flow cell platform forwarded to
  [`pore_activity_heatmap`](https://LRB-IIMCB.github.io/NanoQuRe/reference/pore_activity_heatmap.md):
  `"minion"` or `"promethion"`. Defaults to `"minion"`.

## Value

Path to the rendered HTML report

## Details

Every input must contain the columns `run_id`, `channel`, `start_time`,
`duration`, `sequence_length_template`, `mean_qscore_template`, and
`passes_filtering`. Only these columns and `sample_id` are read from a
file. Raw Guppy or Dorado sequencing summaries do not contain
`sample_id`: for a file without this column the file name (without
directory and extension) is used as the sample identifier. A data frame
input must contain `sample_id`, as there is no file name to fall back
on. The `passes_filtering` column may be logical, TRUE/FALSE text or
1/0. A sample spanning several `run_id` values triggers a warning,
because reads from different runs share one time axis in the plots.

## Examples

``` r
if (FALSE) { # \dontrun{
generate_report(sample_data, output_file = "QC_Report.html")
} # }
```
