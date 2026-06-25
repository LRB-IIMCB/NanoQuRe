# Generate Report

Generates an HTML QC report from one or more sequencing summary
dataframes. Automatically selects a single-sample or multi-sample
template based on the number of unique sample IDs present in the data.

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

  One or more dataframes containing sequencing summaries

- output_file:

  Name of the output HTML file, default is "NanoQuRe_Report.html"

- output_dir:

  Directory the rendered report is written to. Defaults to the current
  working directory.

- platform:

  Flowcell platform forwarded to
  [`pore_activity_heatmap`](https://LRB-IIMCB.github.io/NanoQuRe/reference/pore_activity_heatmap.md):
  `"minion"` or `"promethion"`. Defaults to `"minion"`.

## Value

Path to the rendered HTML report

## Examples

``` r
if (FALSE) { # \dontrun{
generate_report(sample_data, output_file = "QC_Report.html")
} # }
```
