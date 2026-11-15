# Getting Started with NanoQuRe

## Overview

NanoQuRe provides quality control tools for Oxford Nanopore Technologies
(ONT) sequencing data. Starting from a sequencing summary file produced
by an ONT basecaller (Guppy or Dorado), the package generates
interactive plots and summary tables, and can bundle them into a
self-contained HTML report with a single function call.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("LRB-IIMCB/NanoQuRe")
```

## Input data

NanoQuRe works on the per-read columns of a sequencing summary. The
following columns are required for the complete report:

| Column | Type | Description |
|----|----|----|
| `sample_id` | character | Sample identifier (absent from raw summaries) |
| `run_id` | character | Run identifier |
| `channel` | integer | Channel number |
| `start_time` | numeric | Read start time in seconds |
| `duration` | numeric | Read duration in seconds |
| `sequence_length_template` | numeric | Read length in nt |
| `mean_qscore_template` | numeric | Mean Q-score of the read |
| `passes_filtering` | logical | Whether the read passed the basecaller filter |

[`generate_report()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/generate_report.md)
accepts either a path to a tab-separated summary file or a data frame.
From a file, only the columns listed above are read. If the file has no
`sample_id` column, the file name (without directory and extension) is
used as the sample identifier, and `passes_filtering` may be stored as a
logical value, as TRUE/FALSE text, or as 1/0.

The individual plotting and table functions take a data frame. In that
case `sample_id` has to be added by the user and `passes_filtering` has
to be logical:

``` r
seq_summary <- vroom::vroom("sequencing_summary.txt", delim = "\t",
                            show_col_types = FALSE)

# raw summaries carry no sample identifier
seq_summary$sample_id <- "my_sample"

# the pass/fail flag may be read as text; the functions expect a logical column
seq_summary$passes_filtering <- as.logical(seq_summary$passes_filtering)
```

The examples below use the dataset bundled with the package (1000 reads
from a single MinION run):

``` r
library(NanoQuRe)

data(sample_data)
```

## Generating a full QC report

The main entry point is
[`generate_report()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/generate_report.md).
It counts the unique `sample_id` values in the input, selects the
single-sample or the multi-sample template accordingly, and renders a
self-contained HTML file.

``` r
generate_report(sample_data, output_file = "my_QC_report.html")
```

The report is written to `output_dir` (the working directory by default)
and its path is returned. `output_file` is a file name only; a directory
is given through `output_dir`. The `platform` argument (`"minion"`, the
default, or `"promethion"`) selects the flow cell layout of the pore
activity heatmap. Flongle flow cells are not supported.

``` r
# a raw summary file from a PromethION run; the sample is named after the file
generate_report("sequencing_summary_run42.txt",
                output_file = "run42_QC_report.html",
                output_dir = "reports",
                platform = "promethion")
```

## Multi-sample reports

Several data frames or file paths can be passed to
[`generate_report()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/generate_report.md),
which combines them. The multi-sample template is chosen when more than
one unique `sample_id` is present:

``` r
generate_report(sample_data_A, sample_data_B, output_file = "multi_QC_report.html")
```

A sample should contain reads from one run. If one `sample_id` spans
several `run_id` values,
[`generate_report()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/generate_report.md)
warns, because reads from different runs would share one time axis, and
the pore activity heatmap of that sample is replaced by a notice.

## What the report contains

The report bundles the output of the individual NanoQuRe functions
listed below, each followed by a short caption with values computed from
the data. The [Using Individual
Functions](https://LRB-IIMCB.github.io/NanoQuRe/articles/individual-functions.md)
vignette covers each function in detail.

| Function | What it shows |
|----|----|
| [`sequencing_stats()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/sequencing_stats.md) | Run-level summary table |
| [`quality_stats()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/quality_stats.md) | Quality metrics table (N50, mean Q-score, longest read) |
| [`plot_read_lengths()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/plot_read_lengths.md) | Read length distribution with mean and N50 lines |
| [`plot_cumulative_yield()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/plot_cumulative_yield.md) | Cumulative bases over time, pass and fail reads |
| [`plot_quality_distribution()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/plot_quality_distribution.md) | Q-score distribution, pass and fail reads |
| [`plot_average_speed()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/plot_average_speed.md) | Translocation speed \[nt/s\] per hour |
| [`plot_seq_throughput()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/plot_seq_throughput.md) | Yield per hour |
| [`plot_active_channels()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/plot_active_channels.md) | Channels producing reads in each time window |
| [`plot_double()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/plot_double.md) | Read length and Q-score per hour |
| [`pore_activity_heatmap()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/pore_activity_heatmap.md) | Per-channel yield over time and on the flow cell layout |

## Session info

``` r
sessionInfo()
#> R version 4.4.2 (2024-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.2 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/local/software/R/4.4.2/lib/R/lib/libRblas.so 
#> LAPACK: /usr/local/software/R/4.4.2/lib/R/lib/libRlapack.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
#>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
#>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
#> 
#> time zone: Europe/Warsaw
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] NanoQuRe_1.2.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] gtable_0.3.6        jsonlite_2.0.0      dplyr_1.2.1        
#>  [4] compiler_4.4.2      tidyselect_1.2.1    dichromat_2.0-1    
#>  [7] tidyr_1.3.2         jquerylib_0.1.4     systemfonts_1.3.2  
#> [10] scales_1.4.0        textshaping_1.0.5   yaml_2.3.12        
#> [13] fastmap_1.2.0       ggplot2_4.0.3       R6_2.6.1           
#> [16] generics_0.1.4      knitr_1.52          htmlwidgets_1.6.4  
#> [19] tibble_3.3.1        desc_1.4.3          RColorBrewer_1.1-3 
#> [22] bslib_0.12.0        pillar_1.11.1       rlang_1.3.0        
#> [25] DT_0.34.0           cachem_1.1.0        xfun_0.60          
#> [28] S7_0.2.2            fs_2.1.0            sass_0.4.10        
#> [31] otel_0.2.0          viridisLite_0.4.3   plotly_4.12.1      
#> [34] cli_3.6.6           pkgdown_2.2.1       magrittr_2.0.5     
#> [37] digest_0.6.39       grid_4.4.2          rstudioapi_0.19.0  
#> [40] lifecycle_1.0.5     vctrs_0.7.3         data.table_1.18.6.1
#> [43] evaluate_1.0.5      glue_1.8.1          farver_2.1.2       
#> [46] ragg_1.5.2          purrr_1.2.2         httr_1.4.9         
#> [49] rmarkdown_2.32      tools_4.4.2         pkgconfig_2.0.3    
#> [52] htmltools_0.5.9
```
