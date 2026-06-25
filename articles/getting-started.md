# Getting Started with NanoQuRe

## Overview

NanoQuRe provides quality control tools for Oxford Nanopore sequencing
data. Starting from a sequencing summary file produced by ONT
basecallers (Guppy or Dorado), NanoQuRe generates interactive plots and
summary tables — and can bundle everything into a self-contained HTML
report with a single function call.

## Installation

``` r

# install.packages("devtools")
devtools::install_github("All-ice0/NanoQuRe")
```

## Input data

NanoQuRe expects a data frame with the columns produced by ONT
basecallers. The minimum required columns are:

| Column | Type | Description |
|----|----|----|
| `sample_id` | character | Sample identifier |
| `run_id` | character | Run identifier |
| `channel` | integer | Pore channel number |
| `start_time` | numeric | Read start time in seconds |
| `duration` | numeric | Read duration in seconds |
| `sequence_length_template` | numeric | Read length in bases |
| `mean_qscore_template` | numeric | Mean Q score of the read |
| `passes_filtering` | logical | Whether the read passed basecaller filters |

Loading your own file is straightforward:

``` r

seq_summary <- read.csv("sequencing_summary.txt", sep = "\t")

# passes_filtering comes in as a string from some basecallers — coerce if needed
seq_summary$passes_filtering <- as.logical(seq_summary$passes_filtering)
```

For this vignette we use the example dataset bundled with the package:

``` r

library(NanoQuRe)

data(sample_data)
```

## Generating a full QC report

The main entry point is
[`generate_report()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/generate_report.md).
It inspects how many unique `sample_id` values are in your data and
automatically picks the right template — single-sample or multi-sample —
then renders a self-contained HTML file.

``` r

generate_report(sample_data, output_file = "my_QC_report.html")
```

The rendered file will open in your browser automatically and contains
all plots and summary tables covering read quality, yield, throughput,
and pore activity.

## Multi-sample reports

If you have multiple samples, bind their data frames together before
calling
[`generate_report()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/generate_report.md).
The function detects more than one unique `sample_id` and switches to
the multi-sample template automatically:

``` r

generate_report(sample_data_A, sample_data_B, output_file = "multi_QC_report.html")
```

## What the report contains

The report bundles the output of all individual NanoQuRe functions. If
you want to explore or customise any single plot, the [Using Individual
Functions](https://LRB-IIMCB.github.io/NanoQuRe/articles/individual-functions.md)
vignette covers each one in detail.

| Function | What it shows |
|----|----|
| [`sequencing_stats()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/sequencing_stats.md) | Run-level summary table |
| [`quality_stats()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/quality_stats.md) | Quality metrics table (N50, mean Q, longest read) |
| [`plot_cumulative_yield()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/plot_cumulative_yield.md) | Cumulative bases over time, pass vs fail |
| [`plot_seq_throughput()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/plot_seq_throughput.md) | Yield per hour over time |
| [`plot_average_speed()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/plot_average_speed.md) | Translocation speed (bp/s) over time |
| [`plot_read_lengths()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/plot_read_lengths.md) | Read length distribution with mean and N50 lines |
| [`plot_quality_distribution()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/plot_quality_distribution.md) | Q score distribution with pass/fail split |
| [`plot_active_channels()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/plot_active_channels.md) | Channel activity decay over time |
| [`pore_activity_heatmap()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/pore_activity_heatmap.md) | Per-channel activity heatmap on flowcell layout |

## Session info

``` r

sessionInfo()
#> R version 4.6.1 (2026-06-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] NanoQuRe_1.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] gtable_0.3.6       jsonlite_2.0.0     dplyr_1.2.1        compiler_4.6.1    
#>  [5] tidyselect_1.2.1   tidyr_1.3.2        jquerylib_0.1.4    scales_1.4.0      
#>  [9] systemfonts_1.3.2  textshaping_1.0.5  yaml_2.3.12        fastmap_1.2.0     
#> [13] ggplot2_4.0.3      R6_2.6.1           generics_0.1.4     knitr_1.51        
#> [17] htmlwidgets_1.6.4  tibble_3.3.1       desc_1.4.3         RColorBrewer_1.1-3
#> [21] bslib_0.11.0       pillar_1.11.1      rlang_1.2.0        DT_0.34.0         
#> [25] cachem_1.1.0       xfun_0.59          S7_0.2.2           fs_2.1.0          
#> [29] sass_0.4.10        lazyeval_0.2.3     otel_0.2.0         viridisLite_0.4.3 
#> [33] plotly_4.12.0      cli_3.6.6          pkgdown_2.2.0      magrittr_2.0.5    
#> [37] digest_0.6.39      grid_4.6.1         lifecycle_1.0.5    vctrs_0.7.3       
#> [41] evaluate_1.0.5     glue_1.8.1         data.table_1.18.4  farver_2.1.2      
#> [45] ragg_1.5.2         purrr_1.2.2        httr_1.4.8         rmarkdown_2.31    
#> [49] tools_4.6.1        pkgconfig_2.0.3    htmltools_0.5.9
```
