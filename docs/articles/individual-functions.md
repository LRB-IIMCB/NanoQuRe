# Using Individual Functions

## Overview

[`generate_report()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/generate_report.md)
bundles everything into one HTML file, but every plot and table in
NanoQuRe is also available as a standalone function. This is useful for
embedding a specific plot in an R Markdown document, adjusting its
parameters, or inspecting a single metric interactively.

Each function takes a data frame with the reads of one sample and stops
with a message naming the missing columns when the input is incomplete.
The `passes_filtering` column has to be logical here; only
[`generate_report()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/generate_report.md)
converts text or 1/0 flags.

``` r
library(NanoQuRe)

data(sample_data)
```

------------------------------------------------------------------------

## Summary tables

### `sequencing_stats()`

Returns a one-row summary of the run: duration, number of reads, total
bases sequenced, percentage of passed reads, and average translocation
speed \[nt/s\]. The speed is the total read length divided by the summed
read duration.

``` r
sequencing_stats(sample_data)
#> # A tibble: 1 × 6
#>   `sample id`     `duration [h]` `number of reads` `total bases sequenced [Gb]`
#>   <chr>                    <dbl>             <int>                        <dbl>
#> 1 experiment_0001           88.6              1000                        0.001
#> # ℹ 2 more variables: `passed reads [%]` <dbl>, `average speed [nt/s]` <dbl>
```

### `quality_stats()`

Returns the main quality metrics: total and passed read counts, N50,
mean Q-score, longest read, and mean length of passed reads. Missing
values are ignored in both tables.

``` r
quality_stats(sample_data)
#> # A tibble: 1 × 7
#>   `sample id`     `all reads` `passed reads` `n50 value` `mean Q-score`
#>   <chr>                 <int>          <int>       <dbl>          <dbl>
#> 1 experiment_0001        1000            885        1201           10.9
#> # ℹ 2 more variables: `longest read` <dbl>, `passed mean length` <dbl>
```

### `calculate_n50()`

Returns the N50 value alone, without a table or a plot:

``` r
calculate_n50(sample_data)
#> [1] 1201
```

------------------------------------------------------------------------

## Yield and throughput

### `plot_cumulative_yield()`

Shows how the sequenced bases accumulate over the run, separately for
pass and fail reads. A flattening curve suggests that output is
declining.

``` r
plot_cumulative_yield(sample_data)
```

### `plot_seq_throughput()`

Shows the yield per hour instead of the cumulative yield. Each read is
assigned to the hour in which it started, and hours without reads are
shown as zero yield. The plot may help to spot periods of low output
during the run.

``` r
plot_seq_throughput(sample_data)
```

### `plot_average_speed()`

Shows the translocation speed per hour in nt/s (summed read length
divided by summed read duration), separately for pass and fail reads.
Hours without reads appear as gaps in the line. A drop in speed can
indicate pore ageing or buffer depletion, although the summary alone
does not identify the cause.

``` r
plot_average_speed(sample_data)
```

------------------------------------------------------------------------

## Read length and quality

### `plot_read_lengths()`

Plots a histogram of read lengths on a log10 axis, with vertical lines
for the mean read length and the N50 value. By default each bar is the
number of reads in a length bin; `weight = "bases"` shows the number of
bases instead, i.e. where the sequenced bases are. `upper_limit` sets
the x-axis maximum; reads beyond it are counted in a note on the plot.
`log_x = FALSE` switches to a linear axis, on which the bins are sized
to the visible range.

``` r
plot_read_lengths(sample_data)
```

``` r
# bases per length bin, on a linear axis up to 20 kb
plot_read_lengths(sample_data, weight = "bases", log_x = FALSE, upper_limit = 20000)
```

### `plot_quality_distribution()`

Plots the Q-score distribution of pass and fail reads over the full
range of scores. Up to two vertical lines are shown: a user-defined
cut-off for downstream analysis (`qscore_cutoff`, Q7 by default) and,
only when both pass and fail reads exist, the pass/fail threshold of the
basecaller as derived from the data.

``` r
plot_quality_distribution(sample_data, qscore_cutoff = 7)
```

### `plot_double()`

Returns a named list of two plots, `length_plot` and `qscore_plot`, with
the minimum, mean, and maximum read length and Q-score per hour of the
run.

``` r
double_plots <- plot_double(sample_data)
double_plots$length_plot
```

``` r
double_plots$qscore_plot
```

------------------------------------------------------------------------

## Channel activity

### `plot_active_channels()`

Shows how many channels produced at least one read in each time window
(10 minutes by default, set with `bin_width` in hours). Dashed lines
mark the first window in which this number falls to 50% and 25% of the
highest count in the first hour (`thresholds`); the final, usually
partial, window is not considered. Pore scans and pauses are not
recorded in the sequencing summary, so short dips or gaps can reflect
run control and not pore loss.

``` r
plot_active_channels(sample_data)
```

### `pore_activity_heatmap()`

Shows two complementary views of channel yield: a heatmap of channels
over time (`bin_width` in hours, 1 by default) and a heatmap on the flow
cell layout. A darker colour means more bases sequenced through that
channel. `platform` selects the layout: `"minion"` (512 channels,
default) or `"promethion"` (3000 channels). The input has to contain a
single `run_id`, channel numbers outside the selected layout stop the
function, and Flongle flow cells are not supported.

``` r
pore_activity_heatmap(sample_data, platform = "minion")
```

------------------------------------------------------------------------

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
#> [25] utf8_1.2.6          DT_0.34.0           cachem_1.1.0       
#> [28] xfun_0.60           S7_0.2.2            fs_2.1.0           
#> [31] sass_0.4.10         otel_0.2.0          viridisLite_0.4.3  
#> [34] plotly_4.12.1       cli_3.6.6           withr_3.0.3        
#> [37] pkgdown_2.2.1       magrittr_2.0.5      crosstalk_1.2.2    
#> [40] digest_0.6.39       grid_4.4.2          rstudioapi_0.19.0  
#> [43] lifecycle_1.0.5     vctrs_0.7.3         data.table_1.18.6.1
#> [46] evaluate_1.0.5      glue_1.8.1          farver_2.1.2       
#> [49] ragg_1.5.2          purrr_1.2.2         httr_1.4.9         
#> [52] rmarkdown_2.32      tools_4.4.2         pkgconfig_2.0.3    
#> [55] htmltools_0.5.9
```
