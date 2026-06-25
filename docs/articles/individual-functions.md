# Using Individual Functions

## Overview

While
[`generate_report()`](https://LRB-IIMCB.github.io/NanoQuRe/reference/generate_report.md)
bundles everything into one HTML file, every plot and table in NanoQuRe
is also available as a standalone function. This is useful when you want
to embed a specific plot in your own R Markdown report, tweak
parameters, or inspect a single metric interactively.

``` r
library(NanoQuRe)

data(sample_data)
```

------------------------------------------------------------------------

## Summary tables

### `sequencing_stats()`

Returns a one-row summary of the run: duration, total bases sequenced,
pass rate, and average translocation speed.

``` r
sequencing_stats(sample_data)
#> # A tibble: 1 × 6
#>   `sample id`     `duration [h]` `number of reads` `total bases sequenced [Gb]`
#>   <chr>                    <dbl>             <int>                        <dbl>
#> 1 experiment_0001           88.6              1000                        0.001
#> # ℹ 2 more variables: `passed reads [%]` <dbl>, `average speed [bp/s]` <dbl>
```

### `quality_stats()`

Returns key quality metrics: total and passed read counts, N50, mean Q
score, longest read, and mean length of passed reads.

``` r
quality_stats(sample_data)
#> # A tibble: 1 × 7
#>   `sample id`     `all reads` `passed reads` `n50 value` `mean qscore`
#>   <chr>                 <int>          <int>       <dbl>         <dbl>
#> 1 experiment_0001        1000            885        1201          10.9
#> # ℹ 2 more variables: `longest read` <dbl>, `passed mean length` <dbl>
```

------------------------------------------------------------------------

## Yield plots

### `plot_cumulative_yield()`

Shows how total sequenced bases accumulate over the run, split by
pass/fail status. A flattening curve indicates the run is winding down.

``` r
plot_cumulative_yield(sample_data)
```

### `plot_seq_throughput()`

Shows yield per hour rather than cumulative yield — useful for spotting
periods of low pore activity or blockages during the run.

``` r
plot_seq_throughput(sample_data)
```

------------------------------------------------------------------------

## Speed

### `plot_average_speed()`

Shows the average translocation speed of DNA through the pore in bases
per second, split by pass/fail. A drop in speed can indicate pore aging
or buffer issues.

``` r
plot_average_speed(sample_data)
```

------------------------------------------------------------------------

## Read length distribution

### `plot_read_lengths()`

Plots the read length distribution with vertical lines for the mean read
length and the N50 value. The `upper_limit` parameter controls the x
axis maximum — useful when a small number of very long reads would
compress the rest of the distribution.

``` r
# default upper limit is 4000 bp
plot_read_lengths(sample_data)
```

``` r
# zoom out to see the full long-read tail
plot_read_lengths(sample_data, upper_limit = 20000)
```

------------------------------------------------------------------------

## Quality distribution

### `plot_quality_distribution()`

Plots the Q score distribution split by pass/fail. Two vertical lines
are shown: the basecaller’s own pass/fail threshold (derived from the
data) and a user-defined cutoff for downstream analysis.

``` r
# default user-defined cutoff is Q7
plot_quality_distribution(sample_data, qscore_cutoff = 7)
```

------------------------------------------------------------------------

## Channel activity

### `plot_active_channels()`

Shows how many pore channels remain active over the course of the run. A
steep early drop suggests many pores became blocked or inactive quickly.

``` r
plot_active_channels(sample_data)
```

### `pore_activity_heatmap()`

Shows two complementary views of pore activity: a time × channel heatmap
and a spatial heatmap overlaid on the flowcell layout. Darker colour
means more bases sequenced through that channel.

``` r
pore_activity_heatmap(sample_data, platform = "minion")
```

------------------------------------------------------------------------

## N50 directly

### `calculate_n50()`

If you just need the N50 value without any plot:

``` r
calculate_n50(sample_data)
#> [1] 1201
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
#> [1] NanoQuRe_0.1.1
#> 
#> loaded via a namespace (and not attached):
#>  [1] plotly_4.10.4       sass_0.4.10         utf8_1.2.6         
#>  [4] generics_0.1.4      tidyr_1.3.1         digest_0.6.39      
#>  [7] magrittr_2.0.4      evaluate_1.0.5      grid_4.4.2         
#> [10] RColorBrewer_1.1-3  fastmap_1.2.0       jsonlite_2.0.0     
#> [13] httr_1.4.7          purrr_1.2.1         crosstalk_1.2.2    
#> [16] viridisLite_0.4.2   scales_1.4.0        lazyeval_0.2.2     
#> [19] textshaping_1.0.4   jquerylib_0.1.4     cli_3.6.5          
#> [22] rlang_1.1.7         withr_3.0.2         cachem_1.1.0       
#> [25] yaml_2.3.12         otel_0.2.0          tools_4.4.2        
#> [28] dplyr_1.1.4         ggplot2_4.0.1       DT_0.34.0          
#> [31] vctrs_0.7.1         R6_2.6.1            lifecycle_1.0.5    
#> [34] fs_1.6.6            htmlwidgets_1.6.4   ragg_1.5.0         
#> [37] pkgconfig_2.0.3     desc_1.4.3          pkgdown_2.2.0      
#> [40] pillar_1.11.1       bslib_0.10.0        gtable_0.3.6       
#> [43] data.table_1.18.2.1 glue_1.8.0          systemfonts_1.3.1  
#> [46] xfun_0.56           tibble_3.3.1        tidyselect_1.2.1   
#> [49] rstudioapi_0.18.0   knitr_1.51          dichromat_2.0-0.1  
#> [52] farver_2.1.2        htmltools_0.5.9     rmarkdown_2.30     
#> [55] compiler_4.4.2      S7_0.2.1
```
