<!-- badges: start -->
<a href="https://github.com/LRB-IIMCB/NanoQuRe/releases/"><img src="https://img.shields.io/github/tag/LRB-IIMCB/NanoQuRe?include_prereleases=&sort=semver&color=612a78" alt="GitHub tag"></a>
<a href="https://github.com/LRB-IIMCB/NanoQuRe/blob/master/LICENSE"><img src="https://img.shields.io/badge/License-MIT-612a78" alt="License"></a>
<img src="https://img.shields.io/badge/maintained-yes-612a78" alt="maintained - yes">
[![pkgdown](https://img.shields.io/badge/docs-pkgdown-612a78)](https://LRB-IIMCB.github.io/NanoQuRe/)
[![Codecov test coverage](https://codecov.io/gh/LRB-IIMCB/NanoQuRe/graph/badge.svg)](https://app.codecov.io/gh/LRB-IIMCB/NanoQuRe)
<!-- badges: end -->


# NanoQuRe <img src="man/figures/logo.png" align="right" height="139" alt="" />

**An R package for quality control of Oxford Nanopore sequencing data**

## Introduction

**NanoQuRe** reads sequencing summary files produced by ONT basecallers (Guppy or Dorado) and generates interactive plots, summary tables, and a self-contained HTML QC report, all from a single function call.

| What you get | Function |
|---|---|
| Full HTML QC report (auto single/multi-sample layout) | `generate_report()` |
| Run-level summary table | `sequencing_stats()` |
| Quality metrics table (N50, mean Q, longest read) | `quality_stats()` |
| Cumulative yield / throughput / speed plots | `plot_cumulative_yield()`, `plot_seq_throughput()`, `plot_average_speed()` |
| Read length & Q-score distribution plots | `plot_read_lengths()`, `plot_quality_distribution()` |
| Channel activity & pore heatmap plots | `plot_active_channels()`, `pore_activity_heatmap()`, `plot_double()` |

Unlike packages built around external basecallers/ML toolchains, NanoQuRe is a pure R package with no compiled code and no external system dependencies.


## Documentation

**Full documentation:** https://LRB-IIMCB.github.io/NanoQuRe/


## Installation

NanoQuRe is not currently available on CRAN. Install it using `devtools`:

```r
install.packages("devtools")
devtools::install_github("LRB-IIMCB/NanoQuRe")
library(NanoQuRe)
```

No compilation step and no external tools required; installation should take just a few seconds on any platform.


## Usage

The quickest path is a single call on the bundled example dataset:

```r
library(NanoQuRe)
data(sample_data)

generate_report(sample_data, output_file = "QC_report.html")
```

This renders a self-contained HTML report, covering read length and quality distributions, cumulative yield, throughput, translocation speed, and channel activity, to your current working directory (or wherever you point `output_dir`).

Every plot and table behind the report is also available standalone, if you want to embed one in your own analysis or R Markdown document:

```r
plot_read_lengths(sample_data)
quality_stats(sample_data)
```

See the [Getting Started](https://LRB-IIMCB.github.io/NanoQuRe/articles/getting-started.html) and [Using Individual Functions](https://LRB-IIMCB.github.io/NanoQuRe/articles/individual-functions.html) vignettes for the full walkthrough.


## Input data

NanoQuRe expects a data frame with eight columns: `run_id`, `channel`, `start_time`, `duration`, `sequence_length_template`, `mean_qscore_template`, and `passes_filtering` (all present in Guppy and Dorado sequencing summaries), plus `sample_id`. Raw basecaller summaries do not contain `sample_id`: a data frame needs it added by the user (one value per sample), whereas for a file without this column `generate_report()` uses the file name (without directory and extension) as the sample identifier. `generate_report()` accepts the data either as an in-memory data frame or as a path to a tab-separated summary file, from which only the eight columns above are read; `passes_filtering` may be logical, TRUE/FALSE text, or 1/0 when passed to `generate_report()`.


## Important notes

- `pore_activity_heatmap()` (and `generate_report()`, which calls it) takes a `platform` argument: `"minion"` (512 channels, default) or `"promethion"` (3000 channels). Channel numbers outside the selected layout stop the heatmap, whereas MinION data drawn with `"promethion"` would be placed on the wrong layout without an error.
- Multi-sample reports render each sample's pore heatmap independently; a single problematic sample (e.g. spanning more than one `run_id`) is reported inline rather than failing the whole report. A single-sample report likewise shows an inline notice instead of failing when the heatmap cannot be drawn.
- Only the MinION and PromethION layouts are supported; Flongle flow cells (126 channels) are not.
- `generate_report()` warns when one `sample_id` spans several `run_id` values.


## Troubleshooting

If you encounter a bug, please [open an issue](https://github.com/LRB-IIMCB/NanoQuRe/issues) on GitHub, ideally with a minimal reproducible example (a handful of rows of sequencing summary data is usually enough).


## Maintainer

Any issues regarding **NanoQuRe** should be addressed to Natalia Gumińska (nguminska (at) iimcb.gov.pl).

**NanoQuRe** was developed in the <a href="https://www.iimcb.gov.pl/en/research/41-laboratory-of-rna-biology-era-chairs-group">Laboratory of RNA Biology</a> at the <a href="https://www.iimcb.gov.pl/en/">International Institute of Molecular and Cell Biology</a> in Warsaw.
