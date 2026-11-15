# NanoQuRe 1.2.0

## Breaking changes

* **`quality_stats()` and `sequencing_stats()`** return the columns
  `mean Q-score` and `average speed [nt/s]` instead of `mean qscore` and
  `average speed [bp/s]`.
* **`generate_report()`** stops when `output_file` contains a directory
  component; `output_dir` should be used instead.
* **Error messages** of all exported functions are reworded and missing
  columns are reported together (`Required columns missing: ...`).

## Reports

* **`generate_report()`** reads only the columns used by the report and takes
  `sample_id` from the file name when a summary file lacks this column.
* **`generate_report()`** accepts `passes_filtering` stored as TRUE/FALSE text
  or 1/0, validates `platform`, `output_file`, and `output_dir`, and warns
  when one `sample_id` spans several `run_id` values.
* **The single-sample report** shows a notice instead of failing when the
  pore activity heatmap cannot be drawn.

## Plots and tables

* **`plot_read_lengths()`** sizes linear bins to `upper_limit` and validates
  `upper_limit` and `y_limit`.
* **`plot_quality_distribution()`** draws its reference lines up to the
  tallest bar.
* **`plot_active_channels()`** validates `thresholds` and ignores the final,
  usually partial, window when searching for threshold crossings.
* **`plot_double()` and `plot_cumulative_yield()`** no longer require columns
  they do not use.
* **Missing values** in lengths, Q-scores, and durations no longer propagate
  into plots and summary tables.
* **Units and terminology** are unified: `nt`, `nt/s`, and `Q-score`.

## Dependencies

* **`vroom` replaces `readr`**, and `data.table` is no longer required.
* **`stats`, `utils`, and `htmltools`** are declared in Imports.

---

# NanoQuRe 1.1.0

*Released 2026-11-09.*

## Data integrity

* **`pore_activity_heatmap()`** places channels at their correct flow cell
  position, keeps every base in the time panel, and stops on channel numbers
  outside the selected platform.
* **`plot_quality_distribution()`** shows reads of any Q-score; reads above
  Q15 were previously hidden.
* **`plot_double()`** reports the true minimum read length.
* **`plot_seq_throughput()` and `plot_average_speed()`** show hours without
  reads as zero yield and as gaps, instead of joining them with a line.
* **`generate_report()`** stops on missing `sample_id` or `passes_filtering`
  values and reads identifiers as text.

## Reports and plots

* **`generate_report()`** works from read-only libraries.
* **Report captions** describe each plot and give values computed per sample,
  without fixed conclusions.
* **`plot_read_lengths()`** draws a binned histogram on a log10 axis by
  default; new `weight = "bases"` and `log_x` arguments.
* **`plot_active_channels()`** counts channels with a read in each 10-minute
  window; new `bin_width` argument.
* **The unused `scales` dependency is removed.**

---

# NanoQuRe 1.0

*Released 2026-06-25.*

* First release.
