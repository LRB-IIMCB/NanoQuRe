# NanoQuRe (development version)

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
