
basic_table <- function(ligase_data){
  ligase_data %>%
  summarise(
  "run id" = first(run_id),
  "duration" = max(start_time + duration, na.rm = TRUE) / 3600,
  "number of reads" = n()
  )
}

pro_table <- basic_table(ligase_data)

