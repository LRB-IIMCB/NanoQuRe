
generate_report <- function(seq_summary){
  
  template <- system.file("inst", "rmd", "report_single.Rmd", package = "NanoQuRe")
  
  output <- rmarkdown::render()
  return(output)
  
  
}