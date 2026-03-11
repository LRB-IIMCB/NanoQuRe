
generate_report <- function(seq_summary, output_file = "NanoQuRe_Report.html"){
  
  #template <- system.file("rmd", "report_single.Rmd", package = "NanoQuRe")
  template <- "inst/rmd/report_single.Rmd"
  
  output <- rmarkdown::render(
    input = template,
    output_file = output_file,
    params = list(data = seq_summary), # This sends your data to the Rmd 'params'
    envir = new.env(parent = globalenv()) # Runs in a clean environment to avoid conflicts
  )
  
  return(output)
  
}