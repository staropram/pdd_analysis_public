knit_here <- function() {
   out <- rmarkdown::render("drug_treatment_output.Rmd", envir = globalenv())
   browseURL(out)
   out
}
knit_here()