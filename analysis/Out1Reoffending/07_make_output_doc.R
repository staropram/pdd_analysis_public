knit_here <- function() {
   out <- rmarkdown::render("reoffending_output.Rmd", envir = globalenv())
   browseURL(out)
   out
}
knit_here()