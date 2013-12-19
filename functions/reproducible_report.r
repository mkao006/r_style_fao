# Produce reports with knitr

library(knitr) # Create documents with R
library(plyr) # Manipulate data
library(stringr) # Manipulate strings
library(ggplot2) # Draw plots
library(pander)  # Create tables for RMarkdown document

# Function for creating markdown tables
rmd.table <- function(t) {
  require(pander)
  pandoc.table(t, style = 'rmarkdown', split.tables = Inf,
               split.cells = Inf)
}

# Example data.frame

df <- data.frame(country = c('Canada', 'Russia', 'Italy'),
                 value = 1:30, smoke = c('yes', 'no'))


# Run procedure of creating report for every country in our data.frame

d_ply(df, .(country), function(data) {
  knit2html("report_template.rmd", str_c(data$country[1], '.html'))
})
