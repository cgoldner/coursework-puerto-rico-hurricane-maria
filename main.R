## load packages

library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits

## load PDF data

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

## extract text from pdf per page

text <- pdf_text(fn)

## split text per page into lines

x <- str_split(text[9], "\n")
s <- x[[1]]
s <- str_trim(s)

## read header of table

header_index <- str_which(s, "2015")[1]

header <- s[header_index]
month <- str_split(header, "\\s+", simplify = TRUE)
header <- month[, 2:5]
month <- month[,1]

## locate lines relevant for table

tail_index <- str_which(s, "Total")
tail_index
n <- lengths(str_extract_all(s, "\\d+"))
#length(n[n==1])
#which(n == 1)

## save table into data frame

ind <- setdiff((header_index+1):(tail_index-1), which(n==1))
s <- s[ind]
s <- str_remove_all(s, "[^\\d\\s]")
class(s)
s <- str_split_fixed(s, "\\s+", n = 6)[, 1:5]
s <- as.data.frame(s)
s <- s %>% mutate_all(parse_number)

header <- c("day", header)
s <- setNames(s, header)
col_month <- rep(month, nrow(s))
s$month <- col_month
tab <- s[, c("month", header)]

## calculate means relevant for comparison befor and after hurricane

mean(tab$"2015")
mean(tab$"2016")
mean(tab$"2017"[1:19])
mean(tab$"2017"[20:30])

## tidy up the table

tab <- tab %>% gather(year, deaths, -day, -month)

## save tidy table

save(tab, file = "rda/mortality-rep-sep.rda")

## create plot that shows deaths per day and when the hurricane hit

tab %>% filter(year != 2018) %>%
  ggplot(aes(day, deaths, color = year)) +
  geom_line() +
  geom_vline(xintercept = 20)

