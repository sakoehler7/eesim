#' Script for scraping CDC data
#'
library(rvest)
library(purrr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)

## For 2017

i <- str_pad(1:14, pad = "0", width = 2)
url <- i %>% map(~paste("https://wonder.cdc.gov/mmwr/mmwr_2017.asp?mmwr_year=2017&mmwr_week=", as.character(.x), "&mmwr_table=2J&request=Submit&mmwr_location=", sep=""))
html_2017 <- url %>% map(~read_html(.x))

week_end_date <- html_2017 %>%
  map(~html_nodes(.x, 'tr:nth-child(1) td:nth-child(1)')) %>%
  map(~html_text(.x)) %>%
  map(~ str_extract(.x[1], "week ending .+ \\(")) %>%
  map(~ str_replace(.x[1], "week ending", "")) %>%
  map(~ str_replace(.x[1], "\\(", "")) %>%
  map(~ str_trim(.x[1]))

nodes_midatlantic <- html_2017 %>% map(~html_nodes(.x, 'tr:nth-child(12) td:nth-child(2)'))
madat <- nodes_midatlantic %>% map(~html_text(.x))
as.numeric(madat)
checkma <- html_2017 %>% map(~html_nodes(.x, 'tr:nth-child(12) td:nth-child(1)')) %>% map(~html_text(.x)) %>% map(~str_trim(.x))

nodes_nyc <- html_2017 %>% map(~html_nodes(.x, 'tr:nth-child(15) td:nth-child(2)'))
nycdat <- nodes_nyc %>% map(~html_text(.x))
as.numeric(nycdat)
checknyc <- html_2017 %>% map(~html_nodes(.x, 'tr:nth-child(15) td:nth-child(1)')) %>% map(~html_text(.x)) %>% map(~str_trim(.x))


nodes_pa <- html_2017 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(2)'))
padat <- nodes_pa %>% map(~html_text(.x))
as.numeric(padat)
checkpa <- html_2017 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(1)')) %>% map(~html_text(.x)) %>% map(~str_trim(.x))

leg_cases_2017 <- data_frame(week_end_date = as.character(week_end_date),
                             midatlantic = as.numeric(madat),
                             nyc = as.numeric(nycdat),
                             penn = as.numeric(padat),
                             checkma = as.character(checkma),
                             checknyc=as.character(checknyc),
                             checkpa=as.character(checkpa)) %>%
  mutate(week_end_date = mdy(week_end_date))

## For 2016

i <- str_pad(1:52, pad = "0", width = 2)
url <- i %>% map(~paste("https://wonder.cdc.gov/mmwr/mmwr_2016.asp?mmwr_year=2016&mmwr_week=", as.character(.x), "&mmwr_table=2G&request=Submit&mmwr_location=", sep=""))
html_2016 <- url %>% map(~read_html(.x))

week_end_date <- html_2016 %>%
  map(~html_nodes(.x, 'tr:nth-child(1) td:nth-child(1)')) %>%
  map(~html_text(.x)) %>%
  map(~ str_extract(.x[1], "week ending .+ \\(")) %>%
  map(~ str_replace(.x[1], "week ending", "")) %>%
  map(~ str_replace(.x[1], "\\(", "")) %>%
  map(~ str_trim(.x[1]))

nodes_midatlantic <- html_2016 %>% map(~html_nodes(.x, 'tr:nth-child(13) td:nth-child(12)'))
madat <- nodes_midatlantic %>% map(~html_text(.x))
as.numeric(madat)
checkma <- html_2016 %>% map(~html_nodes(.x, 'tr:nth-child(13) td:nth-child(1)')) %>%  map(~html_text(.x)) %>% map(~str_trim(.x))

nodes_nyc <- html_2016 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(12)'))
nycdat <- nodes_nyc %>% map(~html_text(.x))
checknyc <- html_2016 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(1)')) %>%  map(~html_text(.x)) %>% map(~str_trim(.x))
as.numeric(nycdat)

nodes_pa <- html_2016 %>% map(~html_nodes(.x, 'tr:nth-child(17) td:nth-child(12)'))
padat <- nodes_pa %>% map(~html_text(.x))
checkpa <- html_2016 %>% map(~html_nodes(.x, 'tr:nth-child(17) td:nth-child(1)')) %>%  map(~html_text(.x)) %>% map(~str_trim(.x))
as.numeric(padat)

leg_cases_2016 <- data_frame(week_end_date = as.character(week_end_date),
                             midatlantic = as.numeric(madat),
                             nyc = as.numeric(nycdat),
                             penn = as.numeric(padat),
                             checkma = as.character(checkma),
                             checknyc=as.character(checknyc),
                             checkpa=as.character(checkpa)) %>%
  mutate(week_end_date = mdy(week_end_date))

## For 2015

i <- str_pad(1:52, pad = "0", width = 2)
url <- i %>% map(~paste("https://wonder.cdc.gov/mmwr/mmwr_2015.asp?mmwr_year=2015&mmwr_week=", as.character(.x), "&mmwr_table=2G&request=Submit&mmwr_location=", sep=""))
html_2015 <- url %>% map(~read_html(.x))

week_end_date <- html_2015 %>%
  map(~html_nodes(.x, 'tr:nth-child(1) td:nth-child(1)')) %>%
  map(~html_text(.x)) %>%
  map(~ str_extract(.x[1], "week ending .+ \\(")) %>%
  map(~ str_replace(.x[1], "week ending", "")) %>%
  map(~ str_replace(.x[1], "\\(", "")) %>%
  map(~ str_trim(.x[1]))

madat <- html_2015 %>% map(~html_nodes(.x, 'tr:nth-child(13) td:nth-child(12)')) %>% map(~html_text(.x)) %>% as.numeric()
checkma <- html_2015 %>% map(~html_nodes(.x, 'tr:nth-child(13) td:nth-child(1)')) %>% map(~html_text(.x)) %>% map(~str_trim(.x)) %>% as.character()

nycdat <- html_2015 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(12)')) %>% map(~html_text(.x)) %>% as.numeric()
checknyc <- html_2015 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

padat <- html_2015 %>% map(~html_nodes(.x, 'tr:nth-child(17) td:nth-child(12)')) %>% map(~html_text(.x)) %>% as.numeric()
checkpa <- html_2015 %>% map(~html_nodes(.x, 'tr:nth-child(17) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()



leg_cases_2015 <- data_frame(week_end_date = as.character(week_end_date),
                             midatlantic = madat,
                             nyc = nycdat,
                             penn = padat,
                             checkma = checkma,
                             checknyc = checknyc,
                             checkpa = checkpa) %>%
  mutate(week_end_date = mdy(week_end_date))

##For 2014

i <- str_pad(1:52, pad = "0", width = 2)
url <- i %>% map(~paste("https://wonder.cdc.gov/mmwr/mmwr_1995_2014.asp?mmwr_year=2014&mmwr_week=", as.character(.x), "&mmwr_table=2F&request=Submit&mmwr_location=", sep=""))
html_2014 <- url %>% map(~read_html(.x))

week_end_date <- html_2014 %>%
  map(~html_nodes(.x, 'tr:nth-child(1) td:nth-child(1)')) %>%
  map(~html_text(.x)) %>%
  map(~ str_extract(.x[1], "week ending .+ \\(")) %>%
  map(~ str_replace(.x[1], "week ending", "")) %>%
  map(~ str_replace(.x[1], "\\(", "")) %>%
  map(~ str_trim(.x[1]))

madat <- html_2014 %>% map(~html_nodes(.x, 'tr:nth-child(13) td:nth-child(12)')) %>% map(~html_text(.x)) %>% as.numeric()
checkma <- html_2014 %>% map(~html_nodes(.x, 'tr:nth-child(13) td:nth-child(1)')) %>% map(~html_text(.x)) %>% map(~str_trim(.x)) %>% as.character()

nycdat <- html_2014 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(12)')) %>% map(~html_text(.x)) %>% as.numeric()
checknyc <- html_2014 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

padat <- html_2014 %>% map(~html_nodes(.x, 'tr:nth-child(17) td:nth-child(12)')) %>% map(~html_text(.x)) %>% as.numeric()
checkpa <- html_2014 %>% map(~html_nodes(.x, 'tr:nth-child(17) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()



leg_cases_2014 <- data_frame(week_end_date = as.character(week_end_date),
                             midatlantic = madat,
                             nyc = nycdat,
                             penn = padat,
                             checkma = checkma,
                             checknyc = checknyc,
                             checkpa = checkpa) %>%
  mutate(week_end_date = mdy(week_end_date))

##2013

i <- str_pad(1:52, pad = "0", width = 2)
url <- i %>% map(~paste("https://wonder.cdc.gov/mmwr/mmwr_1995_2014.asp?mmwr_year=2013&mmwr_week=", as.character(.x), "&mmwr_table=2F&request=Submit&mmwr_location=", sep=""))
html_2013 <- url %>% map(~read_html(.x))

week_end_date <- html_2013 %>%
  map(~html_nodes(.x, 'tr:nth-child(1) td:nth-child(1)')) %>%
  map(~html_text(.x)) %>%
  map(~ str_extract(.x[1], "week ending .+ \\(")) %>%
  map(~ str_replace(.x[1], "week ending", "")) %>%
  map(~ str_replace(.x[1], "\\(", "")) %>%
  map(~ str_trim(.x[1]))

madat <- html_2013 %>% map(~html_nodes(.x, 'tr:nth-child(12) td:nth-child(2)')) %>% map(~html_text(.x)) %>% as.numeric()
checkma <- html_2013 %>% map(~html_nodes(.x, 'tr:nth-child(12) td:nth-child(1)')) %>% map(~html_text(.x)) %>% map(~str_trim(.x)) %>% as.character()

nycdat <- html_2013 %>% map(~html_nodes(.x, 'tr:nth-child(15) td:nth-child(2)')) %>% map(~html_text(.x)) %>% as.numeric()
checknyc <- html_2013 %>% map(~html_nodes(.x, 'tr:nth-child(15) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

padat <- html_2013 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(2)')) %>% map(~html_text(.x)) %>% as.numeric()
checkpa <- html_2013 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

leg_cases_2013 <- data_frame(week_end_date = as.character(week_end_date),
                             midatlantic = madat,
                             nyc = nycdat,
                             penn = padat,
                             checkma = checkma,
                             checknyc = checknyc,
                             checkpa = checkpa) %>%
  mutate(week_end_date = mdy(week_end_date))

##For 2012

i <- str_pad(1:52, pad = "0", width = 2)
url <- i %>% map(~paste("https://wonder.cdc.gov/mmwr/mmwr_1995_2014.asp?mmwr_year=2012&mmwr_week=", as.character(.x), "&mmwr_table=2F&request=Submit&mmwr_location=", sep=""))
html_2012 <- url %>% map(~read_html(.x))

week_end_date <- html_2012 %>%
  map(~html_nodes(.x, 'tr:nth-child(1) td:nth-child(1)')) %>%
  map(~html_text(.x)) %>%
  map(~ str_extract(.x[1], "week ending .+ \\(")) %>%
  map(~ str_replace(.x[1], "week ending", "")) %>%
  map(~ str_replace(.x[1], "\\(", "")) %>%
  map(~ str_trim(.x[1]))

madat <- html_2012 %>% map(~html_nodes(.x, 'tr:nth-child(12) td:nth-child(2)')) %>% map(~html_text(.x)) %>% as.numeric()
checkma <- html_2012 %>% map(~html_nodes(.x, 'tr:nth-child(12) td:nth-child(1)')) %>% map(~html_text(.x)) %>% map(~str_trim(.x)) %>% as.character()

nycdat <- html_2012 %>% map(~html_nodes(.x, 'tr:nth-child(15) td:nth-child(2)')) %>% map(~html_text(.x)) %>% as.numeric()
checknyc <- html_2012 %>% map(~html_nodes(.x, 'tr:nth-child(15) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

padat <- html_2012 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(2)')) %>% map(~html_text(.x)) %>% as.numeric()
checkpa <- html_2012 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

leg_cases_2012 <- data_frame(week_end_date = as.character(week_end_date),
                             midatlantic = madat,
                             nyc = nycdat,
                             penn = padat,
                             checkma = checkma,
                             checknyc = checknyc,
                             checkpa = checkpa) %>%
  mutate(week_end_date = mdy(week_end_date))

##For 2011

i <- str_pad(1:52, pad = "0", width = 2)
url <- i %>% map(~paste("https://wonder.cdc.gov/mmwr/mmwr_1995_2014.asp?mmwr_year=2011&mmwr_week=", as.character(.x), "&mmwr_table=2F&request=Submit&mmwr_location=", sep=""))
html_2011 <- url %>% map(~read_html(.x))

week_end_date <- html_2011 %>%
  map(~html_nodes(.x, 'tr:nth-child(1) td:nth-child(1)')) %>%
  map(~html_text(.x)) %>%
  map(~ str_extract(.x[1], "week ending .+ \\(")) %>%
  map(~ str_replace(.x[1], "week ending", "")) %>%
  map(~ str_replace(.x[1], "\\(", "")) %>%
  map(~ str_trim(.x[1]))

madat <- html_2011 %>% map(~html_nodes(.x, 'tr:nth-child(12) td:nth-child(2)')) %>% map(~html_text(.x)) %>% as.numeric()
checkma <- html_2011 %>% map(~html_nodes(.x, 'tr:nth-child(12) td:nth-child(1)')) %>% map(~html_text(.x)) %>% map(~str_trim(.x)) %>% as.character()

nycdat <- html_2011 %>% map(~html_nodes(.x, 'tr:nth-child(15) td:nth-child(2)')) %>% map(~html_text(.x)) %>% as.numeric()
checknyc <- html_2011 %>% map(~html_nodes(.x, 'tr:nth-child(15) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

padat <- html_2011 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(2)')) %>% map(~html_text(.x)) %>% as.numeric()
checkpa <- html_2011 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

leg_cases_2011 <- data_frame(week_end_date = as.character(week_end_date),
                             midatlantic = madat,
                             nyc = nycdat,
                             penn = padat,
                             checkma = checkma,
                             checknyc = checknyc,
                             checkpa = checkpa) %>%
  mutate(week_end_date = mdy(week_end_date))

##For 2010

i <- str_pad(1:52, pad = "0", width = 2)
url <- i %>% map(~paste("https://wonder.cdc.gov/mmwr/mmwr_1995_2014.asp?mmwr_year=2010&mmwr_week=", as.character(.x), "&mmwr_table=2F&request=Submit&mmwr_location=", sep=""))
html_2010 <- url %>% map(~read_html(.x))

week_end_date <- html_2010 %>%
  map(~html_nodes(.x, 'tr:nth-child(1) td:nth-child(1)')) %>%
  map(~html_text(.x)) %>%
  map(~ str_extract(.x[1], "week ending .+ \\(")) %>%
  map(~ str_replace(.x[1], "week ending", "")) %>%
  map(~ str_replace(.x[1], "\\(", "")) %>%
  map(~ str_trim(.x[1]))

madat <- html_2010 %>% map(~html_nodes(.x, 'tr:nth-child(12) td:nth-child(2)')) %>% map(~html_text(.x)) %>% as.numeric()
checkma <- html_2010 %>% map(~html_nodes(.x, 'tr:nth-child(12) td:nth-child(1)')) %>% map(~html_text(.x)) %>% map(~str_trim(.x)) %>% as.character()

nycdat <- html_2010 %>% map(~html_nodes(.x, 'tr:nth-child(15) td:nth-child(2)')) %>% map(~html_text(.x)) %>% as.numeric()
checknyc <- html_2010 %>% map(~html_nodes(.x, 'tr:nth-child(15) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

padat <- html_2010 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(2)')) %>% map(~html_text(.x)) %>% as.numeric()
checkpa <- html_2010 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

leg_cases_2010 <- data_frame(week_end_date = as.character(week_end_date),
                             midatlantic = madat,
                             nyc = nycdat,
                             penn = padat,
                             checkma = checkma,
                             checknyc = checknyc,
                             checkpa = checkpa) %>%
  mutate(week_end_date = mdy(week_end_date))

'#For 2009

i <- str_pad(1:52, pad = "0", width = 2)
url <- i %>% map(~paste("https://wonder.cdc.gov/mmwr/mmwr_1995_2014.asp?mmwr_year=2009&mmwr_week=", as.character(.x), "&mmwr_table=2F&request=Submit&mmwr_location=", sep=""))
html_2009 <- url %>% map(~read_html(.x))

week_end_date <- html_2009 %>%
  map(~html_nodes(.x, 'tr:nth-child(1) td:nth-child(1)')) %>%
  map(~html_text(.x)) %>%
  map(~ str_extract(.x[1], "week ending .+ \\(")) %>%
  map(~ str_replace(.x[1], "week ending", "")) %>%
  map(~ str_replace(.x[1], "\\(", "")) %>%
  map(~ str_trim(.x[1]))

madat <- html_2009 %>% map(~html_nodes(.x, 'tr:nth-child(12) td:nth-child(2)')) %>% map(~html_text(.x)) %>% as.numeric()
checkma <- html_2009 %>% map(~html_nodes(.x, 'tr:nth-child(12) td:nth-child(1)')) %>% map(~html_text(.x)) %>% map(~str_trim(.x)) %>% as.character()

nycdat <- html_2009 %>% map(~html_nodes(.x, 'tr:nth-child(15) td:nth-child(2)')) %>% map(~html_text(.x)) %>% as.numeric()
checknyc <- html_2009 %>% map(~html_nodes(.x, 'tr:nth-child(15) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

padat <- html_2009 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(2)')) %>% map(~html_text(.x)) %>% as.numeric()
checkpa <- html_2009 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

leg_cases_2009 <- data_frame(week_end_date = as.character(week_end_date),
                             midatlantic = madat,
                             nyc = nycdat,
                             penn = padat,
                             checkma = checkma,
                             checknyc = checknyc,
                             checkpa = checkpa) %>%
  mutate(week_end_date = mdy(week_end_date))

'#For 2008

i <- str_pad(1:52, pad = "0", width = 2)
url <- i %>% map(~paste("https://wonder.cdc.gov/mmwr/mmwr_1995_2014.asp?mmwr_year=2008&mmwr_week=", as.character(.x), "&mmwr_table=2C&request=Submit&mmwr_location=", sep=""))
html_2008 <- url %>% map(~read_html(.x))

week_end_date <- html_2008 %>%
  map(~html_nodes(.x, 'tr:nth-child(1) td:nth-child(1)')) %>%
  map(~html_text(.x)) %>%
  map(~ str_extract(.x[1], "week ending .+ \\(")) %>%
  map(~ str_replace(.x[1], "week ending", "")) %>%
  map(~ str_replace(.x[1], "\\(", "")) %>%
  map(~ str_trim(.x[1]))

madat <- html_2008 %>% map(~html_nodes(.x, 'tr:nth-child(13) td:nth-child(12)')) %>% map(~html_text(.x)) %>% as.numeric()
checkma <- html_2008 %>% map(~html_nodes(.x, 'tr:nth-child(13) td:nth-child(1)')) %>% map(~html_text(.x)) %>% map(~str_trim(.x)) %>% as.character()

nycdat <- html_2008 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(12)')) %>% map(~html_text(.x)) %>% as.numeric()
checknyc <- html_2008 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

padat <- html_2008 %>% map(~html_nodes(.x, 'tr:nth-child(17) td:nth-child(12)')) %>% map(~html_text(.x)) %>% as.numeric()
checkpa <- html_2008 %>% map(~html_nodes(.x, 'tr:nth-child(17) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

leg_cases_2008 <- data_frame(week_end_date = as.character(week_end_date),
                             midatlantic = madat,
                             nyc = nycdat,
                             penn = padat,
                             checkma = checkma,
                             checknyc = checknyc,
                             checkpa = checkpa) %>%
  mutate(week_end_date = mdy(week_end_date))

'#For 2007

i <- str_pad(1:52, pad = "0", width = 2)
url <- i %>% map(~paste("https://wonder.cdc.gov/mmwr/mmwr_1995_2014.asp?mmwr_year=2007&mmwr_week=", as.character(.x), "&mmwr_table=2C&request=Submit&mmwr_location=", sep=""))
html_2007 <- url %>% map(~read_html(.x))

week_end_date <- html_2007 %>%
  map(~html_nodes(.x, 'tr:nth-child(1) td:nth-child(1)')) %>%
  map(~html_text(.x)) %>%
  map(~ str_extract(.x[1], "week ending .+ \\(")) %>%
  map(~ str_replace(.x[1], "week ending", "")) %>%
  map(~ str_replace(.x[1], "\\(", "")) %>%
  map(~ str_trim(.x[1]))

madat <- html_2007 %>% map(~html_nodes(.x, 'tr:nth-child(13) td:nth-child(12)')) %>% map(~html_text(.x)) %>% as.numeric()
checkma <- html_2007 %>% map(~html_nodes(.x, 'tr:nth-child(13) td:nth-child(1)')) %>% map(~html_text(.x)) %>% map(~str_trim(.x)) %>% as.character()

nycdat <- html_2007 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(12)')) %>% map(~html_text(.x)) %>% as.numeric()
checknyc <- html_2007 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

padat <- html_2007 %>% map(~html_nodes(.x, 'tr:nth-child(17) td:nth-child(12)')) %>% map(~html_text(.x)) %>% as.numeric()
checkpa <- html_2007 %>% map(~html_nodes(.x, 'tr:nth-child(17) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

leg_cases_2007 <- data_frame(week_end_date = as.character(week_end_date),
                             midatlantic = madat,
                             nyc = nycdat,
                             penn = padat,
                             checkma = checkma,
                             checknyc = checknyc,
                             checkpa = checkpa) %>%
  mutate(week_end_date = mdy(week_end_date))

'#For 2006

i <- str_pad(1:52, pad = "0", width = 2)
url <- i %>% map(~paste("https://wonder.cdc.gov/mmwr/mmwr_1995_2014.asp?mmwr_year=2006&mmwr_week=", as.character(.x), "&mmwr_table=2C&request=Submit&mmwr_location=", sep=""))
html_2006 <- url %>% map(~read_html(.x))

week_end_date <- html_2006 %>%
  map(~html_nodes(.x, 'tr:nth-child(1) td:nth-child(1)')) %>%
  map(~html_text(.x)) %>%
  map(~ str_extract(.x[1], "week ending .+ \\(")) %>%
  map(~ str_replace(.x[1], "week ending", "")) %>%
  map(~ str_replace(.x[1], "\\(", "")) %>%
  map(~ str_trim(.x[1]))

madat <- html_2006 %>% map(~html_nodes(.x, 'tr:nth-child(13) td:nth-child(12)')) %>% map(~html_text(.x)) %>% as.numeric()
checkma <- html_2006 %>% map(~html_nodes(.x, 'tr:nth-child(13) td:nth-child(1)')) %>% map(~html_text(.x)) %>% map(~str_trim(.x)) %>% as.character()

nycdat <- html_2006 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(12)')) %>% map(~html_text(.x)) %>% as.numeric()
checknyc <- html_2006 %>% map(~html_nodes(.x, 'tr:nth-child(16) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

padat <- html_2006 %>% map(~html_nodes(.x, 'tr:nth-child(17) td:nth-child(12)')) %>% map(~html_text(.x)) %>% as.numeric()
checkpa <- html_2006 %>% map(~html_nodes(.x, 'tr:nth-child(17) td:nth-child(1)')) %>% map(~html_text(.x))%>% map(~str_trim(.x)) %>% as.character()

leg_cases_2006 <- data_frame(week_end_date = as.character(week_end_date),
                             midatlantic = madat,
                             nyc = nycdat,
                             penn = padat,
                             checkma = checkma,
                             checknyc = checknyc,
                             checkpa = checkpa) %>%
  mutate(week_end_date = mdy(week_end_date))

leg_cases <- bind_rows(leg_cases_2006, leg_cases_2007, leg_cases_2008,
leg_cases_2009, leg_cases_2010, leg_cases_2011, leg_cases_2012, leg_cases_2013,
leg_cases_2014, leg_cases_2015, leg_cases_2016, leg_cases_2017)

save(leg_cases, file="data/leg_cases.rda")
