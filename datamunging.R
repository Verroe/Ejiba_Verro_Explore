#10/27/2016
options(StringsAsFactors = FALSE)
install.packages('dplyr')
library(dplyr)
install.packages('lubridate')
library(lubridate)
install.packages('purrr')
library(purrr)
library(ggplot2)
ind <- read.csv('/Users/verroejiba/Downloads/itcont.txt', sep ='|', skipNul = T, na.strings = '', header = F, quote = "")
names(ind)<- names(read.csv('/Users/verroejiba/Downloads/indiv_header_file.csv'))
#select -- decrease the width 
ind %>%
  select(CMTE_ID, TRANSACTION_TP, ENTITY_TP, TRANSACTION_DT, TRANSACTION_AMT) -> ind
#filter decrease height
ind %>%
  filter(TRANSACTION_TP == '15') %>%
  select(-TRANSACTION_TP) -> ind
cand <- read.csv('/Users/verroejiba/Downloads/cn.txt',sep ='|', skipNul = T, na.strings = '', header = F, quote = "")
names(cand) <- names(read.csv('/Users/verroejiba/Downloads/cn_header_file.csv'))
cand %>%
  select(CAND_ID, CAND_NAME, CAND_ELECTION_YR, CAND_OFFICE) -> cand
cand %>% filter(CAND_OFFICE == 'P') %>% select(-CAND_OFFICE) -> cand
link <- read.csv('/Users/verroejiba/Downloads/ccl.txt', sep ='|', skipNul = T, na.strings = '', header = F, quote = "")
names(link)<- names(read.csv('/Users/verroejiba/Downloads/ccl_header_file.csv'))
link %>%
  select(CAND_ID,CAND_ELECTION_YR, CMTE_ID) -> link
cand %>% 
  inner_join(link, by = c("CAND_ID", "CAND_ELECTION_YR")) -> df
df %>%
  inner_join(ind, by ="CMTE_ID") -> df
#there is a code missing
df[which(is.na(df$TRANSACTION_DT)),]
df %>% mutate(date = as.Date(mdy(TRANSACTION_DT))) %>% 
  select(-TRANSACTION_DT) -> df
df %>% rename(year = CAND_EKECTION_YR, amount = TRANSACTION_AMT, type =ENTITY_TP) -> df
df$name <- paste(lapply(strsplit(as.character(df$CAND_NAME), ','), '[', 2), 
                 lapply(strsplit(as.character(df$CAND_NAME), ','), '[', 1))
df %>% select(-CAND_NAME) -> df
df %>% 
  filter(year == 2012) %>%
  select(-year) -> df
#filter only people
df %>%
  filter(type %in% c('IND','CAN')) %>%
  select(-type) ->df
df %>% 
  filter(abs(amount) <100000) -> df