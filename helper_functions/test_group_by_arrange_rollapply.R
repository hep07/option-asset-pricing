##########################################
# generate test data
##########################################


library(dplyr)
library(zoo)
set.seed(1989)

df = expand.grid(site = factor(seq(10)),
                 year = 2000:2004,
                 day = 1:50)
# use Poisson to make math easy to check moving means of temperature
df$temp = rpois(dim(df)[1], 5) 
# Assume rains 33% of the days and averages 5 mm each time but highly variable
df$precip = rbinom(dim(df)[1], 1, 1/3) * rlnorm(dim(df)[1], log(5), 1)
df
df <- df[sample(1:nrow(df),nrow(df),F), ]

# say if we want for each site, each year, the temp on the last day
temp1 <- df %>% group_by(site,year) %>% arrange(day) %>% summarize(last_day_temp = last(temp))
temp2 <- df %>% group_by(site,year) %>% arrange(site, year, day) %>% summarize(last_day_temp = last(temp))
df %>% group_by(site,year) %>% summarize(last_day_temp = last(temp))

# lead or lag within group
temp1 <- df %>% group_by(site, year)  %>% mutate(lag_temp = lead(temp,1)) %>% arrange(site,year,day)

temp2 <- df %>% group_by(site, year) %>% arrange(site,year,day) %>% mutate(lag_temp = lead(temp,1)) 

max(abs(temp1$lag_temp- temp2$lag_temp),na.rm = T)


df = data.frame(name=rep(c(1,3),3),
                score=c(100, 80, 60,140, 120, 110))
df

df %>%
  group_by(name) %>% summarise(last_score = last(score))
  

df %>%
  arrange(name) %>%
  group_by(name) %>% 
  mutate(next.score = lead(score),
         before.score = lag(score) )

df %>%
  group_by(name) %>%
  mutate(next.score = lead(score,1),
         before.score = lag(score,1) )

# moving mean for that day and previous days (e.g. 5 represents the mean of that day and the for previous days)
df2 = df %>%
  group_by(site, year) %>%
  arrange(site, year, day) %>%
  mutate(temp.5 = rollmean(x = temp, 5, align = "right", fill = NA))
head(df2, 75)

# moving mean for the previous days not including the current day (e.g. 5 represents the mean of the 5 previous days)
df2 = df2 %>%
  mutate(temp.lag1 = lag(temp, n = 1)) %>%
  mutate(temp.5.previous = rollapply(data = temp.lag1, 
                                     width = 5, 
                                     FUN = mean, 
                                     align = "right", 
                                     fill = NA, 
                                     na.rm = T))
head(df2, 75)


df2 = df2 %>%
  mutate(precip.30 = rollsum(x = precip, 30, align = "right", fill = NA))
head(df2, 75)


df2 = df %>%
  group_by(site, year) %>%
  arrange(site, year, day) %>%
  mutate(temp.5 = rollsum(x = temp, 5, align = "right", fill = NA),
         temp.5.previous = rollapply(data = temp.lag1, 
                                     width = 2, 
                                     FUN = mean, 
                                     align = "right", 
                                     fill = NA, 
                                     na.rm = T),
         precip.30 = rollsum(x = precip, 30, align = "right", fill = NA))
head(df2, 75)

# last is last non NA value or just last
df = data.frame(name=rep(c(1,3),3),
                score=c(100, 80, 60,140, 120, NA))
df %>% group_by(name) %>% summarise(last = last(score))


# fill date gaps in time series
Fill_TS_NAs <- function(main) {
  # takes datatable with Date and PERMNO as columns and fills in NAs where there are gaps
  
  core <- main[,.(Date, PERMNO)]
  # find first and last dates of each PERMNO
  date.bookends <- core %>%
    group_by(PERMNO) %>%
    summarize(first = first(Date), last = last(Date))
  
  # generate all dates for all PERMNOs then trim those outside of each PERMNO's first and last dates
  output <- core %>%
    cbind(., V2 = 1) %>% # create 3rd column so spread can be applied, 
    spread(., PERMNO, V2) %>% # this requires everything other than PERMNO as the row index and PERMNO as the column index, and row * col index does not have duplicate, i.e. unique index
    gather(., PERMNO, V2, -Date) %>% # key column name, value column name, columns to gather
    merge(date.bookends, by="PERMNO", all.x=TRUE) %>%
    #group_by(PERMNO) %>% # Pu: this groupby does not seem to be needed
    filter(Date>=first & Date<=last) %>%
    select(Date, PERMNO) %>%
    merge(., main, by=c("Date", "PERMNO"), all.x=TRUE)
  
  return(output)
}

df <- data.frame(Date = c(rep(199601,5),rep(199602,5),rep(199603,5)), PERMNO = c(1:6,3:8,2:4), value = 1:15)
temp2 <- df %>% data.table %>% Fill_TS_NAs

df = expand.grid(site = factor(seq(10)),
                 year = 2000:2004,
                 day = 1:50)
# use Poisson to make math easy to check moving means of temperature
df$temp = rpois(dim(df)[1], 5) 
# Assume rains 33% of the days and averages 5 mm each time but highly variable
df$precip = rbinom(dim(df)[1], 1, 1/3) * rlnorm(dim(df)[1], log(5), 1)

df <- df[sample(1:nrow(df),nrow(df),F), ]

df2 = df %>%
  group_by(site, year) %>%
  arrange(site, year, day) %>%
  mutate(temp.5 = rollsum(x = temp, 5, align = "right", fill = NA),
         temp.5.previous = rollapply(data = temp, 
                                     width = 5, 
                                     FUN = mean, 
                                     align = "right", 
                                     fill = NA, 
                                     na.rm = T),
         precip.30 = rollsum(x = precip, 30, align = "right", fill = NA))
head(df2)
