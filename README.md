# ceu
gittest


set.seed(42)
tx <- data.table(
  item   = sample(letters[1:3], 10, replace = TRUE),
  time   = as.POSIXct(as.Date('2016-01-01')) - runif(10) * 36*60^2,
  amount = rpois(10, 25))
prices <- data.table(
  item  = letters[1:3],
  date  = as.Date('2016-01-01') - 1:2,
  price = as.vector(outer(c(100, 200, 300), c(1, 1.2))))
items <- data.table(
  item   = letters[1:3],
  color  = c('red', 'white', 'red'),
  weight = c(2, 4, 2.5))

#assign NULL removes the col.

#filter for transactions with "b" items
dt[Dest == 'LAX', list(Dest, DepTime, ArrTime)]  #first/last 5 rows

tx

tx[item == 'b']

#filter for transactions with less than 25 items
tx[amount < 25]

#filter for transactions with less then 25 "b" items
tx[amount < 25 & item == 'b']

#count the number of transactions for each items
tx[, .N, by = item]
tx[, .(trxdb = .N), by = item] #the same with col name, '.' start a list

#count the number of transactions for each day
tx[, .(trxdb = .N), by = as.Date(tx$time)]
tx$date <- as.Date(tx$time)] # the same
tx[, date := as.Date(tx$time)] # add new column

#count the overall number of items sold on each day
tx[, .(trxdb = .N, trxsum = sum(amount)), by = as.Date(tx$time)] 

?t # matrix transpose

#Further data.table examples on

#left joins
#transforming wide and long tables with reshape2
#rolling and overlap joins

x <- 'amount'
tx[, get(x)]

merge(tx, items, by = 'item') # simple left join
merge(tx, items, by.x = 'item', by.y = 'item') # simple left join direct giving the field name

setkey(tx, item)
setkey(items, item)
str(tx)
items[tx]
tx[items]
tx <- tx[items]

setkey(tx, item, date)  #column names doesnot matter, only the oder!
setkey(prices, item, date)

prices[tx]  # lookup price in tx

merge(tx, prices, by.x = 'item', by.y = 'item') # simple left join

tx[, sum(weight), by = date]
tx[, .(sum(amount), .N), by = .(color, date)]

table(tx$color, tx$date)
?merge

install.packages("reshape2")
library(reshape2) # melt ~ dcast

ft <- tx[, .(sum(amount), .N), by = .(color, date)]
ft

dcast(ft, color ~ date) # pivot table
dcast(ft, date ~ color, value.var = V1) # pivot table
?dcast

ft <- dcast(ft, date ~ color) # pivot table

melt(ft, id.vars = 'date')

tx[c(1,4,7), color := NA]
is.na(tx) # give back NA 
tx[is.na(color), color := 'x'] # give back NA 



---------MODELS-----------------------------------------

df <- read.csv('http://bit.ly/BudapestBI-R-csv')

str(df)

?t.test #2 samlples compare the mean
t.test(heightIn ~ sex, data = df)

table(df$sex)

aov(heightIn ~ sex, data = df) # variance within and between groups, how similar the 2 group 
#aov - useful multiple variable
summary(aov(heightIn ~ sex, data = df))

boxplot(table(sex))
library(ggplot2)
ggplot(df, aes(x = sex, y=heightIn)) + geom_boxplot()
ggplot(df, aes(x = sex, y=heightIn)) + geom_violin()

str(df)
setDT(df) # data.table
setDT(ft) # data.table


df[, high := as.character(heightIn > mean(heightIn))] 

df[, .N, by = .(sex, high)]
dcast(df, sex ~ high) # pivot table
ft <- dcast(df, sex ~ high) # pivot table
ft
?chisq.test()
chisq.test(ft)
str(ft)
ft[,-1] #-1 w/o first column
ft$sex <- NULL
chisq.test(ft)  #p val > 0.05 if higher may be different



