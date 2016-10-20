setwd("~/Downloads")
library(xlsx)
financ_data_su <- read.xlsx("~/Downloads/FinanceAnalyst_HomeworkData.xlsx", sheetName = "Signups")

financ_data_events <- read.xlsx("~/Downloads/FinanceAnalyst_HomeworkData.xlsx", sheetName = "Events")

save(financ_data_su, financ_data_events, file = "finance_r_data.RDA")
load(file = "finance_r_data.RDA")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++ #
str(financ_data_su)

financ_data_su$NA. <- NULL
financ_data_su$User.ID <- as.character(financ_data_su$User.ID)
try_1 <- financ_data_su[1:10,]
try_1$month <- format(try_1$Signup.Date, '%Y-%m')
financ_data_su$month <- format(financ_data_su$Signup.Date, '%Y-%m')
financ_data_su$year <- format(financ_data_su$Signup.Date, '%Y')

library(data.table)
financ_data_su_dt <- data.table(financ_data_su)

# aggr by month
aggr_su_dt <- financ_data_su_dt[, .N, by = month]
head(aggr_su_dt)
# how to sort by col in data table?
aggr_su_dt = aggr_su_dt[order(month, descending = F), ]
barplot(aggr_su_dt$N, main="question 1", xlab="Month", ylab="Signup Counts", names.arg=aggr_su_dt$month)
plot(aggr_su_dt$N, type = "o", main = "Signup by Month", xlab = "Month", ylab = "Signup Counts", axes = F, ann = F)
axis(1, at = 1:10, lab = aggr_su_dt$month)
c_range = range(0, aggr_su_dt$N)
axis(2, las = 1, at = 1000 * 0:c_range[2])
title(main = "Signup Counts by Month", font.main = 4)
title(xlab = "Month")
title(ylab = "Signup Counts")
# aggr by year
aggr_y_su_dt <- financ_data_su_dt[, .N, by = year]
aggr_y_su_dt <- aggr_y_su_dt[order(year, descending = F), ]
barplot(aggr_y_su_dt$N, main = "Year Trend", xlab = "Year", ylab = "Signup Counts", names.arg = aggr_y_su_dt$year)
plot(aggr_y_su_dt$N, type = "o", axes = F, ann = F)
axis(1, at = 1:4, lab = aggr_y_su_dt$year)
axis(2, las = 1, at = 3000 * 0:c_range[2])
title(main = "Yearly Trend", font.main = 4)
title(xlab = "Year")
title(ylab = "Signup Counts")



str(financ_data_events)
financ_data_events$User.ID <- as.character(financ_data_events$User.ID)