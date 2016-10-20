library(xlsx)
financ_data_su <- read.xlsx("~/Documents/Kaggle Project/2016github_project/abc/FinanceAnalyst_HomeworkData.xlsx", sheetName = "Signups")

financ_data_events <- read.xlsx("~/Documents/Kaggle Project/2016github_project/abc/FinanceAnalyst_HomeworkData.xlsx", sheetName = "Events")
str(financ_data_events)
financ_data_su$User.ID <- as.character(financ_data_su$User.ID)
financ_data_events$User.ID <- as.character(financ_data_events$User.ID)

save(financ_data_su, financ_data_events, file = "finance_r_data.RDA")
load(file = "finance_r_data.RDA")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++ #
str(financ_data_su)

financ_data_su$NA. <- NULL

financ_data_su$month <- format(financ_data_su$Signup.Date, '%Y-%m')
financ_data_su$year <- format(financ_data_su$Signup.Date, '%Y')

library(data.table)
financ_data_su_dt <- data.table(financ_data_su)

# aggr by month
aggr_su_dt <- financ_data_su_dt[, .N, by = month]
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

# +++++++++++++ doing the same thing over again for event_end ++++++++++++++ #
# need to write a function to do it every time...
financ_data_events$month <- format(financ_data_events$Event.End.Date, '%Y-%m')
financ_data_events$year <- format(financ_data_events$Event.End.Date, '%Y')
financ_data_e_dt <- data.table(financ_data_events)

# aggr by month
aggr_e_dt <- financ_data_e_dt[, .N, by = month]
# how to sort by col in data table?
aggr_e_dt = aggr_e_dt[order(month, descending = F), ]
e_range <- range(0, aggr_e_dt$N)
plot(aggr_e_dt$N, type = "o", axes = F, ann = F)
axis(1, at = 1: nrow(aggr_e_dt), lab = aggr_e_dt$month)
axis(2, las = 1, at = 200 * 0:e_range[2])
title(main = "Event End Each Month", font.main = 4)
title(xlab = "Month")
title(ylab = "Event End Counts")

# unique users per month
library(sqldf)
uniq_user_id_e_dt <- sqldf('select month, "User.ID"
                           from financ_data_e_dt
                           group by month, "User.ID"
                           ')
uniq_user_id_e_dt <- data.table(uniq_user_id_e_dt)
# then aggregate by month again
aggr_uniq_user_dt <- uniq_user_id_e_dt[, .N, by = month]
aggr_uniq_user_dt <- aggr_uniq_user_dt[order(month, descending = F)]
colnames(aggr_uniq_user_dt) <- c("month", "#_Unique_Customers")
cust_n_range <- range(0, aggr_uniq_user_dt$`#_Unique_Customers`)

plot(aggr_uniq_user_dt$`#_Unique_Customers`, type = "o", axes = F, ann = F )

axis(1, at = 1:nrow(aggr_uniq_user_dt), lab = aggr_uniq_user_dt$month)
axis(2, las = 1, at = 100*0:cust_n_range[2])

title(main = "Unique Customer Counts by Month", font.main = 4)
title(xlab = "Month")
title(ylab = "Unique Customer Counts")

join_aggr_by_month <- cbind(aggr_e_dt, aggr_uniq_user_dt$`#_Unique_Customers`)
colnames(join_aggr_by_month)[3] <- "#_Unique_customers"

join_aggr_by_month$avg_event_per_customer <-join_aggr_by_month$N / join_aggr_by_month$`#_Unique_customers`

# plot the avg by month
avg_range <- range(0, join_aggr_by_month$avg_event_per_customer)
plot(join_aggr_by_month$avg_event_per_customer, type = "o", axes = F, ann = F )

axis(1, at = 1:nrow(join_aggr_by_month), lab = join_aggr_by_month$month)
axis(2, las = 1, at = 0.5*0:avg_range[2])

title(main = "Avg End Events per Customer Per Month", font.main = 4)
title(xlab = "Month")
title(ylab = "Ratio (End Events / Unique Customer)")

# +++++++++++++++++ #
# join by customer id?
# first event ending, min(end date)
financ_data_e_dt$Event.End.Date <- as.character(financ_data_e_dt$Event.End.Date)
first_event_end_dt <- sqldf('select "User.ID",
                                    min("Event.End.Date") as first_end_date
                            from financ_data_e_dt
                            group by "User.ID"')
# signup date per customer
# should we check the duplicate in signup table?


