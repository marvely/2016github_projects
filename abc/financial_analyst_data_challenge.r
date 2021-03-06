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
first_event_end_dt <- sqldf('select a."User.ID",
                                    a.first_end_date,
                                    b.Revenue
                             from (select "User.ID",
                                  min("Event.End.Date") as first_end_date
                                  from financ_data_e_dt
                                  group by "User.ID"
                                  ) as a
                            join financ_data_e_dt as b
                            on a."User.ID"= b."User.ID"
                               and a.first_end_date = b."Event.End.Date"
                            ')
# signup date per customer
# should we check the duplicate in signup table?
aggr_dup_customer_dt <- financ_data_su_dt[, .N, by = User.ID]
# no dup
rm(aggr_dup_customer_dt)

# join two tables
financ_data_su_dt$Signup.Date <- as.character(financ_data_su_dt$Signup.Date)
join_cus_date <- sqldf('select s."User.ID" as user_id,
                               s."Signup.Date" as signup_date,
                               e.first_end_date,
                               e.Revenue
                      from financ_data_su_dt as s
                      left join first_event_end_dt as e
                      on s."User.ID" = e."User.ID"
                      ')
# 4104 customers have at least 1 event end date,
# however total 30150 unique user id we have
join_cus_date_update <- sqldf('select *
                              from join_cus_date
                              where first_end_date is not NULL')

# there are 4104 customers who has first event end date?
# only 3509 customers included in signup date?
join_cus_date_update$time_passed <- difftime(strptime(join_cus_date_update$first_end_date, format = "%Y-%m-%d"), strptime(join_cus_date_update$signup_date, format = "%Y-%m-%d"), units = "days" )
# avg across customers
avg_period <- mean(join_cus_date_update$time_passed)

# weighted by revenue
weighted_avg_period <- weighted.mean(x = join_cus_date_update$time_passed, w = join_cus_date_update$Revenue)

# looks like revenue weights heavier on longer period customers.
# translate to business language, customers who stay with us longer produce more revenue than newer customers.
# wait... turns out not the case in the next part of analysis, looks like there's a outlier or something impacted the mean~~~~

# ++++++++++++++++++++++++++ revenue projection/prediction ++++++++++++++++++++++++ #

# variables: time_passed, month/year started, month/year first end, ...
summary(join_cus_date_update$Revenue)
# revenue is skewed to the left, with a long tail to the right.
plot(join_cus_date_update$Revenue)
hist(join_cus_date_update$Revenue, main = "Revenue distribution")
#
# more or less looks like a normal distribution
hist(log(join_cus_date_update$Revenue+1.3), main = "Log Transformed Revenue distribution")
join_cus_date_update$time_passed <- as.numeric(join_cus_date_update$time_passed)

# check correlation first
cor(join_cus_date_update$time_passed, join_cus_date_update$Revenue)
cor(join_cus_date_update$time_passed, log(join_cus_date_update$Revenue+1.3))
# really really low!!! after transformation, even lower... k...

# what else should we use? number of users? number of unique users
# group by month then avg over Revenue and count the total # of users
aggr_join_cus_date_update <- sqldf('select substr(first_end_date, 1, 7) as first_end_month,
                                           user_id,
                                           signup_date,
                                           first_end_date,
                                           Revenue
                                    from join_cus_date_update')
aggr_join_cus_date_update <- sqldf('select first_end_month,
                                           count(user_id) as count_users,
                                           sum(Revenue) as monthly_revenue
                                  from aggr_join_cus_date_update
                                  group by first_end_month
                                  ')
cor(aggr_join_cus_date_update$count_users, aggr_join_cus_date_update$monthly_revenue)
# although users and revenue is highly correlated, would it be available to us?
# we may also need to predict how many users will have the event 
# i feel like this is easier if done in tableau???
