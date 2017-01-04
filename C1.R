library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
#library(circular)

my_data <- read.table("new_lending_club.txt", sep ="|", header = TRUE, stringsAsFactors = FALSE)

allfraud<-read.table("fraud_all_without_constant_columns.txt", sep ="|", header = TRUE, stringsAsFactors = FALSE)

new<-read.table("new_train.txt", sep ="|", header = TRUE, stringsAsFactors = FALSE)



# *************************************************
#              data processing
# *************************************************

# rename the features
feature_names <- c(
        "auth_id",
        "acct_id",
        "fraud_ind",
        "acct_actvn_dt",
        "acct_avl_cash_before_amt",
        "acct_avl_money_before_amt",
        "acct_credit_limit",
        "acct_curr_balance",
        "acct_multicard_ind",
        "acct_open_dt",
        "acct_prod_cd",
        "acct_type_cd",
        "address_vfcn_frmt_cd",
        "address_vfcn_respns_cd",
        "apprd_authzn_cnt",
        "apprd_cash_authzn_cnt",
        "arqc_rslt_cd", # constant
        "authzn_acct_stat_cd",
        "authzn_amount",
        "authzn_type_cd",
        "authzn_char_cd",
        "authzn_opset_id",
        "authzn_orig_src_id",
        "authzn_outstd_amt",
        "authzn_outstd_cash_amt",
        "authzn_rqst_proc_cd",
        "authzn_rqst_proc_dt",
        "authzn_rqst_proc_tm",
        "authzn_rqst_type_cd",
        "authzn_trmnl_pin_capblt_num",
        "avg_dly_authzn_amt",
        "card_vfcn_2_respns_cd",
        "card_vfcn_2_vldtn_dur",
        "card_vfcn_msmt_reas_cd",
        "card_vfcn_presnc_cd",
        "card_vfcn_respns_cd",
        "card_vfcn2_vldtn_cd",
        "cardholder_present_cd",
        "currency_rate",
        "elctr_cmrc_ind_cd",
        "home_phn_num_chng_dur",
        "hotel_stay_car_rentl_dur",
        "last_adr_chng_dur",
        "last_plstc_rqst_reas_cd",
        "mrch_type_cd",
        "mrch_country_cd",
        "new_user_added_dur",
        "phn_chng_snc_appn_ind",
        "pin_blk_cd", # constant
        "pin_vldtn_ind",
        "plstc_actvn_dt",
        "plstc_actvn_reqd_ind",
        "plstc_frst_use_ts",
        "plstc_isu_dur",
        "plstc_prev_curr_cd",
        "plstc_rqst_ts",
        "pos_cond_cd",
        "pos_entry_mthd_cd",
        "rcurg_authzn_ind",
        "rvrsl_ind",
        "sendr_rsidnl_cntry_cd", # constant
        "src_crcy_cd",
        "src_crcy_dcml_psn_num", # constant
        "trmnl_attndnc_cd",
        "trmnl_capblt_cd",
        "trmnl_clasfn_cd",
        "trmnl_id",
        "trmnl_pin_capblt_cd",
        "distance_from_home"
)

colnames(my_data) <- feature_names


# remove the constant features 
my_data <- my_data %>%
        select(-arqc_rslt_cd, -pin_blk_cd, -sendr_rsidnl_cntry_cd, -src_crcy_dcml_psn_num)

# remove massive feature
my_data <- my_data %>%
        select(-trmnl_id)

# remove the very unbalanced feature 
my_data <- my_data %>%
        select(-acct_type_cd, -authzn_opset_id, -authzn_rqst_proc_cd, 
               -card_vfcn_msmt_reas_cd, -phn_chng_snc_appn_ind, -src_crcy_cd,
                )

# convert the categorical feature to factor feature
catg_feature <- c(
        "fraud_ind",                   
        "acct_multicard_ind",          
        "address_vfcn_frmt_cd",        
        "address_vfcn_respns_cd",     
        "authzn_acct_stat_cd",         
        "authzn_type_cd",              
        "authzn_char_cd",             
        "authzn_orig_src_id",          
        "authzn_rqst_type_cd",         
        "authzn_trmnl_pin_capblt_num", 
        "card_vfcn_2_respns_cd",    
        "card_vfcn_presnc_cd",         
        "card_vfcn_respns_cd",         
        "card_vfcn2_vldtn_cd",         
        "cardholder_present_cd",      
        "elctr_cmrc_ind_cd",           
        "last_plstc_rqst_reas_cd",     
        "mrch_country_cd",             
        "pin_vldtn_ind",               
        "plstc_actvn_reqd_ind",        
        "plstc_prev_curr_cd",          
        "pos_cond_cd",                 
        "pos_entry_mthd_cd",          
        "rcurg_authzn_ind",            
        "rvrsl_ind",                   
        "trmnl_attndnc_cd",            
        "trmnl_capblt_cd",             
        "trmnl_clasfn_cd",             
        "trmnl_pin_capblt_cd",         
        "acct_prod_cd",               
        "mrch_type_cd"
)

# convert to factor feature
my_data[catg_feature] <- lapply(my_data[catg_feature], as.factor)

# combine the date and time for transaction
authzn_date_time <- paste(my_data$authzn_rqst_proc_dt, my_data$authzn_rqst_proc_tm, sep = " ")
my_data$authzn_date_time <- as.POSIXct(authzn_date_time, format = "%Y-%m-%d %H:%M:%S", tz = "EST5EDT")

# set a standard time point
standard_point <- as.POSIXct("1994-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "EST5EDT")

# convert the time to seconds
my_data$auth_time_secs <- difftime(my_data$authzn_date_time, standard_point, units = "secs")



# *************************************************
#             feature engineering 
# *************************************************

# group the merchant type code
group_merchant_type <- function(temp_data){
        temp_data$mrch_type_cd <- as.numeric(as.character(temp_data$mrch_type_cd))
        temp_data$mrch_type[temp_data$mrch_type_cd<2000] <- "con_service"
        temp_data$mrch_type[temp_data$mrch_type_cd>=3000 & temp_data$mrch_type_cd<=3350] <- "airline"
        temp_data$mrch_type[temp_data$mrch_type_cd>=3351 & temp_data$mrch_type_cd<=3500] <- "car_rental"
        temp_data$mrch_type[temp_data$mrch_type_cd>=3501 & temp_data$mrch_type_cd<=4000] <- "hotel"
        temp_data$mrch_type[temp_data$mrch_type_cd>=4001 & temp_data$mrch_type_cd<4812] <- "transportation"
        temp_data$mrch_type[temp_data$mrch_type_cd>=4812 & temp_data$mrch_type_cd<5000] <- "utilities"
        temp_data$mrch_type[temp_data$mrch_type_cd>=2000 & temp_data$mrch_type_cd<3000] <- "wholesale"
        temp_data$mrch_type[temp_data$mrch_type_cd>=5000 & temp_data$mrch_type_cd<=5199] <- "wholesale"
        temp_data$mrch_type[temp_data$mrch_type_cd>=5200 & temp_data$mrch_type_cd<5500] <- "retail"
        temp_data$mrch_type[temp_data$mrch_type_cd>=5500 & temp_data$mrch_type_cd<5600] <- "automobiles"
        temp_data$mrch_type[temp_data$mrch_type_cd>=5600 & temp_data$mrch_type_cd<5700] <- "clothing"
        temp_data$mrch_type[temp_data$mrch_type_cd>=5700 & temp_data$mrch_type_cd<6000] <- "miscellaneous"
        temp_data$mrch_type[temp_data$mrch_type_cd>=6000 & temp_data$mrch_type_cd<7210] <- "service_provider"
        temp_data$mrch_type[temp_data$mrch_type_cd>=7210 & temp_data$mrch_type_cd<7311] <- "personal_service"
        temp_data$mrch_type[temp_data$mrch_type_cd>=7311 & temp_data$mrch_type_cd<7531] <- "business"
        temp_data$mrch_type[temp_data$mrch_type_cd>=7531 & temp_data$mrch_type_cd<7829] <- "repair"
        temp_data$mrch_type[temp_data$mrch_type_cd>=7829 & temp_data$mrch_type_cd<8000] <- "amusement"
        temp_data$mrch_type[temp_data$mrch_type_cd>=8000 & temp_data$mrch_type_cd<9000] <- "professional_service"
        temp_data$mrch_type[temp_data$mrch_type_cd>=9000] <- "government"
        temp_data$mrch_type[temp_data$mrch_type_cd == 9401] <- "other"
        temp_data$mrch_type[temp_data$mrch_type_cd %in% c(7375,7379,7829,8734)] <- "wholesale"
        temp_data$mrch_type[temp_data$mrch_type_cd %in% c(4829,6050,6051,6529,6530,6534)] <- "quasi_cash"
        temp_data$mrch_type[temp_data$mrch_type_cd == 7011] <- "hotel"
        temp_data$mrch_type[temp_data$mrch_type_cd == 4511] <- "airline"
        temp_data$mrch_type[temp_data$mrch_type_cd == 7512] <- "car_rental"
        return(temp_data)
}

my_data <- group_merchant_type(my_data)
my_data$mrch_type <- as.factor(my_data$mrch_type)
my_data$mrch_type_cd <- NULL


# group merchant country code 
transform_country <- function(x){ 
        if( x %in% c(84,188,222,320,340,484,558,591,60,124,304,666,32,68,76,152,170,218,238,254,328,600,604,740,858,862,
                     660,28,533,44,52,535,92,136,192,531,212,214,308,312,332,388,474,500,630,652,659,662,663,670,534,780,796,850))
        {x="America"}
        else if( x == 840)
        {x="USA"}
        else if( x %in% c(398,417,762,795,860,156,344,446,392,408,410,496,158,4,50,64,356,364,
                          462,524,586,144,4,50,64,356,364,462,524,586,144,96,116,360,418,458,104,608,702,764,626,704,
                          51,31,48,196,268,368,376,400,414,422,275,512,634,682,760,792,784,887))
        {x="Asia"}
        else if( x %in% c(112,100,203,348,498,616,642,643,703,804,
                          248,830,208,233,234,246,831,352,372,832,428,
                          440,833,578,744,752,826,8,20,70,191,292,300,
                          336,380,807,470,499,620,674,688,705,724,40,
                          56,250,276,280,438,442,492,528,756))
        {x="Europe"}
        else 
        {x = "Unclassified"}
        return(x)
}

country_code <- my_data$mrch_country_cd
country_code <- as.numeric(as.character(country_code))
country_code <- lapply(country_code, transform_country)
country_code <- as.character(country_code)
my_data$mrch_country_cd <- as.factor(country_code)


#############LATER

#1. continous var

###acct_curr_balance
plot(density(temp$acct_curr_balance),col='red',ylim=c(0,0.01))
barplot(my_data$acct_curr_balance)
temp<-subset(my_data,acct_curr_balance!=0 & acct_curr_balance!=39)
ggplot(temp, aes(acct_curr_balance))+ geom_histogram(binwidth=.5, colour="black", fill="white")


#t.test
### auth_amount
with(my_data,t.test(log(authzn_amount+1)~fraud_ind,alternative='less'))

#plot
plot(density(log(subset(my_data,fraud_ind=='Y')$authzn_amount)),col='red',ylim=c(0,0.4))
lines(density(log(subset(my_data,fraud_ind=='N')$authzn_amount)))

boxplot(subset(my_data,fraud_ind=='Y')$authzn_amount)
summary(my_data$authzn_amount)
summary(subset(my_data,fraud_ind=='N')$authzn_amount)

summary(allfraud$authzn_amount)
sum(allfraud$authzn_amount)
plot(density(log(allfraud$authzn_amount)),col='red',ylim=c(0,0.4))
lines(density(log(subset(my_data,fraud_ind=='N')$authzn_amount)))
t.test(log(allfraud$authzn_amount+1),log(subset(my_data,fraud_ind=='N')$authzn_amount+1),alternative='two.sided')


##account_open_time

#### summary
summary(my_data$acct_open_year*(-1)+2014)
summary(subset(my_data,fraud_ind=='Y')$acct_open_year*(-1)+2014)
summary(subset(my_data,fraud_ind=='N')$acct_open_year*(-1)+2014)

####log trans density plot
plot(density(log(subset(my_data,fraud_ind=='Y')$acct_open_year*(-1)+2014)),col='red',ylim=c(0,1.2))
lines(density(log(subset(my_data,fraud_ind=='N')$acct_open_year*(-1)+2014)))
#### non trans density plot
plot(density(subset(my_data,fraud_ind=='Y')$acct_open_year*(-1)+2014),col='red',ylim=c(0,0.22))
lines(density(subset(my_data,fraud_ind=='N')$acct_open_year*(-1)+2014))

####barplot
ggplot(my_data, aes(acct_open_year, fill = fraud_ind))+ geom_histogram(position = "dodge")

#t.test
t.test(subset(my_data,fraud_ind=='Y')$acct_open_year*(-1)+2014,subset(my_data,fraud_ind=='N')$acct_open_year*(-1)+2014,alternative='less')
#with(my_data,t.test(acct_open_year*(-1)+2016~fraud_ind,alternative='greater'))
#t.test(rnorm(2120, 6, 2),rnorm(95148, 6, 2),alternative='less')


#2. categorical var
#chi-square test
### merchant type
f<-table(subset(my_data,fraud_ind=='Y')$mrch_type)
non<-table(subset(my_data,fraud_ind=='N')$mrch_type)

merch_type<-data.frame(mt=unique(my_data$mrch_type),stringsAsFactors = FALSE)
merch_type$fraud<-f[merch_type$mt]
merch_type$non_fraud<-non[merch_type$mt]
with(merch_type,chisq.test(fraud,non_fraud))

tbl1 = table(my_data$mrch_type, my_data$fraud_ind) 
chisq.test(tbl1)

### morning,eveing
#x <- matrix(c(1366, 754, 73998, 21150), ncol = 2)
tbl = table(my_data$authzn_time, my_data$fraud_ind) 
chisq.test(tbl)


#plot 
barplot(merch_type$fraud,las=2,cex.names=0.6,legend.text='fraud',ylim=c(0,800))
barplot(merch_type$non_fraud,las=2,cex.names=0.5,legend.text='non_fraud')

merch_type$fraud_per<-prop.table(table(subset(my_data,fraud_ind=='Y')$mrch_type))[merch_type$mt]
merch_type$non_fraud_per<-prop.table(table(subset(my_data,fraud_ind=='N')$mrch_type))[merch_type$mt]

counts <- rbind(merch_type$fraud_per, merch_type$non_fraud_per)
barplot(counts, main="Percetage of Merchant Type by Fraud and Non-Fraud",
        col=c("darkblue","red"),
        legend = c('fraud','non_fraud'), beside=TRUE,las=2,cex.names=0.5)









#############



#-------- group feature authzn_acct_stat_cd ------------------------
my_data$authzn_acct_stat_cd <- as.character(my_data$authzn_acct_stat_cd)
my_data$authzn_acct_stat_cd[my_data$authzn_acct_stat_cd == "F"] <- "D"
my_data$authzn_acct_stat_cd[my_data$authzn_acct_stat_cd == "R"] <- "D"
my_data$authzn_acct_stat_cd  <- as.factor(my_data$authzn_acct_stat_cd)


#----------------------------group currency rate -----------------------------
my_data$currency_rate <- as.numeric(as.character(my_data$currency_rate))
my_data$currency_rate[my_data$currency_rate != 1000000 & my_data$currency_rate != 0] <- 2
my_data$currency_rate[my_data$currency_rate == 1000000] <- 1
my_data$currency_rate <- as.factor(my_data$currency_rate)


#----------------------------- transaction date -----------------------------
my_data <- my_data%>%
        separate(authzn_rqst_proc_tm, into = c("authzn_hour", "authzn_min", "authzn_sec"), sep = ":", remove = FALSE)
my_data$authzn_hour <- as.numeric(my_data$authzn_hour)
my_data$authzn_min <- as.numeric(my_data$authzn_min)
my_data$authzn_sec <- as.numeric(my_data$authzn_sec)


# visualization
ggplot(my_data, aes(x=authzn_hour, fill=fraud_ind)) +
        geom_histogram(binwidth=.5, alpha=.5, position="identity")

prop.table(table(my_data$authzn_hour, my_data$fraud_ind),1)


#-----------------------------extract account open date -----------------------------
my_data <- my_data%>%
        separate(acct_open_dt, into = c("acct_open_year", "acct_open_month", "acct_open_day"), 
                 sep = "-", remove = FALSE)

my_data <- my_data%>%
        select(-acct_open_dt, -acct_open_month, -acct_open_day)

my_data$acct_open_year <- as.numeric(my_data$acct_open_year)



#---------------------group transaction time into morning and evening-----------------------------
my_data$authzn_time <- "evening"
my_data$authzn_time[my_data$authzn_hour >= 8&my_data$authzn_hour <= 20] <- "morning"
my_data$authzn_time <- as.factor(my_data$authzn_time)

# visualization
ggplot(my_data, aes(x=authzn_time, fill=fraud_ind)) +
        geom_histogram(binwidth=.5, alpha=.5, position="identity")
prop.table(table(my_data$authzn_time, my_data$fraud_ind),1)


#--------------------------construct time feature -----------------------------
my_data <- my_data%>%
        mutate(theta = authzn_hour + authzn_min/60, theta = (2*pi*theta)/24) 

# function that computes the period mean of time 
period_mean <- function(myhour){
        n <- length(myhour)
        Cp <- sum(cos(myhour))
        Cp_bar <- Cp/n
        Sp <- sum(sin(myhour))
        Sp_bar <- Sp/n
        Rp <- sqrt(Cp*Cp + Sp*Sp)
        Rp_bar <- Rp/n
        if(Cp_bar > 0 & Sp_bar >0){
                Tp <- atan(Sp/Cp) 
        }else if(Cp_bar < 0){
                Tp <- atan(Sp/Cp) + pi
        }else{
                Tp <- atan(Sp/Cp) + 2 * pi
        }
        Tp <- 24*Tp/(2*pi)
        return(Tp)
}

# construct time features
time_feature_constructor <-function(CI, Day, ID_filter, i) {
        filter_transaction = (ID_filter$auth_time_secs[(i+1):auth_length] > ID_filter$auth_time_secs[i] - 3600*24*Day) * 1
        filter_theta = ID_filter$theta[(i+1):auth_length] * filter_transaction
        filter_theta = subset(filter_theta, filter_theta > 0)
        if(length(filter_theta) <= 2){
                ID_filter[[paste("time_feature_",Day,"Days_", CI, "CI", sep="")]][i] <- "N"
        }else{
                time_mean <- period_mean(filter_theta)
                current_time <-  24*ID_filter$theta[i]/(2*pi)
                if(current_time  > time_mean-CI & current_time  < time_mean+CI ){
                        ID_filter[[paste("time_feature_",Day,"Days_", CI, "CI", sep="")]][i] <- "TRUE"
                }else{
                        ID_filter[[paste("time_feature_",Day,"Days_", CI, "CI", sep="")]][i] <- "FALSE"  
                }
        }
        rm(filter_transaction, filter_theta)
        return(ID_filter)
}


for (Day in c(7, 28, 56)){
        for(CI in c(4, 5, 6)){
                my_data[[paste("time_feature_",Day,"Days_", CI, "CI", sep="")]] <- "N"
                my_data[[paste("time_feature_",Day,"Days_", CI, "CI", sep="")]] <- "N"   
        }
}

IDs <- unique(my_data$acct_id)
new_data <- data.frame()

for (k in 1:length(IDs)){
        CI <- 6
        current_ID = IDs[k]
        ID_filter = my_data%>%
                filter(acct_id == current_ID) %>%
                arrange(desc(auth_time_secs))
        auth_length = nrow(ID_filter)
        
        for (i in 1:(auth_length-1)){
                for(Day in c(7, 28, 56)){
                        for(CI in c(4, 5, 6)){
                                ID_filter <- time_feature_constructor(CI, Day, ID_filter, i)
                        }
                }
        }
        new_data = rbind(new_data, ID_filter)
}


time_feature_name <- c("time_feature_7Days_4CI","time_feature_7Days_5CI",
                       "time_feature_7Days_6CI","time_feature_28Days_4CI",
                       "time_feature_28Days_5CI","time_feature_28Days_6CI",
                       "time_feature_56Days_4CI","time_feature_56Days_5CI",
                       "time_feature_56Days_6CI" )
my_data <- new_data
my_data[time_feature_name] <- lapply(my_data[time_feature_name], as.factor)
my_data <- my_data%>%
        select(-theta)
rm(new_data)


#--------------------------construct aggregation feature -----------------------------
new_data <- data.frame()

# function that construct aggregation features
process_transaction <-function(Hour, temp_data, i) {
        filter_transaction = (temp_data$auth_time_secs[(i+1):auth_length] > temp_data$auth_time_secs[i] - 3600*Hour) * 1
        filter_transaction_amt = temp_data$authzn_amount[(i+1):auth_length] * filter_transaction
        temp_data[[paste("authzn_count_",Hour,"h", sep="")]][i] <- sum(filter_transaction, na.rm = TRUE)
        temp_data[[paste("authzn_amt_",Hour,"h", sep="")]][i] <- sum(filter_transaction_amt, na.rm = TRUE)
        rm(filter_transaction, filter_transaction_amt)
        return(temp_data)
}

for (Hour in c(1,3,6,12,18,24,72,168)){
        my_data[paste("authzn_count_",Hour,"h", sep="")] <- 0
        my_data[paste("authzn_amt_",Hour,"h", sep="")] <- 0
}

for (k in 1:length(IDs)){
        current_ID = IDs[k]
        ID_filter = my_data%>%
                filter(acct_id == current_ID) %>%
                arrange(desc(auth_time_secs))
        auth_length = nrow(ID_filter)
        
        for (i in 1:(auth_length-1)){
                for (hour in c(1,3,6,12,18,24,72,168)){
                        ID_filter <- process_transaction(hour, ID_filter, i)
                }
        }
        new_data = rbind(new_data, ID_filter)
}
my_data <- new_data
rm(new_data)

write.table(my_data, "my_data.txt", col.names = FALSE, row.names = TRUE, quote = FALSE, sep = "|")


# *************************************************
#             Train Desicion Tree Model
# *************************************************
library(rpart)
fit <- rpart(fraud_ind ~ acct_open_year+authzn_amount+mrch_type+distance_from_home, data = my_data,method="class")
summary(fit)
printcp(fit)
plotcp(fit)
plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
rpart.plot(fit)

require(tree)
tree.fraud = tree()
summary(tree.fraud)
plot(tree.fraud)
text(tree.fraud, pretty = 0)

# *************************************************
#             Train Logistic Model
# *************************************************
my_data <- my_data%>%
        select(-acct_actvn_dt, -authzn_hour, 
               -authzn_min, -authzn_sec, -authzn_sec, -theta,
               -authzn_rqst_proc_dt, -authzn_rqst_proc_tm,
               -plstc_actvn_dt, -plstc_frst_use_ts, -plstc_rqst_ts, 
               -auth_time_secs, -authzn_date_time)


my_train <- my_data %>%
        filter(auth_id %in% my_train_auth_id)

my_validation <- my_data %>%
        filter(auth_id %in% my_validation_auth_id)


my_train$fraud_ind <- as.character(my_train$fraud_ind)
my_train$fraud_ind[my_train$fraud_ind == "Y"] <- 1
my_train$fraud_ind[my_train$fraud_ind == "N"] <- 0
my_train$fraud_ind<-as.factor(my_train$fraud_ind)

#-----------------------------logistic model -----------------------------
base_model <- glm(fraud_ind~.-auth_id, data = my_train, family = "binomial")

# estimate the probability of fraud for each transaction
pred_y_prob <- predict(base_model, newdata = my_validation, type = "response") 


# classification function given a threshold
classification_given_threshold <- function(list_prob, threshold) {
        result <- integer(length(list_prob))
        result[list_prob >= threshold] <- "Y"
        result[list_prob < threshold] <- "N"
        result
}

# classify fraudulent transaction or nonfraudulent transaction
# our probability threshold is taken as 0.02
pred_y <- classification_given_threshold(pred_y_prob, 0.02)


result <- data.frame(AUTH_ID=my_validation$auth_id, P_FRAUD=pred_y_prob, DECLINE=pred_y)

write.table(Whole_result, "C1_results.txt", col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "|")


# *************************************************
#             Cost Analysis
# The cost analysis is used to select the probability 
# threshold by cross validation
# *************************************************

true_data <- my_test%>%
        select(auth_id, fraud_ind, authzn_amount, acct_credit_limit)
true_y <- as.character(true_data$fraud_ind)
true_data$fraud_ind <- as.numeric(true_y)

# function that computes the total cost 
compute_cost <- function(pred_y){
        compare_y <- data.frame(true_data, pred_y)
        false_positive <- compare_y %>%
                filter(true_y==0, pred_y ==1) %>%
                select(authzn_amount, acct_credit_limit) 
        
        low_limit_user <- (false_positive$acct_credit_limit <= 2000) * 1
        high_limit_user <- (false_positive$acct_credit_limit > 2000) * 1
        
        false_positive_cost <- 0.0075 * sum(low_limit_user * false_positive$authzn_amount) +
                0.002 * 75 * sum(low_limit_user) +
                0.005 * 4 * sum(high_limit_user * false_positive$authzn_amount) +
                0.005 * 275 * sum(high_limit_user) 
        
        false_negative <- compare_y %>%
                filter(true_y==1, pred_y ==0) %>%
                select(authzn_amount)
        false_negative_cost <- sum(false_negative$authzn_amount)
        total_cost <- false_negative_cost + false_positive_cost
}




