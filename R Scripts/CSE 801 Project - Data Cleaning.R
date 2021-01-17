###################### CSE 801 Project - Loan Analysis ########################

# I. Data and libraries ---------

setwd("C:/Users/saksh/Desktop/Credit Default Project/Data")
dir()

data <- read.csv("accepted_2007_to_2018Q4.csv")

# II. Data cleaning and manipulation ---------

# We are going to work only with the most recent issued loans
# Maybe work with the last 2 years of data.

# See which variables are dates
grep("_d", colnames(data), fixed = T)
colnames(data)[grep("_d", colnames(data), fixed = T)] # not exact. Must end with '_d'

date_cols <- which(substring(colnames(data), nchar(colnames(data))-1 ,nchar(colnames(data))) == "_d")
head(data[,date_cols])

# Copy of the dataset
data2 <- data

# Paste '01-' in dates
test <- head(data2$issue_d)
test <- paste('01-',test, sep = "")
as.Date(test, format = "%d-%b-%Y")

for(i in date_cols){
  data2[,i] <- paste('01-',data2[,i], sep = "")
  data2[,i] <- as.Date(data2[,i], format = "%d-%b-%Y")
}

# subset issued loans from 2017-2018
summary(data2$issue_d)
data2 <- subset(data2, data2$issue_d>= as.Date('2017-01-01'))
data2 <- subset(data2, data2$issue_d<= as.Date('2018-12-31')) # More o less 1 mill observations

# III. Match with Data Dictionary --------

dir()
dict <- readxl::read_excel('LCDataDictionary.xlsx', sheet = 2)

dict <- subset(dict,!is.na(dict$BrowseNotesFile))
dict$BrowseNotesFile <- trimws(dict$BrowseNotesFile)
dict$BrowseNotesFile <- gsub('([[:upper:]])', '_\\1', dict$BrowseNotesFile)
dict$BrowseNotesFile <- tolower(dict$BrowseNotesFile)

table(dict$BrowseNotesFile %in% colnames(data))

# variables that are right and wrong
w <- c('is_inc_v', 'mths_since_most_recent_inq', 'mths_since_oldest_il_open',
         'mths_since_recent_loan_delinq', 'verified_status_joint')
c <- c('verification_status', 'mths_since_recent_inq', 'mo_sin_old_il_acct',
           'mths_since_recent_bc_dlq', 'verification_status_joint')

features <- dict$BrowseNotesFile[which(!dict$BrowseNotesFile %in% w)]
features <- c(features, c)

final_features <- features[which(features %in% colnames(data2))]

data3 <- data2[,colnames(data2) %in% final_features]

# IV. Data Preprocesing ----------------

# Convert other date columns 
head(data3$earliest_cr_line)
head(data3$sec_app_earliest_cr_line)

date_cols2 <- which(colnames(data3) %in% c('earliest_cr_line','sec_app_earliest_cr_line'))

for(i in date_cols2){
  data3[,i] <- paste('01-',data3[,i], sep = "")
  data3[,i] <- as.Date(data3[,i], format = "%d-%b-%Y")
}

summary(data3$earliest_cr_line)
summary(data3$sec_app_earliest_cr_line)

# correct emp_lengh
table(data3$emp_length)
table(data3$emp_length=='< 1 year')
data3$emp_length <- ifelse(data3$emp_length=='< 1 year','0 years',data3$emp_length)

table(data3$emp_length=='10+ years')
data3$emp_length <- ifelse(data3$emp_length=='10+ years','11 years',data3$emp_length)

table(data3$emp_length=='1 year')
data3$emp_length <- ifelse(data3$emp_length=='1 year','1 years',data3$emp_length)

data3$emp_length <- as.numeric(gsub(' years','',data3$emp_length))
summary(data3$emp_length)

# NA values

sapply(data3, function(x) sum(is.na(x)))

data3$desc <- NULL
data3$member_id <- NULL

# Fill empty values

fill1 <- c('emp_title', 'verification_status_joint')
fill1 <- which(colnames(data3) %in% fill1)


fill2 <- c('bc_open_to_buy', 'mo_sin_old_il_acct', 'mths_since_last_delinq',
          'mths_since_last_major_derog', 'mths_since_last_record',
          'mths_since_rcnt_il', 'mths_since_recent_bc', 'mths_since_recent_bc_dlq',
          'mths_since_recent_inq', 'mths_since_recent_revol_delinq',
          'pct_tl_nvr_dlq','sec_app_mths_since_last_major_derog')
fill2 <- which(colnames(data3) %in% fill2)

fill_min <- which(!c(1:ncol(data3)) %in% c(fill1,fill2))

# Fill empty 
for(i in fill1){
  data[,i] <- ifelse(is.na(data[,i]),'',data[,i])
  }

# Fill max 
for(i in fill2){
  data[,i] <- ifelse(is.na(data[,i]),max(data[,i]),data[,i])
}

#Fill min
for(i in fill_min){
  data[,i] <- ifelse(is.na(data[,i]),min(data[,i]),data[,i])
}

# Write data
write.csv(data3,"Cleaned Loan Data.csv", row.names = F)
