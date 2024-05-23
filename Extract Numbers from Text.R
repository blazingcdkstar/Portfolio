
# ---------------------------------------------------- load packages  ------------------------------------------------------- #

# install pacman
if (!require("pacman")) install.packages("pacman")
# install and load packages
p_load(dplyr, ggplot2, lubridate, stringr, qdapRegex, tidyr, openxlsx)


# ---------------------------------------------------- import data  ------------------------------------------------------- #


#import live cfpb data
# set min/max date range for import
mindate <- Sys.Date() - years(1)
maxdate <- Sys.Date()

# create the url. this is split into 3 sections to capture the date critier in the url.

starturl <- "https://www.consumerfinance.gov/data-research/consumer-complaints/search/api/v1/?date_received_max="
midurl <- "&date_received_min="
endurl <- "&field=all&format=csv&has_narrative=true&no_aggs=true&product=Mortgage&size=11282&sort=created_date_desc"

# combine the three sections to form the total url
# for example, the start url is pasted with the maxdate, to get the "date_received_max=maxdate"
# midurl is pasted to mindate to get the date_received_min=mindate

myurl <- paste(starturl,maxdate,midurl,mindate,endurl,sep = "")

# read in the data using the url into a dataframe called df
df <- read.csv(myurl)



# ---------------------------------------------------- format data  ------------------------------------------------------- #
# format date columns
df$Date.received <- mdy(df$Date.received)
df$Date.sent.to.company <- mdy(df$Date.sent.to.company)
df$Mth.Received <- floor_date(df$Date.received, unit = 'month')

# create small data set, with only text, date, and id fields.
text_df <- df %>% select(Date.received, Mth.Received, Complaint.ID, Company, Consumer.complaint.narrative)

# copy text column to new columns for cleaning and extracting data
text_df$complaint.work <- text_df$Consumer.complaint.narrative

# capture character count
text_df$cc <- str_count(text_df$Consumer.complaint.narrative)
# capture count of spaces
text_df$wc <- str_count(rm_white(text_df$Consumer.complaint.narrative), " ")


# ------------------------------- Extract the Values that are between Curly Braces {} ---------------------------------------- #

# capture the count of left curly braces
text_df$left.curl.count <- str_count(text_df$complaint.work, "\\{")

# capture the count of right curly braces
text_df$right.curl.count <- str_count(text_df$complaint.work, "\\}")

# check that the curly brace count matches
text_df$matching.curl.count <- text_df$left.curl.count - text_df$right.curl.count

# create dataframe named nomatch to identify records without a match
nomatch <- text_df %>%
  select(Complaint.ID, left.curl.count, right.curl.count, matching.curl.count) %>% 
  filter(matching.curl.count != 0)

# if there are records with non-matching counts, print the population and a warning.
if (nrow(nomatch) > 0){
  print(nomatch)
  warning('These records have non-matching braces')
}

# find the character count for the left curly brace
text_df$left.curl <- regexpr(pattern = "{",
                             text = text_df$complaint.work,
                             fixed = TRUE)

# find the character count for the right curly brace
text_df$right.curl <- regexpr(pattern = "}",
                              text = text_df$complaint.work,
                              fixed = TRUE)

# extract the text between the curly braces
text_df$curly.braces <- ex_between(text.var = text_df$complaint.work, left = "{", right = "}")


# ------------------------------- Clean the curly brace data ---------------------------------------- #

# create new data frame, only with the complaint id and the text extracted from between the curly braces
clean_curly_braces <- text_df %>%
  select(Complaint.ID, curly.braces)

# remove records with no data in curly braces
clean_curly_braces <- clean_curly_braces[-which(is.na(clean_curly_braces$curly.braces)),]

# put each value, separated by a comma, into its own column
clean_curly_braces <- clean_curly_braces %>% unnest_wider(curly.braces, names_sep = ",")

# capture the column count
mycolcount <- dim(clean_curly_braces)[2]

# make sure the data frame clean_curly_braces is a dataframe
clean_curly_braces <- as.data.frame(clean_curly_braces)

# make the Complaint.ID a string rather than an integer
clean_curly_braces$Complaint.ID <- as.character(clean_curly_braces$Complaint.ID)

# go through each column, and remove non-numeric characters, and format as a number
for (i in 2:mycolcount) {
  
  mycol <- names(clean_curly_braces[i])
  clean_curly_braces[,mycol] <- gsub("[^0-9.-]", "", clean_curly_braces[,mycol])
  clean_curly_braces[,mycol] <- as.numeric(clean_curly_braces[,mycol])
  
}


# change data from wide to long format
curly_vals <- clean_curly_braces %>% pivot_longer(!Complaint.ID)

# identify row index of records where the value is na
naind <- which(is.na(curly_vals$value))

# remove the rows with na for value
curly_vals <- curly_vals[-naind,]


# ------------------------------- Create Summary Pivot and Join to DataSet---------------------------------------- #
# create pivot to capture the minimum, maximum, sum, and count of values in the curly braces
curly_vals_pivot <- curly_vals %>% 
  select(Complaint.ID, value) %>% 
  group_by(Complaint.ID) %>% 
  summarise(Unique_Val_List = paste(sort(unique(value)),collapse = ', '),
            Min_Amt = min(value),
            Max_Amt = max(value),
            Sum_Amt = sum(value),
            Val_Count = n(),
            Unique_Val_Count = length(sort(unique(value))))
            
# join the summary data to the text data
text_df$Complaint.ID <- as.character(text_df$Complaint.ID)
text_df <- text_df %>% left_join(curly_vals_pivot, by = 'Complaint.ID')

# create final clean output with only needed columns
text_df_fin <- text_df %>% 
  select(Date.received, Mth.Received, Complaint.ID, Company, 
         Unique_Val_List, Min_Amt, Max_Amt, Sum_Amt, Val_Count, Unique_Val_Count,Consumer.complaint.narrative)


# --------------------------------------------- Export Results -------------------------------------------------- #
# set working directory to same folder that script is saved in
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

write.xlsx(text_df_fin, 'Extract Numbers from Text.xlsx', rowNames = FALSE)
