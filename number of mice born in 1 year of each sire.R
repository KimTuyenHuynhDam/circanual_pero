# Install and load the required packages

library(readxl)
library(tidyverse)
library(broom)
library(lubridate)
library(dplyr)
#on Window
setwd("C:/Users/huynhdam/My Drive/Kiaris lab/peromyscus/calculate children of sire, dam in 1 year")
#on Mac
#setwd("~/Google Drive/My Drive/Kiaris lab/peromyscus/calculate children of sire, dam in 1 year")


### get the data containing all mice information including the Dam, Sire info
mice_data=read_xlsx("full_with_Dam_Sire_IS.xlsx") %>%  
  ###re-format the date data
  mutate(Birthday = as.Date(Birthday, format = "%m/%d/%Y")) %>%  
  mutate(Birthday_Sire = as.Date(Birthday_Sire, format = "%m/%d/%Y")) %>%
  mutate(Birthday_Dam = as.Date(Birthday_Dam, format = "%m/%d/%Y")) %>%
  filter(! is.na(Birthday ) ) %>% #remove n.a values
  filter(! is.na(Birthday_Sire ) ) %>% #remove n.a values
  filter(! is.na(Birthday_Dam ) )


#View(mice_data)
##################################
#####the below codes mainly to select the children of Sire/Dam from the first time delivery until 1 year later
##Sire part
Sire_data = mice_data %>%  group_by(Sire) %>%  nest() 


#############

###########extract children of the first Sire
first_sire = Sire_data[[2]][[1]] 

first_delivery <- min(first_sire$Birthday)

# Calculate the one year later date
one_year_later <- first_delivery + 365  
one_year_sire = first_sire %>% filter(Birthday < one_year_later) 

Sire_full_1_year = Sire_data[[1]][[1]] %>% cbind(one_year_sire)

#View(Sire_full_1_year)
##########then continue with 2nd Sire until the end


for (i in 2: nrow(Sire_data) ) {

  each_sire = Sire_data[[2]][[i]] 
  
  first_delivery <- min(each_sire$Birthday)

  # Calculate the one year later date
  one_year_later <- first_delivery + 365  
  one_year_sire = each_sire %>% filter(Birthday < one_year_later) 
  
  temp= Sire_data[[1]][[i]] %>% cbind(one_year_sire)
  ###because we need to establish first dataframe of Sire_full_1_year,
  ##then we can bind the rows of all subsequent data of all remaining Sire
  Sire_full_1_year = Sire_full_1_year %>% rbind(temp) 
  
  }

colnames(Sire_full_1_year)[1] ='Sire'

#View(Sire_full_1_year)

###########

##########

#group_by_birthmonth_Sire = Sire_full_1_year %>%
#  group_by(BirthYear_Sire, BirthMonth_Sire) %>%
#  summarise(total_count=n(),.groups = 'drop') %>%
#  as.data.frame() %>%
#  pivot_wider(names_from = BirthMonth_Sire, values_from = total_count, names_sort = TRUE)

#View(group_by_birthmonth_Sire)


######
Dam_data = mice_data %>%
  group_by(Dam) %>%
  nest() 
###########extract children of the first Dam
first_dam = Dam_data[[2]][[1]] 

first_delivery <- min(first_dam$Birthday)

# Calculate the one year later date
one_year_later <- first_delivery + 365  
one_year_dam = first_dam %>% filter(Birthday < one_year_later) 

Dam_full_1_year = Dam_data[[1]][[1]] %>% cbind(one_year_dam)

#View(Dam_full_1_year)
##########then continue with 2nd Dam until the end
for (i in 2: nrow(Dam_data) ) {
  
  each_dam = Dam_data[[2]][[i]] 
  
  first_delivery <- min(each_dam$Birthday)
  
  # Calculate the one year later date
  one_year_later <- first_delivery + 365  
  one_year_dam = each_dam %>% filter(Birthday < one_year_later) 
  
  temp= Dam_data[[1]][[i]] %>% cbind(one_year_dam)
  ###because we need to establish first dataframe of Dam_full_1_year,
  ##then we can bind the rows of all subsequent data of all remaing Dam
  Dam_full_1_year = Dam_full_1_year %>% rbind(temp) 
  
}

colnames(Dam_full_1_year)[1] ='Dam'

#View(Dam_full_1_year)

####### export the data
#write.csv(Sire_full_1_year, 'IS - F1 - all mice - 1 year from 1st delivery of Sire.csv')
#write.csv(Dam_full_1_year, 'IS - F1 - all mice - 1 year from 1st delivery of Dam.csv')



###########group and count by BirthYear, then BirthMonth

#group_by_birthmonth_Dam = Dam_full_1_year %>%
#  group_by(BirthYear_Dam, BirthMonth_Dam) %>%
#  summarise(total_count=n(),.groups = 'drop') %>%
#  as.data.frame() %>%
#  pivot_wider(names_from = BirthMonth_Dam, values_from = total_count, names_sort = TRUE)

#View(group_by_birthmonth_Dam)



#write.csv(group_by_birthmonth_Dam, 'F1 birth month Dam (1 year from first delivery).csv')
#write.csv(group_by_birthmonth_Sire, 'F1 birth month Sire (1 year from first delivery).csv')
#####################################

#Dam_IS = read.csv("IS - F1 - all mice - 1 year from 1st delivery of Dam.csv")

###########group and count by BirthMonth of Dam/Sire, then BirthMonth of their chilrdren

Dam_by_birthmoth = Dam_full_1_year %>% filter(BirthYear < 2023) %>%
  group_by(BirthMonth_Dam, BirthMonth) %>%
  summarise(total_count=n(),.groups = 'drop') %>%
  as.data.frame() %>%
  pivot_wider(names_from = BirthMonth, values_from = total_count, names_sort = TRUE)

#write.csv(Dam_by_birthmoth, "Dam_by_birthmoth - IS.csv")




#Sire_IS = read.csv("IS - F1 - all mice - 1 year from 1st delivery of Sire.csv")


Sire_by_birthmoth = Sire_full_1_year %>% filter(BirthYear < 2023) %>%
  group_by(BirthMonth_Sire, BirthMonth) %>%
  summarise(total_count=n(),.groups = 'drop') %>%
  as.data.frame() %>%
  pivot_wider(names_from = BirthMonth, values_from = total_count, names_sort = TRUE)

#write.csv(Sire_by_birthmoth, "Sire_by_birthmoth - IS.csv")





###########group and count by Dam/Sire_by_birthmonth - 5 year interval
###########Dam part
list_len_Dam =  (max(Dam_full_1_year$BirthYear_Dam)- min(Dam_full_1_year$BirthYear_Dam))/5
#list_len_Dam
Dam_by_birthyear_month = vector(mode='list', length=round(list_len_Dam,0))

i=1

interval = min(Dam_full_1_year$BirthYear_Dam) 
while (interval < max(Dam_full_1_year$BirthYear_Dam)) {
  interval2 = interval +5
 
    Dam_by_5_year = Dam_full_1_year %>% filter(BirthYear_Dam >= interval & BirthYear_Dam <interval2) 
    Dam_by_birthmonth =  Dam_by_5_year  %>% 
      group_by(BirthMonth_Dam, BirthMonth) %>%
      summarise(total_count=n(),.groups = 'drop') %>%
      as.data.frame() %>%
      pivot_wider(names_from = BirthMonth, values_from = total_count, names_sort = TRUE)
    
    #Dam_by_birthyear_month = append(Dam_by_birthyear_month,Dam_by_birthmonth)
    Dam_by_birthyear_month[[i]] = Dam_by_birthmonth
    names(Dam_by_birthyear_month)[i] <- paste(interval, "-",interval2-1)
    i = i +1
  interval = interval2
 
}
#View(Dam_by_birthyear_month)
names(Dam_by_birthyear_month)

lapply(Dam_by_birthyear_month, function(BirthMonth) write.table( data.frame(BirthMonth),na = "",
                                                                 'Dam_by_birthmonth - 5 year interval - IS.csv',
                                                                 row.names = F, append= T, sep=',' ))

#write.csv(Dam_by_birthyear_month, "Dam_by_birthmonth - 5 year interval - IS.csv")

###########Sire part
list_len_Sire =  (max(Sire_full_1_year$BirthYear_Sire)- min(Sire_full_1_year$BirthYear_Sire))/5
#list_len_Sire
Sire_by_birthyear_month = vector(mode='list', length=round(list_len_Sire,0))

i=1

interval = min(Sire_full_1_year$BirthYear_Sire)
while (interval < max(Sire_full_1_year$BirthYear_Sire)) {
  interval2 = interval +5
  
  Sire_by_5_year = Sire_full_1_year %>% filter(BirthYear_Sire >= interval & BirthYear_Sire <interval2) 
  Sire_by_birthmonth =  Sire_by_5_year  %>% 
    group_by(BirthMonth_Sire, BirthMonth) %>%
    summarise(total_count=n(),.groups = 'drop') %>%
    as.data.frame() %>%
    pivot_wider(names_from = BirthMonth, values_from = total_count, names_sort = TRUE)
  
  #Sire_by_birthyear_month = append(Sire_by_birthyear_month,Sire_by_birthmonth)
  Sire_by_birthyear_month[[i]] = Sire_by_birthmonth
  names(Sire_by_birthyear_month)[i] <- paste(interval, "-",interval2-1)
  i = i +1
  interval = interval2
  
}
#View(Sire_by_birthyear_month)
names(Sire_by_birthyear_month)

lapply(Sire_by_birthyear_month, function(BirthMonth) write.table( data.frame(BirthMonth),na = "",
                                                                 'Sire_by_birthmonth - 5 year interval - IS.csv',
                                                                 row.names = F, append= T, sep=',' ))

#write.csv(Sire_by_birthyear_month, "Sire_by_birthmonth - 5 year interval - IS.csv")


###################################### group by birthMonth Sire, then birth year, birthmomth of F1


Dam_by_birthmonth_thenF1 = Dam_full_1_year %>% filter(BirthYear < 2023) %>%
  group_by( BirthMonth_Dam, BirthYear, BirthMonth) %>%
  summarise(total_count=n(),.groups = 'drop') %>%
  as.data.frame() %>%
  pivot_wider(names_from = BirthMonth, values_from = total_count, names_sort = TRUE)

write.csv(Dam_by_birthmonth_thenF1, "By birthMonth Dam, then birth year, birthmomth of F1- IS.csv", na = "")


Sire_by_birthmonth_thenF1 = Sire_full_1_year %>% filter(BirthYear < 2023) %>%
  group_by( BirthMonth_Sire, BirthYear, BirthMonth) %>%
  summarise(total_count=n(),.groups = 'drop') %>%
  as.data.frame() %>%
  pivot_wider(names_from = BirthMonth, values_from = total_count, names_sort = TRUE)

write.csv(Sire_by_birthmonth_thenF1, "By birthMonth Sire, then birth year, birthmomth of F1- IS.csv", na = "")







########################
#install.packages("ShellChron")
#library(ShellChron)

#View(Sire_by_birthmoth)

#test = t(Sire_by_birthmonth) 
#test2 = t(test)
#test2
#sinreg(y=test2[1], y =test2[2], fixed_period = NA, plot = T)
