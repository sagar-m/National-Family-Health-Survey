
library(data.table)
setwd("~/Documents/nfhs/IAHR71DT")

data0 <- fread("IAHR72FL2.csv")

str(data2)

colnames(data) <- colnames(data0)
rm(data0)
#summary(data)
str(data0)

tail(data0[,77:82],20)


#delete columns which have null entries
data1 <- data1[,which(unlist(lapply(data1, function(x)!all(is.na(x))))),with=F]

str(data1)

#know which columns have only null entries
data0[,which(unlist(lapply(data0, function(x)all(is.na(x))))),with=F]
cols <- colnames(data0)
rm(ind)

# change char clm to factor

sapply(data2, class) 

### pre processing
ind <- data1[,lapply(.SD,is.character)]
ind <- names(ind[,.SD[T]])
### Convert multiple columns to factor
data1[,(ind):=lapply(.SD,factor),.SDcols=ind]

data3[["hv009"]] <- as.factor(data3[["hv009"]])
data3[["hv010"]] <- as.factor(data3[["hv010"]])
data3[["hv011"]] <- as.factor(data3[["hv011"]])
data3[["hv012"]] <- as.factor(data3[["hv012"]])
data3[["hv013"]] <- as.factor(data3[["hv013"]])

library(plyr)
data2 <- rename(data2, replace = c("hhid" = "HHID_Unique_Case_identification"))


data3 <- data0 %>%
select(hv009,hv010,hv011,hv012,hv013,hhid) %>%
data.frame(cbind(data2))

str(data3)
dim(data3)

summary(data2$hhid)


# change blanks to NA
data1$hv245[data1$hv245==""] <- "NA"
data1 <- droplevels(data1)


str(data2)

sum((data0$hv220 == data3$HV220_Age_of_head) == 0)

## remove unwanted cols
data3[["HV009_no._of_HH_members"]] <- NULL 
data3[["HV010_no._of_eligible_women"]] <- NULL
data3[["HV011_no._of_eligible_men"]] <- NULL
data3[["HV012_no._of_de_jure_household_members"]] <- NULL
data3[["HV010_no._of_eligible_women"]] <- NULL


#cols has nas
library(dplyr)

na_cols <- c(
"HV245 Hectares for agricultural land",
"HML1 No. of mosquito nets",
"HV228 Children slept under bednet",
"HML2 No. of children under bednet previous night",
"HV238 No. of households sharing toilet",
"HV236 Person fetching water",
"HV239 Food cooked on stove or open fire",
"HV225 HH shares a toilet with other HH", #2.2L
"HV235 Location of source for water", #1.8L
"HV242 HH has separate kitchen room" #1L
)
library(dplyr)

data2[ , -which(names(data2) %in% na_cols)]

data1 <- subset(data2, select = -c(20,23,29,30,41,42,44,49,55,56))


#remove rows with NA
data4 <- data3[complete.cases(data3), ] 
data4 <- na.omit(data3)

#rowSums(is.na(data1))
#table(is.na(data2))
sapply(data2, function(x) sum(is.na(x))) # same as colSums
#new_DF<-dplyr::filter(DF,is.na(Var2))
#DF %>% filter(is.na(Var2))
sum(complete.cases(data2))
colSums(is.na(data2))


# converting yes, don't know and nos to numbers
summary(data2[11])
#col[11:12]

for(i in 1:20) {
  if(nlevels(data0[[col[i]]])==3) {
    levels(data0[[col[i]]]) <- c(2,0,1)
  } else  {
    levels(data0[[col[i]]]) <- c(0,1)
  }
}

for(i in 1:4) {
  print(summary(data1[[col[i]]]))
}

summary(data0[['hv230b']]) #no 
summary(data2[['HV230B Presence of water at hand washing place']]) #no = 0
fact_(data2[['HV244 Own land usable for agriculture']])

col <- col[-11]

#data2[, (col) := factor(get(col), levels = rev(levels(get(col))))]

summary(data1[["HV232 Items present: Soap or detergent"]])

col <- c("HV206 Electricity",
"HV207 A radio",
"HV208 A television",
"HV209 A refrigerator",
"HV210 A bicycle",
"HV211 A motorcycle",
"HV212 A car",
"HV219 Sex of head", #M,#F
"HV221 HH has telephone",
"HV227 Have a bednet for sleeping",
#"HV230A Place where members wash their hands", #remove not observed
#"HV230B Presence of water at hand washing place", #"water is available "blank "water not available
"HV237 Anything done to water to make safe to drink", #has don"t know: 2, no: 0, yes: 1
"HV237A Boil", #has don"t know: 2, no: 0, yes: 1
"HV237B Add bleach/chlorine", #has don"t know: 2, no: 0, yes: 1
"HV237C Strain through a cloth", #has don"t know: 2, no: 0, yes: 1
"HV237D Use water filter", #has don"t know: 2, no: 0, yes: 1
"HV237F Let it stand and settle", # has don"t know: 2, no: 0, yes: 1
"HV237G Country specific", # has don"t know: 2, no: 0, yes: 1
"hv237h", #rename to country specific", #has don"t know: 2, no: 0, yes: 1
"HV237X Other", #has don"t know: 2, no: 0, yes: 1
"HV237Z Water usually treated by: don't know", #has don"t know: 2, no: 0, yes: 1
"HV243A Has a mobile telephone",
"HV243B Has a watch",
"HV243C Has an animal-drawn cart",
"HV244 Own land usable for agriculture", #0: no, #1 is yes
"HV246 Livestock, herds or farm animals",
"HV247 Any HH member has bank account", #includes don"t know, 3
"HV025 residence urban or rural" #0: rural, 1: urban
)

for(i in 2:4) {
  if(nlevels(data1[[col[i]]])==4) {
    levels(data1[[col[i]]]) <- c( ,0,1,NA)
  } else {
    levels(data1[[col[i]]]) <- c(NA,0,1)
  }
}

data1[["HV225 HH shares a toilet with other HH"]] <- data2[["HV225 HH shares a toilet with other HH"]]

summary(data1[["HV232B Items present: Ash, mud, sand"]])

col <-c(
"HV232 Items present: Soap or detergent",#includes blank, and NAs
"HV232B Items present: Ash, mud, sand", #includes blank, and NAs
"HV242 HH has separate kitchen room", #includes blank, and NAs
"HV225 HH shares a toilet with other HH" #has 4, blank in start and NA's last
)

#data3 <- data2[[col]]


## remove unwanted cols
data1[["HV015 Result of household interview"]] <- NULL 
data1[["HV020 ever-married"]] <- NULL
data1[["hv000"]] <- NULL
#####

#rename a column
colnames(data1)[which(names(data1) == "hv237h")] <- "HV237H Country specific"

rm(col, cols, i, na_cols, data)
str(data1)
table(data1[["HV247 Any HH member has bank account"]])
summary(data2)

write.csv(data3,file='data3333.csv', row.names = F)



head(data1[["HV000 Alphabetic country code"]])


head(data4[1:3,1:3])
data4[] <- lapply(data4, factor)
data4[["HHID_Unique_Case_identification"]] <- as.numeric(data4[["HHID_Unique_Case_identification"]])
summary(data4)
#rm(data3)

attach(data4)
levels(data4$HV230A_Place_where_members_wash_their_hands) <- c("not permitted to see", "not in dwelling", "not observed","observed")

library(plyr)
data4 <- rename(data4, replace = c("HV201-Major_source_of_drinking_water" = "HV201_Major_source_of_drinking_water"))

str(data4)

data4["HV204_Time_to_get_water_new"] <- data4["HV204_Time_to_get_to_water"]
levels(data4$HV204_Time_to_get_water_new)[97:99] <- c(1,NA,1)

data4$HV204_Time_to_get_water_new <- as.integer(data4$HV204_Time_to_get_water_new)
typeof(data4$HV204_Time_to_get_water_new)

levels(data4$HV230B_Presence_of_water_at_hand_washing_place) <- c(1,0)  

data4 <- rename(data4, replace = c("HV232B_Items_present:_Ash,_mud,_sand" = "HV232B_Ash_mud_sand_present"))

levels(data4$HV241_Food_is_cooked_where)

######

######## USE THIS


library(data.table)

setwd("~/Documents/nfhs/IAHR71DT")

data2 <- fread("data4.csv", na.strings="NA", stringsAsFactors=T)

data3$HV012_no._of_de_jure_household_members <- as.numeric(data3$HV012_no._of_de_jure_household_members)

colnames(data3)

#data3 <- data4[,c(1,3,4,20,22:31,38,42,43,45,47,56,57,60,61)] 

#colnames(data4[,c(1,3,4,20,22:31,38,42,43,45,47,56,57,60,61)])       

#remove rows with NA
data3 <- data3[complete.cases(data3), ] 

#rm(data4)
#colnames(data4)

d.samp <- data3[sample(1:dim(data3)[1], 40000,replace = F), ]





############ summarizing for slides

d <- data3 %>%
group_by(HV270_The_wealth_index) %>%
summarise(mean_hh = mean(HV012_no._of_de_jure_household_members), std = sd(HV012_no._of_de_jure_household_members)) %>%
ungroup()

getOption("scipen") 
options("scipen"=10) 

data0 %>%
group_by(hv012) %>%
count() %>%
ungroup() %>%
mutate(pct = paste0(round(100 * n/sum(n), 0), "%")) %>%
arrange(hv012) %>%
print(n = nrow(.))
#select(-c(n))

#summarise(sum(n), #sum(HV025_residence_urban_or_rural==1),sum(HV025_residence_urban_or_rural==0))

library(dplyr)
data0$hv014 <- as.numeric(data0$hv014)

data0 %>%
group_by(hv025,hv270) %>%
select(hv012) %>%
#summarise_at(vars(contains("237")), mean, na.rm = T) %>%
count() %>%
ungroup() %>%
mutate(pct = paste0(round(100 * n/sum(n), 0), "%")) %>%
arrange(desc(n))




data0 %>%
group_by(hv025,hv270) %>%
summarise(mean_hh = mean(hv012)) 


data3 %>%
group_by(HV025_residence_urban_or_rural,HV270_The_wealth_index) %>%
count(HV237_Anything_done_to_water_to_make_safe_to_drink) %>%
mutate(Frequency = paste0(round(100 * n/sum(n), 0), "%")) %>%
arrange(HV270_The_wealth_index)


data0 %>%
select(hv012,hv025) %>%
  mutate(Total_Members = case_when(
    hv012 > 14 ~ "15+",
    hv012 > 12 ~ "13-14",
    hv012 > 10 ~ "11-12",
    hv012 > 8 ~ "9-10",
    hv012 > 6 ~ "7-8",
    hv012 > 4 ~ "5-6",
    hv012 > 2 ~ "3-4",
    TRUE ~ "0-2")) %>%
  mutate(Total_Members = factor(Total_Members, 
                                    levels = c("11-12", "13-14", 
                                               "3-4", "5-6","7-8","15+","9-10","0-2"))) %>%
group_by(hv025,Total_Members) %>%
summarise(n = n()) %>%
mutate(Frequency = paste0(round(100 * n/sum(n), 0), "%")) %>%
arrange(desc(n))
#mutate(Frequency = n/sum(n))

data0 %>%
select(hv012,hv025, hv270) %>%
  mutate(Total_Members = case_when(
    hv012 > 14 ~ "15+",
    hv012 > 12 ~ "13-14",
    hv012 > 10 ~ "11-12",
    hv012 > 8 ~ "9-10",
    hv012 > 6 ~ "7-8",
    hv012 > 4 ~ "5-6",
    hv012 > 2 ~ "3-4",
    TRUE ~ "0-2")) %>%
  mutate(Total_Members = factor(Total_Members, 
                                    levels = c("11-12", "13-14", 
                                               "3-4", "5-6","7-8","15+","9-10","0-2"))) %>%
group_by(hv025,hv270,Total_Members) %>%
summarise(n = n()) %>%
ungroup() %>%
mutate(Frequency = paste0(round(100 * n/sum(n), 0), "%")) %>%
arrange(desc(hv025,n)) %>%
print(n = nrow(.))



## correct one
data0 %>%
select(hv014,hv270,hv012,hv025) %>%
  mutate(Total_Members = case_when(
    hv012 > 14 ~ "15+",
    hv012 > 12 ~ "13-14",
    hv012 > 10 ~ "11-12",
    hv012 > 8 ~ "9-10",
    hv012 > 6 ~ "7-8",
    hv012 > 4 ~ "5-6",
    hv012 > 2 ~ "3-4",
    TRUE ~ "0-2")) %>%
  mutate(Total_Members = factor(Total_Members, 
                                    levels = c("11-12", "13-14", 
                                               "3-4", "5-6","7-8","15+","9-10","0-2"))) %>%
count(hv025,hv014,Total_Members) %>%
ungroup() %>%
group_by(hv025) %>%
mutate(Frequency = paste0(round(100 * n/sum(n), 0), "%")) %>%
arrange(desc(hv025,hv014,n)) %>%
print(n = nrow(.))
#mutate(Frequency = n/sum(n)) 


data0 %>%
group_by(hv025,hv014) %>%
count() %>%
ungroup() %>%
mutate(pct = paste0(round(100 * n/sum(n), 0), "%")) %>%
arrange(hv014) %>%
print(n = nrow(.))


## correct one
data3 %>%
select(hv014,hv270,hv012,hv025) %>%
  mutate(Total_Members = case_when(
    HV012_no._of_de_jure_household_members > 30 ~ "31-40",
    HV012_no._of_de_jure_household_members > 20 ~ "21-30",
    HV012_no._of_de_jure_household_members > 10 ~ "11-20",
    TRUE ~ "0-10")) %>%
  mutate(Total_Members = factor(Total_Members, 
                                    levels = c("31-40","21-30", "11-20","0-10"))) %>%
count(HV025_residence_urban_or_rural,HV014_No._of_children,Total_Members) %>%
mutate(Frequency = paste0(round(100 * n/sum(n), 0), "%")) %>%
arrange(desc(HV014_No._of_children, Frequency)) %>%
print(n = nrow(.))
#mutate(Frequency = n/sum(n)) 


data3 %>%
group_by(HV025_residence_urban_or_rural, HV270_The_wealth_index, HV237_Anything_done_to_water_to_make_safe_to_drink) %>%
count(HV025_residence_urban_or_rural,HV270_The_wealth_index,HV237_Anything_done_to_water_to_make_safe_to_drink) %>%
ungroup() %>%
mutate(Frequency = paste0(round(100 * n/sum(n), 0), "%"))%>%
#mutate(Frequency = n/sum(n)) 
print(n = nrow(.))
#arrange(HV270_The_wealth_index)


data3 %>%
group_by(HV025_residence_urban_or_rural, HV205_Type_of_toilet_facility) %>%
count(HV025_residence_urban_or_rural,HV205_Type_of_toilet_facility) %>%
ungroup() %>%
mutate(Frequency = paste0(round(100 * n/sum(n), 0), "%"))%>%
#mutate(Frequency = n/sum(n)) 
print(n = nrow(.))

colnames(data3)


## correct one
data3 %>%
select(HV205_Type_of_toilet_facility,HV270_The_wealth_index,HV012_no._of_de_jure_household_members,HV025_residence_urban_or_rural) %>%
  mutate(Total_Members = case_when(
    HV012_no._of_de_jure_household_members > 30 ~ "31-40",
    HV012_no._of_de_jure_household_members > 20 ~ "21-30",
    HV012_no._of_de_jure_household_members > 10 ~ "11-20",
    TRUE ~ "0-10")) %>%
  mutate(Total_Members = factor(Total_Members, 
                                    levels = c("31-40","21-30", "11-20","0-10"))) %>%
count(HV025_residence_urban_or_rural,HV205_Type_of_toilet_facility,Total_Members) %>%
mutate(Frequency = paste0(round(100 * n/sum(n), 0), "%")) %>%
arrange(desc(Total_Members,Frequency)) %>%
print(n = nrow(.))
#mutate(Frequency = n/sum(n)) 

data0 %>%
select(hv205,hv270,hv012,hv025) %>%
  mutate(Total_Members = case_when(
    hv012 > 14 ~ "15+",
    hv012 > 12 ~ "13-14",
    hv012 > 10 ~ "11-12",
    hv012 > 8 ~ "9-10",
    hv012 > 6 ~ "7-8",
    hv012 > 4 ~ "5-6",
    hv012 > 2 ~ "3-4",
    TRUE ~ "0-2")) %>%
  mutate(Total_Members = factor(Total_Members, 
                                    levels = c("11-12", "13-14", 
                                               "3-4", "5-6","7-8","15+","9-10","0-2"))) %>%
count(hv025,hv205,Total_Members) %>%
ungroup() %>%
#group_by(hv025) %>%
mutate(Frequency = paste0(round(100 * n/sum(n), 0), "%")) %>%
arrange(desc(n)) %>%
print(n = nrow(.))
#mutate(Frequency = n/sum(n)) 



data3 %>%
group_by(HV025_residence_urban_or_rural, HV225_HH_shares_a_toilet_with_other_HH) %>%
count(HV025_residence_urban_or_rural,HV225_HH_shares_a_toilet_with_other_HH) %>%
ungroup() %>%
mutate(Frequency = paste0(round(100 * n/sum(n), 0), "%"))%>%
#mutate(Frequency = n/sum(n)) 
print(n = nrow(.))


colnames(data3)

data3 %>%
select(HV204_Time_to_get_water_new,HV270_The_wealth_index,HV025_residence_urban_or_rural) %>%
  mutate(Total_Time = case_when(
  	HV204_Time_to_get_water_new > 90 ~ "90+",
  	HV204_Time_to_get_water_new > 70 ~ "71-90",
  	HV204_Time_to_get_water_new > 50 ~ "51-70",
    HV204_Time_to_get_water_new > 30 ~ "31-50",
    HV204_Time_to_get_water_new > 10 ~ "11-30",
    TRUE ~ "0-10")) %>%
  mutate(Total_Time = factor(Total_Time, 
                                    levels = c("11-30","31-50", "51-70","0-10","71-90","90+"))) %>%
group_by(HV025_residence_urban_or_rural) %>%
count(HV025_residence_urban_or_rural,Total_Time) %>%
mutate(Frequency = paste0(round(100 * n/sum(n), 0), "%")) %>%
arrange(desc(HV025_residence_urban_or_rural)) %>%
print(n = nrow(.))

#######

