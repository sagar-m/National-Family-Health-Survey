#working plots

########### WORKED!!! ### where's the axis?

#attach(data4)

# Create DF with table()
df <- table(data3$HV247_Any_HH_member_has_bank_account,data3$HV230A_Place_where_members_wash_their_hands)

# Use apply on DF to get frequency of each group
DF_freq <- apply(df, 2,function(x) x/sum(x))

# Load reshape2 and use melt on DF to create DF_melted
require(reshape2)
DF_melted <- melt(DF_freq)

# Change names of DF_melted
names(DF_melted) <- c("FILL", "X", "value")

BMI_fill <- scale_fill_brewer("HV247_Any_HH_member_has_bank_account", palette = "Reds")

# Add code to make this a faceted plot
ggplot(DF_melted, aes(x = factor(X), y = value, fill = factor(FILL))) +
  geom_bar(stat = "identity", position = "stack") +
  BMI_fill + 
  facet_grid(FILL ~ .) +
  theme(legend.position = "bottom") +
  scale_x_discrete("HV230A_Place_where_members_wash_their_hands") +
  scale_y_discrete("HV247_Any_HH_member_has_bank_account")

########################

# Create DF with table()
df <- table(data4$HV024_Region_of_residence,data4$HV204_Time_to_get_to_water, data4$HV201_Major_source_of_drinking_water)


###### BARPLOT


d <- ggplot(data3, aes(HV234A_Result_of_salt_test_for_iodine,HV204_Time_to_get_water_new)) + scale_y_continuous("Time to get water (h)",  
                                                                                                                       limits = c(0, 30),   breaks = seq(0, 30, 10),   expand = c(0, 0)) +
  scale_x_discrete("Iodine") +
  theme_classic()

d + 
  stat_summary(fun.y = mean, geom = "bar",   fill = "grey50")
#stat_summary(fun.data = mean_sdl, mult = 1,   geom = "errorbar", width = 0.2)


########## ggpairs

# Use sample data

d.samp <- data4[sample(1:dim(data4)[1], 1000), ]

table(d.samp[,21:21])

library(GGally)

# Different aesthetics for different plot sections and plot types
pm <- ggpairs(
  data3[, c(38,43,45)],
  mapping = ggplot2::aes(color = HV247_Any_HH_member_has_bank_account),
  upper = list(continuous = wrap("density", alpha = 0.5), combo = "box_no_facet"),
  lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot_no_facet", alpha = 0.4)),
  columnLabels = c("mobile_ph", "Bank","Econ_status")
)

#### FACETED PLOT


# Create DF with table()
df <- table(data3$HV009_no._of_HH_members,data3$HV205_Type_of_toilet_facility, data3$HV025_residence_urban_or_rural)

# Use apply on DF to get frequency of each group
DF_freq <- apply(df, 2,function(x) x/sum(x))

# Load reshape2 and use melt on DF to create DF_melted
library(reshape2)
DF_melted <- melt(df)

# Change names of DF_melted
names(DF_melted) <- c("HH_members", "Toilet_type","Urb_Rur", "value")
head(DF_melted)

ggplot(DF_melted, aes(x = HH_members, y = value, col = Toilet_type)) +
  geom_jitter() +
  facet_grid(. ~ Urb_Rur) +
  theme(legend.position = "bottom")


############
getOption("scipen") 
options("scipen"=10) 


#### Histogram


ggplot(data0, aes(x = hv012, fill=hv025)) +
  geom_histogram(stat = "count", binwidth = 0.8, position = "stack")


## density plots ## worked
  
ggplot(data3, aes(x = HV204_Time_to_get_water_new, fill=HV025_residence_urban_or_rural)) +
geom_density(col = NA, alpha = 0.35) +
theme_classic() +
theme(legend.position = "bottom")


# add weights

data1 <- data0 %>%
group_by(hv270) %>%
#summarize(N = n()) %>%
mutate(x = n()/nrow(data0))
#mutate(x = N/nrow(data0))


sum(data1$x)
data1[100000:100007,87:89]

#weighted density plot # can't make out the colors, how to get the outline

data1$hv012 <- as.numeric(data1$hv012)

#HV270_The_wealth_index
#HV025_residence_urban_or_rural
#worked earlier
ggplot(data1, aes(x = hv012, fill=hv025)) + 
geom_density(aes(weight = x), col = NA, alpha = 0.35) +
theme_classic() +
        theme(legend.position = "bottom") +
        labs(x = "No. of Household Members") +
        labs(fill='Rural or Urban') 
#new density plot
ggplot(data0, aes(x = hv012,..count..,fill=hv025)) +
geom_density(aes(position="fill"), col = NA, alpha = 0.35) +
theme_classic() +
        theme(legend.position = "bottom") +
        labs(x = "No. of Household Members") +
        labs(fill='Rural or Urban') +
        xlim(0,20)

ggplot(data0, aes(x = hv012,fill=hv025)) +
geom_density(aes(position="fill"), col = NA, alpha = 0.35) +
theme_classic() +
        theme(legend.position = "bottom") +
        labs(x = "No. of Household Members") +
        labs(fill='Rural or Urban') +
        xlim(0,20)




#violin plot
ggplot(data1, aes(x = hv270, y = hv012, fill=hv270)) +
geom_violin(aes(weight = n), col = NA) +
theme_classic() +
        theme(legend.position = "bottom") +
        labs(y = "No. of Household Members", x = "") +
        labs(fill='Wealth') +
        ylim(0,20)



#faceted histogram

ggplot(data1, aes (x = hv012, fill= factor(hv025))) + 
  geom_histogram(binwidth = 1, stat = "count") +
        facet_grid(hv025 ~ .) +
        theme_classic() +
        theme(legend.position = "bottom") +
        labs(x = "No. of Household Members") +
        labs(fill='Rural or Urban') 


#worked, now error:     
ggplot(data1, aes (x = hv012, fill= factor(hv270))) + 
  geom_histogram(binwidth = 1, stat = "count",aes(y = density)) +
        facet_grid(hv025 ~ .) +
        theme_classic() +
        theme(legend.position = "bottom") +
        labs(x = "Wealth")

#worked, don't use
ggplot(data1, aes (x = hv012, fill= factor(hv270))) + 
  geom_histogram(binwidth = 1, aes(y = ..count../sum(..count..)), position = "fill", stat="count") +
   theme_classic()     

#worked
ggplot(data1, aes (x = hv012, fill= factor(hv270))) + 
  geom_histogram(binwidth = 1, stat = "count") +
        facet_grid(hv025 ~ .) +
        theme_classic() +
        theme(legend.position = "bottom") +
        labs(x = "No. of Members in HH", fill = "Wealth") +
        xlim(0,20)

#hv014 = children
#HV243A_Has_a_mobile_telephone

### geom point plot

ggplot(data0, aes(x = hv014, y = hv012, colour=hv270, size = hv025)) +
geom_point(alpha = 0.3) +
theme_classic() +
labs(x = "No. of children in HH", fill = "Wealth", size = "Rural/Urban", y = "No. of Members") +
theme(legend.position = "bottom")


# geom_line

ggplot(data0, aes(hv270, fill = hv012)) + 
geom_bar()




## Scales ## worked

d.samp <- data3[sample(1:dim(data3)[1], 40000,replace = F), ]

ggplot(data0, aes(x = hv012, y = hv014, col=hv025)) +
  geom_jitter() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") +
  labs(y = "No. of children in HH", col = "Rural/Urban", x = "No. of Members")


## grouping by fill ## worked

#d.samp <- table(data3$HV237D_Use_water_filter,data3$HV237C_Strain_through_a_cloth,data3$`HV237B_Add_bleach/chlorine`)

d.samp <- table(data3$HV237C_Strain_through_a_cloth,data3$`HV237B_Add_bleach/chlorine`)


ggplot(data3, aes(x = factor(HV237C_Strain_through_a_cloth), fill = factor(`HV237B_Add_bleach/chlorine`))) +
  geom_bar() + # or stat_bin()
  facet_grid(. ~ HV237D_Use_water_filter) +
  theme(legend.position = "bottom") +
  
  ## add title Use_water_filter
  
  
  












#HEATMAP

# Create color palette
myColors <- brewer.pal(9, "Reds")

# Build the heat map from scratch

ggplot(data4, aes(x = HV210_A_bicycle, y = HV201_Major_source_of_drinking_water, fill = data4$HV204_Time_to_get_water_new)) +
  geom_tile() +
  facet_wrap(~HV024_Region_of_residence, ncol = 1) +
  scale_fill_gradientn(colors = myColors)

#heat map alternative

# The heat map we want to replace
# Don't remove, it's here to help you!
myColors <- brewer.pal(9, "Reds")
ggplot(data4, aes(x = HV201_Major_source_of_drinking_water, y = HV210_A_bicycle, fill = data4$HV204_Time_to_get_water_new)) +
  geom_tile() +
  facet_wrap( ~ HV024_Region_of_residence, ncol = 1) +
  scale_fill_gradientn(colors = myColors)

# Line plots
ggplot(data4, aes(x=HV210_A_bicycle, y = data4$HV204_Time_to_get_water_new, col = HV201_Major_source_of_drinking_water, group = HV201_Major_source_of_drinking_water)) +
  geom_line() +
  facet_wrap(~ site, nrow = 1)

#alternative 2

# Create overlapping ribbon plot from scratch
ggplot(data4, aes(x = HV210_A_bicycle, y = data4$HV204_Time_to_get_water_new, col = HV024_Region_of_residence, group = HV024_Region_of_residence, fill = HV024_Region_of_residence)) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "ribbon", alpha = 0.1, col = NA)



