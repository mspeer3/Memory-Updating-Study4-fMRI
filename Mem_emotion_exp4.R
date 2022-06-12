

##Load packages
library(rstatix)


##Import data
me4_data <- read.csv(file = "Mem_emotion_exp4.csv", header = TRUE, stringsAsFactors = FALSE)



#Feeling change across conditions (24h later)
t.test(me4_data$Pos_feelch, me4_data$Con_feelch, paired=TRUE, var.equal=TRUE)

k <- select(me4_data, ID, Pos_feelch, Con_feelch)
l <- k %>% convert_as_factor(ID)
m <- gather(l, condition, feel_change, Pos_feelch:Con_feelch, factor_key=TRUE)
levels(m$condition)[levels(m$condition)=="Pos_feelch"] <- "Positive"
levels(m$condition)[levels(m$condition)=="Con_feelch"] <- "Control"
me4_feelch <- m %>% convert_as_factor(ID, condition)

cohens_d(me4_feelch, feel_change~condition, paired=TRUE)


#Feeling change across conditions (2-months later)
t.test(me4_data$P_feelch_2mon, me4_data$C_feelch_2mon, paired=TRUE, var.equal=TRUE)

k <- select(me4_data, ID, P_feelch_2mon, C_feelch_2mon)
l <- k %>% convert_as_factor(ID)
m <- gather(l, condition, feel_ch2m, P_feelch_2mon:C_feelch_2mon, factor_key=TRUE)
levels(m$condition)[levels(m$condition)=="P_feelch_2mon"] <- "Positive"
levels(m$condition)[levels(m$condition)=="C_feelch_2mon"] <- "Control"
me4_feelch2m <- m %>% convert_as_factor(ID, condition)
me4_feelch2m <- na.omit(me4_feelch2m) 

cohens_d(me4_feelch2m, feel_ch2m~condition, paired=TRUE)



##Comparing spearman rho values (correlating dissimilarity w/ feeling change across time) between positive
##and control conditions within Nacc, Hippocampus and VMPC ROIs. (For this analysis, we did a sign permutation
##test comparing the difference in spearman rho values between positive & control conditions using the
##nltools python toolbox. The t-tests below yield very similar statistics.)

##Nacc
t.test(me4_data$nacc_pos, me4_data$nacc_con, paired=TRUE, var.equal=TRUE)

k <- select(me4_data, ID, nacc_pos, nacc_con)
l <- k %>% convert_as_factor(ID)
m <- gather(l, condition, nacc, nacc_pos:nacc_con, factor_key=TRUE)
levels(m$condition)[levels(m$condition)=="nacc_pos"] <- "Positive"
levels(m$condition)[levels(m$condition)=="nacc_con"] <- "Control"
me4_nacc <- m %>% convert_as_factor(ID, condition)

cohens_d(me4_nacc, nacc~condition, paired=TRUE)


##Hippocampus
t.test(me4_data$hipp_pos, me4_data$hipp_con, paired=TRUE, var.equal=TRUE)

k <- select(me4_data, ID, hipp_pos, hipp_con)
l <- k %>% convert_as_factor(ID)
m <- gather(l, condition, hipp, hipp_pos:hipp_con, factor_key=TRUE)
levels(m$condition)[levels(m$condition)=="hipp_pos"] <- "Positive"
levels(m$condition)[levels(m$condition)=="hipp_con"] <- "Control"
me4_hipp <- m %>% convert_as_factor(ID, condition)

cohens_d(me4_hipp, hipp~condition, paired=TRUE)


##VMPFC
t.test(me4_data$vmpfc_pos, me4_data$vmpfc_con, paired=TRUE, var.equal=TRUE)

k <- select(me4_data, ID, vmpfc_pos, vmpfc_con)
l <- k %>% convert_as_factor(ID)
m <- gather(l, condition, vmpfc, vmpfc_pos:vmpfc_con, factor_key=TRUE)
levels(m$condition)[levels(m$condition)=="vmpfc_pos"] <- "Positive"
levels(m$condition)[levels(m$condition)=="vmpfc_con"] <- "Control"
me4_vmpfc <- m %>% convert_as_factor(ID, condition)

cohens_d(me4_vmpfc, vmpfc~condition, paired=TRUE)




