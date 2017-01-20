## 


rm(list = ls())
require(Hmisc)

output_dataframe <- read.csv("2017-01-17-22-07-05_output_dataframe b.csv", stringsAsFactors = F)
guideline_master <- read.csv("14_11_12_master_guideline list.csv",
                             stringsAsFactors = F)

# some checking - if this is empty all is good
output_dataframe[is.na(output_dataframe$Agreement.amended), c("Guideline", "Recommendation.number", "individual_cr", "Agreement.amended")]
# check that all the cochrane reviews are written before the guidelines
colnames(output_dataframe)
# if this is true things are all good:
length(which(output_dataframe$Publish.year < output_dataframe$cochrane_year + 1)) == 0

#######################################################################
# now we can do the analysis looking at the pairs 
#  - as this has been curated for the guideline year etc

output_dataframe$unique_rec <- paste(output_dataframe$Guideline, 
                                     output_dataframe$Recommendation.number, 
                                     sep = "_")

# lets get the total number of guideline recommendations which could
# use cochrane reviews
unique_recommendations <- unique(output_dataframe$unique_rec)
length(unique_recommendations)

check_cochrane_use <- function(unique_rec, verbose = F){
  # function which takes the unique rec, and 
  # finds all the linked cochrane reviews
  # and calculates the proportion which were found
  # by the guideline authors 
  fdata <- output_dataframe[which(output_dataframe$unique_rec == unique_rec), ]
  if(verbose){
    print(fdata[, c("unique_rec", "individual_cr", "Guideline_found")])
  }
  return(length(which(fdata$Guideline_found))/nrow(fdata))
}

check_cochrane_use("G0001_9", verbose = T)

## make a new table of the unique guideline recommendations
## and have the cochrane use rate next to it

recommendation_cochrane_rate <- cbind(unique_recommendations, rep(NA, length(unique_recommendations)))


for (i in 1:nrow(recommendation_cochrane_rate)){
  recommendation_cochrane_rate[i, 2] <- check_cochrane_use(recommendation_cochrane_rate[i, 1])
}
colnames(recommendation_cochrane_rate) <- c("unique_recommendations",
                                            "cochrane_rate")
recommendation_cochrane_rate <- as.data.frame(recommendation_cochrane_rate, stringsAsFactors = F)
class(recommendation_cochrane_rate$cochrane_rate)
recommendation_cochrane_rate$cochrane_rate <- as.numeric(recommendation_cochrane_rate$cochrane_rate)


length(which(recommendation_cochrane_rate[, 2] == 1)) # finds all the CR
length(which(recommendation_cochrane_rate[, 2] == 0)) # finds no CR
length(which(recommendation_cochrane_rate[, 2] >  0 
             & recommendation_cochrane_rate[, 2] <  1)) # finds some CR

### now add in details for the use of alternate high level evidence
check_other_evidence_use <- function(unique_rec, verbose = F){
  # function which takes the unique rec, and 
  # finds out if alternate high level evidence was used
  fdata <- output_dataframe[which(output_dataframe$unique_rec == unique_rec), ]
  if(verbose){
    print(fdata[, c("unique_rec", "individual_cr", "Guideline_found", "Other.high.level.evidence")])
  }
  return(fdata$Other.high.level.evidence[1])
}

check_other_evidence_use("G0001_9", verbose = T)
recommendation_cochrane_rate$Other.high.level.evidence <- NA
for (i in 1:nrow(recommendation_cochrane_rate)){
  recommendation_cochrane_rate$Other.high.level.evidence[i] <- 
    check_other_evidence_use(recommendation_cochrane_rate[i, 1])
}

get_commisioning_agency <- function(guideline){
  ## function which takes a guideline and looks in output_dataframe for the 
  ## comissioning agency
  ## use a substring to get the first 5 characters of a guideline string - allows
  ## us to use the unique guidline_recommendation string
  guideline <- substr(guideline, start = 1, stop = 5)
  
  return(output_dataframe[which(output_dataframe$Guideline == guideline), 
                          "Commission.agency"][1])
}

get_commisioning_agency("G0001_09")
recommendation_cochrane_rate$Commision.agency <- NA
for (i in 1:nrow(recommendation_cochrane_rate)){
  recommendation_cochrane_rate$Commision.agency[i] <- 
    get_commisioning_agency(recommendation_cochrane_rate$unique_recommendations[i])
}


recommendation_cochrane_rate$found_all <- 
  ifelse(recommendation_cochrane_rate$cochrane_rate == 1, 1, 0)

recommendation_cochrane_rate$found_none <- 
  ifelse(recommendation_cochrane_rate$cochrane_rate == 0, "found_none", "found_some or all")

recommendation_cochrane_rate$found_category <- "found_some"
recommendation_cochrane_rate$found_category[
  which(recommendation_cochrane_rate$cochrane_rate == 1)] <- "found_all"
recommendation_cochrane_rate$found_category[
  which(recommendation_cochrane_rate$cochrane_rate == 0)] <- "found_none"



get_agreement_rate <- function(unique_rec, verbose = F){
  # function which takes the unique rec, and 
  # finds out proportion of times in which there is complete agreement between
  # the recommendation and cochrane review 
  fdata <- output_dataframe[which(output_dataframe$unique_rec == unique_rec), ]
  if(verbose){
    print(fdata[, c("unique_rec", "individual_cr", "Guideline_found", "Agreement.amended")])
  }
  return(length(which(fdata$Agreement.amended == 1)) / nrow(fdata))
}
get_agreement_rate("G0001_9", verbose = T)
recommendation_cochrane_rate$Proportion.totally.agree <- NA
for (i in 1:nrow(recommendation_cochrane_rate)){
  recommendation_cochrane_rate$Proportion.totally.agree[i] <- 
    get_agreement_rate(recommendation_cochrane_rate$unique_recommendations[i])
}

####################################################################################
# Agreement - NOW AUDITED
#0 = inapplicable
#1 = completely/  No evidence in CR but the guideline has to make a recommendation and takes in mind the fifferent target group, adverse effects, pharmacovigilance, costs etc. 
#2 = partial 
#3 = no agreement
#4 = No evidence and guidelines makes a strong recommendation

table(output_dataframe$Agreement.amended)
sum(table(output_dataframe$Agreement.amended))
length(which(output_dataframe$Agreement.amended != 1))
length(which(output_dataframe$Agreement.amended == 2))
length(which(output_dataframe$Agreement.amended == 3))
length(which(output_dataframe$Agreement.amended == 4))


################################################################################
# influence of guideline agency 

colnames(recommendation_cochrane_rate)
tab <- table(recommendation_cochrane_rate$Commision.agency, 
             recommendation_cochrane_rate$found_all)
tab <- as.data.frame.matrix(tab)
print(tab)
fisher.test(tab)
tab$sum <- 0
tab$sum = apply(tab, 1, sum)
tab

tab <- cbind(tab, binconf(tab[, "1"], tab$sum))
tab <- tab[order(tab$PointEst), ]
require(gplots)
png("fig 2a.png")
par(mar = c(8, 4, 4, 2))

barplot2(tab$PointEst, width = .9, space = .1, ylim = c(0, 1),
         ylab = c("Proportion of recommendations referencing", "all Cochrane Reviews"))
text(x = seq(1:nrow(tab)) - 0.25, y = -0.05, rownames(tab), xpd = T, pos = 2, srt = 45)
draw_CI <- function(x, low, high){
  lines(x = c(x, x),
        y = c(low, high),
        lwd = 3)
}
for(n in 1:nrow(tab)){
  draw_CI(n - 0.5, tab[n, "Lower"], tab[n, "Upper"])
}
dev.off()
tab
####################################################################
##
## influence of topic of guideline

get_topic <- function(guideline){
  ## function which takes a guideline and looks in output_dataframe for the 
  ## topic
  ## use a substring to get the first 5 characters of a guideline string - allows
  ## us to use the unique guidline_recommendation string
  guideline <- substr(guideline, start = 1, stop = 5)
  
  return(output_dataframe[which(output_dataframe$Guideline == guideline), 
                          "Topic.of.guideline"][1])
}

get_topic("G0001_09")


recommendation_cochrane_rate$Topic <- NA
for (i in 1:nrow(recommendation_cochrane_rate)){
  recommendation_cochrane_rate$Topic[i] <- 
    get_topic(recommendation_cochrane_rate$unique_recommendations[i])
}

tab2 <- table(recommendation_cochrane_rate$Topic, 
             recommendation_cochrane_rate$found_all)
tab2 <- as.data.frame.matrix(tab2)
fisher.test(tab2)
tab2$sum <- 0
tab2$sum = apply(tab2, 1, sum)
tab2

tab2 <- cbind(tab2, binconf(tab2[, "1"], tab2$sum))
tab2 <- tab2[order(tab2$PointEst), ]
require(gplots)
png("figure2b.png")
par(mar = c(8, 4, 4, 2))
barplot2(tab2$PointEst, width = .9, space = .1, ylim = c(0, 1),
         ylab = c("Proportion of recommendations referencing", "all Cochrane Reviews"))
text(x = seq(1:nrow(tab2)) - 0.25, y = -0.05, rownames(tab2), xpd = T, pos = 2, srt = 45)
draw_CI <- function(x, low, high){
  lines(x = c(x, x),
        y = c(low, high),
        lwd = 3)
}
for(n in 1:nrow(tab2)){
  draw_CI(n - 0.5, tab2[n, "Lower"], tab2[n, "Upper"])
}
dev.off()
tab2

######################################################################
#
# Guideline publication year

get_publish_year <- function(guideline){
  ## function which takes a guideline and looks in output_dataframe for the 
  ## topic
  ## use a substring to get the first 5 characters of a guideline string - allows
  ## us to use the unique guidline_recommendation string
  guideline <- substr(guideline, start = 1, stop = 5)
  
  return(output_dataframe[which(output_dataframe$Guideline == guideline), 
                          "Publish.year"][1])
}

get_publish_year("G0001_09")
recommendation_cochrane_rate$Year <- NA
for (i in 1:nrow(recommendation_cochrane_rate)){
  recommendation_cochrane_rate$Year[i] <- 
    get_publish_year(recommendation_cochrane_rate$unique_recommendations[i])
}


##########################################################################################
#
#  Data analysis for table 2

table(output_dataframe$Guideline)
lookup_guideline_name <- function(g_num){
  guideline_master$Name[which(substr(guideline_master$Filename, 1, 5) == g_num)]
}
lookup_guideline_name("G0002")

table(output_dataframe$Guideline)

get_table_2_data <- function(g_num){
  fdata <- recommendation_cochrane_rate[
    which(substr(recommendation_cochrane_rate$unique_recommendations, 1, 5) == g_num), ]
  # print(fdata)
  print(paste(lookup_guideline_name(g_num)))
  print(paste("number of CR available =", nrow(fdata)))
  print(paste("number of guidelines which totally agree with CR", 
              length(which(fdata$Proportion.totally.agree == 1))))
  print(paste("table of finding relevant cochrane reviews:"))
  print(table(fdata$found_category))
  return(c(
    g_num,
    lookup_guideline_name(g_num),
    nrow(fdata), #number of recommendations for which a CR is available
    length(which(fdata$Proportion.totally.agree == 1)),
    length(which(fdata$found_category == "found_all")),
    length(which(fdata$found_category == "found_some")),
    length(which(fdata$found_category == "found_none"))
    ))
}

gs <- unique(output_dataframe$Guideline)

get_table_2_data("G0011")

table_2 <- lapply(X = gs, FUN = get_table_2_data)
table_2 <- do.call(rbind, table_2)  
write.csv(table_2, file = "table2.csv")

###############################################################################
#
#  Mixed effects model
#  dataframe = recommendation_cochrane_rate
#  outcome = 'found_all'

write.csv(recommendation_cochrane_rate, "rcr.csv")
rm(list = ls())

rcr <- read.csv("rcr.csv")
rcr$Guideline <- substr(rcr$unique_recommendations, 1, 5)
head(rcr$Guide)


head(rcr)
within(rcr, {
  found_all <- factor(found_all, levels = 0:1, labels = c("no", "yes"))
  Guideline <- factor(Guideline)
  Commision.agency <- factor(Commision.agency)
  Topic <- factor(Topic)
})

require(ggplot2)
require(GGally)
require(reshape2)
require(lme4)



#######################################################################
# get rid of guidelines with only one data point
colnames(rcr)
table(rcr$Guideline)
rcr <- rcr[!rcr$Guideline %in% c('G0011', 'G0014', 'G0025' ) , ]

m0 <- glmer(found_all ~   
              (1 | Guideline), data = rcr, family = binomial, 
            control = glmerControl(optimizer = "bobyqa"),
            nAGQ = 10)
m3 <- glmer(found_all ~ Commision.agency +   
              (1  + Commision.agency| Guideline), data = rcr, family = binomial, 
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun = 1e5)),
            nAGQ = 1)
anova(m0, m3) ## commissioning agency 
print(m3, corr = FALSE)
summary(m3)
se3 <- sqrt(diag(vcov(m3)))
print(se3)
(tab3 <- cbind(Est = fixef(m3), LL = fixef(m3) - 1.96 * se3, UL = fixef(m3) + 1.96 *
                se3))
tab3
exp(tab3)

m4 <- glmer(found_all ~ Topic +   
              (1 + Topic | Guideline), data = rcr, 
            family = binomial, 
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun = 1e5)),
            nAGQ = 1)
anova(m0, m4) # topic of guideline
print(m4, corr = FALSE)
summary(m4)

se4 <- sqrt(diag(vcov(m4)))
print(se4)
(tab4 <- cbind(Est = fixef(m4), LL = fixef(m4) - 1.96 * se4, UL = fixef(m4) + 1.96 *
                se4))
exp(tab4)

rcr$Scaled_year <-  rcr$Year - 2001
m5 <- glmer(found_all ~ Scaled_year +   
            (1 + Scaled_year | Guideline), data = rcr, 
            family = binomial, 
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun = 1e5)),
            nAGQ = 1)
summary(m5)
anova(m0, m5) # publication year
colnames(rcr)

addmargins(table(rcr$found_all, rcr$Other.high.level.evidence, useNA = 'ifany'))
m6 <- glmer(found_all ~ Other.high.level.evidence +   
              (1 + Other.high.level.evidence | Guideline), 
              data = rcr, family = binomial, 
              control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun = 1e5)),
              nAGQ = 1)
summary(m6)
anova(m0, m6) # other high level evidence
colnames(rcr)


## table S5

table(rcr$Topic)
unique(rcr$Guideline[which(rcr$Topic == 'Asthma')])
unique(rcr$Guideline[which(rcr$Topic == 'Cystic Fibrosis')])
unique(rcr$Guideline[which(rcr$Topic == 'Respiratory infections')])
unique(rcr$Guideline[which(rcr$Topic == 'Ventilation in peri-anaesthetic/critical care')])

table(rcr$Commision.agency)
unique(rcr$Guideline[which(rcr$Commision.agency == 'BTS')])
unique(rcr$Guideline[which(rcr$Commision.agency == 'BTS/BIS/HPA/HD')])
unique(rcr$Guideline[which(rcr$Commision.agency == 'BTS/SIGN')])
unique(rcr$Guideline[which(rcr$Commision.agency == 'CF Trust')])
unique(rcr$Guideline[which(rcr$Commision.agency == 'NICE')])
unique(rcr$Guideline[which(rcr$Commision.agency == 'RCPCH')])
unique(rcr$Guideline[which(rcr$Commision.agency == 'SIGN')])

