setwd(here::here("NESHEIM"))

survey = data.frame(read.csv('data/fullsurvey.csv',header=TRUE))
exp1Raw = data.frame(read.csv('data/experiment1.csv',header=TRUE))
exp2Raw = data.frame(read.csv('data/experiment2.csv',header=TRUE))
exp3Raw = data.frame(read.csv('data/experiment3.csv',header=TRUE))
exp4Raw = data.frame(read.csv('data/experiment4.csv',header=TRUE))

#what to exclude stated disorders and incomplete experiments
excludeRaw = survey$P01Q10 != 0 | survey$ExitCode != 0  


### removing redundant participants so i can calculate aq stats ###
# commented out when running exp 
survey <- survey[!survey$ExitCode == -2, ]
survey <- survey[!survey$P01Q10 == 2, ]
survey <- survey[!is.na(survey$ExitCode), ]

#calculate 21 item PDI
PDIraw = survey[,68:151]
a = 1:length(PDIraw)
b = a[seq(1, length(a), 4)]
PDIraw = PDIraw[,b]
PDIraw = rowSums(PDIraw)

AQraw = survey[,18:67]

#--- calculate AQ from survey -------
AQforIndex = c(2,4,5,6,7,9,12,13,16,18,19,20,21,22,23,26,
               33,35,39,41,42,43,45,46)
AQbackIndex = c(1,3,8,10,11,14,15,17,24,25,27,28,29,30,31,32, 
                34,36,37,38,40,44,47,48,49,50)

AQfor = AQraw[,AQforIndex] #isolate forward scored social items
AQback = AQraw[,AQbackIndex] #isolate backward scored social items

AQtotal =
  rowSums(AQfor=="slightly agree"|AQfor=="definitely agree") +  #treats agree responses as true and sums
  rowSums(AQback=="slightly disagree"|AQback=="definitely disagree") #treats disagree responses as true and sums

##----- calculate AQ subscales ------
socialInd = c(1,11,13,15,22,36,44,45,47,48)
switchInd = c(2,4,10,16,25,32,34,37,43,46)
detailInd = c(5,6,9,12,19,23,28,29,30,49)
commInd = c(7,17,18,26,27,31,33,35,38,39)
imagInd = c(3,8,14,20,21,24,40,41,42,50)

socialFor = c(13,22,45)
switchFor = c(2,4,16,43,46)
detailFor = c(5,6,9,12,19,23)
commFor = c(7,18,26,33,35,39)
imagFor = c(20,21,41,42)

socialBack = c(1,11,15,36,44,47,48)
switchBack = c(10,25,32,34,37)
detailBack = c(28,29,30,49)
commBack = c(17,27,31,38)
imagBack = c(3,8,14,24,40,50)

AQsocFor = AQraw[,socialFor] #isolate forward scored social items
AQsocBack = AQraw[,socialBack] #isolate backward scored social items
AQswitchFor = AQraw[,switchFor] #isolate forward scored social items
AQswitchBack = AQraw[,switchBack] #isolate backward scored social items
AQdetailFor = AQraw[,detailFor] #isolate forward scored social items
AQdetailBack = AQraw[,detailBack] #isolate backward scored social items
AQcommFor = AQraw[,commFor] #isolate forward scored social items
AQcommBack = AQraw[,commBack] #isolate backward scored social items
AQimagFor = AQraw[,imagFor] #isolate forward scored social items
AQimagBack = AQraw[,imagBack] #isolate backward scored social items

AQsocial =
  rowSums(AQsocFor=="slightly agree"|AQsocFor=="definitely agree") +  #treats agree responses as true and sums
  rowSums(AQsocBack=="slightly disagree"|AQsocBack=="definitely disagree")
AQswitch =
  rowSums(AQswitchFor=="slightly agree"|AQswitchFor=="definitely agree") +  #treats agree responses as true and sums
  rowSums(AQswitchBack=="slightly disagree"|AQswitchBack=="definitely disagree")
AQdetail =
  rowSums(AQdetailFor=="slightly agree"|AQdetailFor=="definitely agree") +  #treats agree responses as true and sums
  rowSums(AQdetailBack=="slightly disagree"|AQdetailBack=="definitely disagree")
AQcomm =
  rowSums(AQcommFor=="slightly agree"|AQcommFor=="definitely agree") +  #treats agree responses as true and sums
  rowSums(AQcommBack=="slightly disagree"|AQcommBack=="definitely disagree")
AQimag =
  rowSums(AQimagFor=="slightly agree"|AQimagFor=="definitely agree") +  #treats agree responses as true and sums
  rowSums(AQimagBack=="slightly disagree"|AQimagBack=="definitely disagree")


###############################################################################
######## set up experiment 1 data frame #######################################
exclude = excludeRaw[survey$Experiment==1]

PDI = PDIraw[survey$Experiment==1]
AQ = AQtotal[survey$Experiment==1]
social = AQsocial[survey$Experiment==1]
switching = AQswitch[survey$Experiment==1]
detail = AQdetail[survey$Experiment==1]
comm = AQcomm[survey$Experiment==1]
imag = AQimag[survey$Experiment==1]
Age = survey$P01Q01[survey$Experiment==1]
Gender = survey$P01Q02[survey$Experiment==1]
sub = survey$ParticipantId[survey$Experiment==1]


###############################################################################
######## set up experiment 2 data frame #######################################
exclude = excludeRaw[survey$Experiment==2]

PDI = PDIraw[survey$Experiment==2]
AQexp2 = AQtotal[survey$Experiment==2]
social = AQsocial[survey$Experiment==2]
switching = AQswitch[survey$Experiment==2]
detail = AQdetail[survey$Experiment==2]
comm = AQcomm[survey$Experiment==2]
imag = AQimag[survey$Experiment==2]
Age = survey$P01Q01[survey$Experiment==2]
Gender = survey$P01Q02[survey$Experiment==2]
sub = survey$ParticipantId[survey$Experiment==2]

###############################################################################
######## set up experiment 3 data frame #######################################
exclude = excludeRaw[survey$Experiment==3]

PDI = PDIraw[survey$Experiment==3]
AQ = AQtotal[survey$Experiment==3]
social = AQsocial[survey$Experiment==3]
switching = AQswitch[survey$Experiment==3]
detail = AQdetail[survey$Experiment==3]
comm = AQcomm[survey$Experiment==3]
imag = AQimag[survey$Experiment==3]
Age = survey$P01Q01[survey$Experiment==3]
Gender = survey$P01Q02[survey$Experiment==3]
sub = survey$ParticipantId[survey$Experiment==3]

###############################################################################
######## set up experiment 4 data frame #######################################
exclude = excludeRaw[survey$Experiment==4]

PDI = PDIraw[survey$Experiment==4]
AQ4 = AQtotal[survey$Experiment==4]
social = AQsocial[survey$Experiment==4]
switching = AQswitch[survey$Experiment==4]
detail = AQdetail[survey$Experiment==4]
comm = AQcomm[survey$Experiment==4]
imag = AQimag[survey$Experiment==4]
Age = survey$P01Q01[survey$Experiment==4]
Gender = survey$P01Q02[survey$Experiment==4]
sub = survey$ParticipantId[survey$Experiment==4]

################ descriptive analysis ################
mean(AQ4)
#making df with only relevant exp with all survey info
survey_exp4_ <- survey[survey$Experiment %in% 4, ]
survey_exp2_ <- survey[survey$Experiment %in% 2, ]
survey_exp3_ <- survey[survey$Experiment %in% 3, ]

#removing individuals with commobidities, incomplete surveys and rows with na's
survey_exp4_ <- survey_exp4_[!survey_exp4_$P01Q10 == 1, ]
survey_exp4_ <- survey_exp4_[!survey_exp4_$ExitCode == -1, ]
survey_exp4_ <- survey_exp4_[!is.na(survey_exp4_$ExitCode), ]

survey_exp3_ <- survey_exp3_[!survey_exp3_$P01Q10 == 1, ]
survey_exp3_ <- survey_exp3_[!survey_exp3_$ExitCode == -1, ]
survey_exp3_ <- survey_exp3_[!is.na(survey_exp3_$ExitCode), ]

survey_exp2_ <- survey_exp2_[!survey_exp2_$P01Q10 == 1, ]
survey_exp2_ <- survey_exp2_[!survey_exp2_$ExitCode == -1, ]
survey_exp2_ <- survey_exp2_[!is.na(survey_exp2_$ExitCode), ]

#age descriptive stats 
summary(survey_exp4_$P01Q01)
sd(survey_exp4_$P01Q01)

#gender descriptive stats
summary(survey_exp2_$P01Q02 == "Female")
summary(survey_exp3_$P01Q02 == "Female")
summary(survey_exp4_$P01Q02 == "Female")

#residence descriptive stats
summary(survey_exp2_$P01Q06 == "USA")
summary(survey_exp3_$P01Q06 == "USA")
summary(survey_exp4_$P01Q06 == "USA")

#degree descriptive stats
summary(survey_exp2_$P01Q05)

##counting the different degrees 
education_exp4 <- survey_exp4_ %>%
  count(P01Q05)
