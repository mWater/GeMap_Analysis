setwd("~/Dropbox/mWater/clients and projects/World Bank/RFP 1135112 Nigeria/Data")
gemap <- read.csv("Gemap responses final - June 29.csv", head=T)

library(WriteXLS)

# Remove refused responses
gemap <- subset(gemap, Does.the.person.agree.to.the.interview. == "Yes")


### CALCULATE PPI ###

# Rename PPI variables
names(gemap)[20:29] <- c("ppi_Q_01","ppi_Q_02","ppi_Q_03","ppi_Q_04","ppi_Q_05","ppi_Q_06","ppi_Q_07","ppi_Q_08","ppi_Q_09","ppi_Q_10")

# Initialize the PPI score vectors

gemap$ppi01 <- 0
gemap$ppi02 <- 0
gemap$ppi03 <- 0
gemap$ppi04 <- 0
gemap$ppi05 <- 0
gemap$ppi06 <- 0
gemap$ppi07 <- 0
gemap$ppi08 <- 0
gemap$ppi09 <- 0
gemap$ppi10 <- 0

# Add the scores for each response

# PPI Q 1 How many members does HH have
gemap$ppi01[gemap$ppi_Q_01 == "Eight or more"] <- 0
gemap$ppi01[gemap$ppi_Q_01 == "Six or Seven"] <- 6
gemap$ppi01[gemap$ppi_Q_01 == "Five"] <- 11
gemap$ppi01[gemap$ppi_Q_01 == "Four"] <- 14
gemap$ppi01[gemap$ppi_Q_01 == "Three"] <- 19
gemap$ppi01[gemap$ppi_Q_01 == "Two"] <- 30
gemap$ppi01[gemap$ppi_Q_01 == "One"] <- 38
# Note: we are making the conservative assumption that no answer means the highest score
gemap$ppi01[gemap$ppi_Q_01 == "Prefers not to answer"] <- 0
gemap$ppi01[gemap$ppi_Q_01 == ""] <- 0

# PPI Q 2 Are all HH members aged 6 - 18 attending school
gemap$ppi02[gemap$ppi_Q_02 == "No"] <- 0
gemap$ppi02[gemap$ppi_Q_02 == "No members aged 6 to 18"] <- 7
gemap$ppi02[gemap$ppi_Q_02 == "Yes"] <- 9
gemap$ppi02[gemap$ppi_Q_02 == ""] <- 0
gemap$ppi02[gemap$ppi_Q_02 == "Prefers not to answer"] <- 0

# PPI Q 3 What is the main flooring material of the house?
gemap$ppi03[gemap$ppi_Q_03 == "Unimproved"] <- 0
gemap$ppi03[gemap$ppi_Q_03 == "Improved"] <- 4
gemap$ppi03[gemap$ppi_Q_03 == ""] <- 0

# PPI Q 4 What is the main roofing material of the house?
gemap$ppi04[gemap$ppi_Q_04 == "Mud/mud bricks"] <- 0
gemap$ppi04[gemap$ppi_Q_04 == "Thatch (grass or straw)"] <- 3
gemap$ppi04[gemap$ppi_Q_04 == "Asbestos, wood/bamboo, corrugated iron sheets, cement/concrete, roofing tiles, or other"] <- 6
gemap$ppi04[gemap$ppi_Q_04 == ""] <- 0

# PPI Q 5 What is the main source of drinking water for the household?
gemap$ppi05[gemap$ppi_Q_05 == "Unprotected well/rain water, or untreated pipe-borne"] <- 0
gemap$ppi05[gemap$ppi_Q_05 == "Vendor, truck, protected well, river, lake, or pond"] <- 4
gemap$ppi05[gemap$ppi_Q_05 == "Treated pipe-borne water, borehold/hand pump, or other"] <- 6
gemap$ppi05[gemap$ppi_Q_05 == ""] <- 0

# PPI Q 6 What type of toilet is used by the household?
gemap$ppi06[gemap$ppi_Q_06 == "Pail/bucket, covered or uncovered pit latrine, ventilated improved pit latrine, other, or none"] <- 0
gemap$ppi06[gemap$ppi_Q_06 == "Toilet on water, or flush to sewer or septic tank"] <- 5
gemap$ppi06[gemap$ppi_Q_06 == ""] <- 0

# PPI Q 7 Does any member of the household own a television?
gemap$ppi07[gemap$ppi_Q_07 == "No"] <- 0
gemap$ppi07[gemap$ppi_Q_07 == "Yes"] <- 15
gemap$ppi07[gemap$ppi_Q_07 == ""] <- 0

# PPI Q 8 Does any member of the household own a stove?
gemap$ppi08[gemap$ppi_Q_08 == "No"] <- 0
gemap$ppi08[gemap$ppi_Q_08 == "Yes"] <- 7
gemap$ppi08[gemap$ppi_Q_08 == ""] <- 0

# PPI Q 9 Does any member of the household own a mattress/bed?
gemap$ppi09[gemap$ppi_Q_09 == "No"] <- 0
gemap$ppi09[gemap$ppi_Q_09 == "Yes"] <- 5
gemap$ppi09[gemap$ppi_Q_09 == ""] <- 0

# PPI Q10 Does any member of the household own a radio?
gemap$ppi10[gemap$ppi_Q_10 == "No"] <- 0
gemap$ppi10[gemap$ppi_Q_10 == "Yes"] <- 5
gemap$ppi10[gemap$ppi_Q_10 == ""] <- 0

# Calculate the PPI scores
PPIsubscores <- data.frame(gemap$ppi01,gemap$ppi02,gemap$ppi03,gemap$ppi04,gemap$ppi05,gemap$ppi06,gemap$ppi07,gemap$ppi08,gemap$ppi09,gemap$ppi10)
gemap$ppi_score <- rowSums(PPIsubscores)

# Calculate the poverty likelihoods (Natl Poverty Line)

gemap$ppi_pov_lik[(gemap$ppi_score >=  0) & (gemap$ppi_score <= 4)] <- 87.1
gemap$ppi_pov_lik[(gemap$ppi_score >=  5) & (gemap$ppi_score <= 9)] <- 93.2
gemap$ppi_pov_lik[(gemap$ppi_score >= 10) & (gemap$ppi_score <=14)] <- 84.3
gemap$ppi_pov_lik[(gemap$ppi_score >= 15) & (gemap$ppi_score <=19)] <- 81.4
gemap$ppi_pov_lik[(gemap$ppi_score >= 20) & (gemap$ppi_score <=24)] <- 75.5
gemap$ppi_pov_lik[(gemap$ppi_score >= 25) & (gemap$ppi_score <=29)] <- 68.2
gemap$ppi_pov_lik[(gemap$ppi_score >= 30) & (gemap$ppi_score <=34)] <- 65.5
gemap$ppi_pov_lik[(gemap$ppi_score >= 35) & (gemap$ppi_score <=39)] <- 54.0
gemap$ppi_pov_lik[(gemap$ppi_score >= 40) & (gemap$ppi_score <=44)] <- 49.6
gemap$ppi_pov_lik[(gemap$ppi_score >= 45) & (gemap$ppi_score <=49)] <- 39.9
gemap$ppi_pov_lik[(gemap$ppi_score >= 50) & (gemap$ppi_score <=54)] <- 26.1
gemap$ppi_pov_lik[(gemap$ppi_score >= 55) & (gemap$ppi_score <=59)] <- 25.6
gemap$ppi_pov_lik[(gemap$ppi_score >= 60) & (gemap$ppi_score <=64)] <- 20.7
gemap$ppi_pov_lik[(gemap$ppi_score >= 65) & (gemap$ppi_score <=69)] <- 15.8
gemap$ppi_pov_lik[(gemap$ppi_score >= 70) & (gemap$ppi_score <=74)] <- 4.3
gemap$ppi_pov_lik[(gemap$ppi_score >= 75) & (gemap$ppi_score <=79)] <- 8.3
gemap$ppi_pov_lik[(gemap$ppi_score >= 80) & (gemap$ppi_score <=84)] <- 1.0
gemap$ppi_pov_lik[(gemap$ppi_score >= 85) & (gemap$ppi_score <=89)] <- 1.6
gemap$ppi_pov_lik[(gemap$ppi_score >= 90) & (gemap$ppi_score <=94)] <- 3.1
gemap$ppi_pov_lik[(gemap$ppi_score >= 95) & (gemap$ppi_score <=100)] <-0.0


### Survey results analysis

## Convert unit questions to single unit

# Income all converted to per month:
gemap$Income.of.the.household[gemap$Income.of.the.household..Units. == "Naira per day"] <- gemap$Income.of.the.household[gemap$Income.of.the.household..Units. == "Naira per day"] * 30
gemap$Income.of.the.household..Units.[gemap$Income.of.the.household..Units. == "Naira per day"] <- "Naira per month"
gemap$Income.of.the.household[gemap$Income.of.the.household..Units. == "Naira per week"] <- gemap$Income.of.the.household[gemap$Income.of.the.household..Units. == "Naira per week"] * 4
gemap$Income.of.the.household..Units.[gemap$Income.of.the.household..Units. == "Naira per week"] <- "Naira per month"
gemap$Income.of.the.household[gemap$Income.of.the.household..Units. == "Naira per year"] <- gemap$Income.of.the.household[gemap$Income.of.the.household..Units. == "Naira per year"] / 12
gemap$Income.of.the.household..Units.[gemap$Income.of.the.household..Units. == "Naira per year"] <- "Naira per month"

# Spend on water vendor to per day:
gemap$How.much.money.do.you.spend.on.the.water.vendor.[gemap$How.much.money.do.you.spend.on.the.water.vendor...Units. == "Naira per week"] <- gemap$How.much.money.do.you.spend.on.the.water.vendor.[gemap$How.much.money.do.you.spend.on.the.water.vendor...Units. == "Naira per week"] / 7
gemap$How.much.money.do.you.spend.on.the.water.vendor...Units.[gemap$How.much.money.do.you.spend.on.the.water.vendor...Units. == "Naira per week"] <- "Naira per day"
gemap$How.much.money.do.you.spend.on.the.water.vendor.[gemap$How.much.money.do.you.spend.on.the.water.vendor...Units. == "Naira per month"] <- gemap$How.much.money.do.you.spend.on.the.water.vendor.[gemap$How.much.money.do.you.spend.on.the.water.vendor...Units. == "Naira per month"] / 30
gemap$How.much.money.do.you.spend.on.the.water.vendor...Units.[gemap$How.much.money.do.you.spend.on.the.water.vendor...Units. == "Naira per month"] <- "Naira per day"

# Spend on water (sources outside the home) to per day:
gemap$How.much.do.you.pay.for.water.[gemap$How.much.do.you.pay.for.water...Units. == "Naira per week"] <- gemap$How.much.do.you.pay.for.water.[gemap$How.much.do.you.pay.for.water...Units == "Naira per week"] / 7
gemap$How.much.do.you.pay.for.water...Units.[gemap$How.much.do.you.pay.for.water...Units. == "Naira per week"] <- "Naira per day"
gemap$How.much.do.you.pay.for.water.[gemap$How.much.do.you.pay.for.water...Units. == "Naira per month"] <- gemap$How.much.do.you.pay.for.water.[gemap$How.much.do.you.pay.for.water...Units == "Naira per month"] / 30
gemap$How.much.do.you.pay.for.water...Units.[gemap$How.much.do.you.pay.for.water...Units. == "Naira per month"] <- "Naira per day"

# Total time spent collecting water to minutes
gemap$What.is.the.total.time.members.of.your.household.spend.each.day.fetching.water.[gemap$What.is.the.total.time.members.of.your.household.spend.each.day.fetching.water...Units == "Hours"] <- gemap$What.is.the.total.time.members.of.your.household.spend.each.day.fetching.water.[gemap$What.is.the.total.time.members.of.your.household.spend.each.day.fetching.water...Units == "Hours"] * 60
gemap$What.is.the.total.time.members.of.your.household.spend.each.day.fetching.water...Units[gemap$What.is.the.total.time.members.of.your.household.spend.each.day.fetching.water...Units == "Hours"] <- "Minutes"

### Gender disaggregated analysis

names(gemap)[names(gemap)=="Gender.of.the.respondent"] <- "gender"   #Rename gender column for brevity

# Education
write.csv((xtabs(~ Education.level.of.the.respondent + gender, gemap)), "gender-educ_freq.csv")
write.csv(prop.table(xtabs(~ Education.level.of.the.respondent + gender, gemap),2), "gender-educ_prop.csv")

# Occupation
write.csv((xtabs(~ Occupation.of.the.the.respondent + gender, gemap)), "gender-occup_freq.csv")
write.csv(prop.table(xtabs(~ Occupation.of.the.the.respondent + gender, gemap),2), "gender-occup_prop.csv")

# Distance to water - normal
write.csv(xtabs(~ How.far.do.you.need.to.travel.to.collect.water.during.normal.periods. + gender, gemap, subset=gemap$How.far.do.you.need.to.travel.to.collect.water.during.normal.periods. != ""), "gender-distNormal_freq.csv")
write.csv(prop.table(xtabs(~ How.far.do.you.need.to.travel.to.collect.water.during.normal.periods. + gender, gemap, subset=gemap$How.far.do.you.need.to.travel.to.collect.water.during.normal.periods. != ""),2), "gender-distNormal_prop.csv")

# Distance to water - scarcity
write.csv(xtabs(~ How.far.do.you.need.to.travel.to.collect.water.during.periods.of.scarcity. + gender, gemap, subset=How.far.do.you.need.to.travel.to.collect.water.during.periods.of.scarcity. != ""), "gender-distScarce_freq.csv")
write.csv(prop.table(xtabs(~ How.far.do.you.need.to.travel.to.collect.water.during.periods.of.scarcity. + gender, gemap, subset=gemap$How.far.do.you.need.to.travel.to.collect.water.during.periods.of.scarcity. != ""),2), "gender-distScarce_prop.csv")

# Location convenient
names(gemap)[names(gemap)=="Is.the.location.of.the.source.convenient.to.you."] <- "LocationConvenient"
write.csv(xtabs(~ LocationConvenient + gender, gemap, subset=LocationConvenient != ""), "gender-LocConven_freq.csv")
write.csv(prop.table(xtabs(~ LocationConvenient + gender, gemap, subset=gemap$LocationConvenient != ""),2), "gender-LocConven_prop.csv")

# Who fetches water
names(gemap)[names(gemap)=="Who.usually.fetches.water.for.the.household."] <- "WhoFetches"
write.csv(xtabs(~ WhoFetches + gender, gemap, subset=WhoFetches != ""), "gender-WhoFetches_freq.csv")
write.csv(prop.table(xtabs(~ WhoFetches + gender, gemap, subset=gemap$WhoFetches != ""),2), "gender-WhoFetches_prop.csv")

# Made complaint
names(gemap)[names(gemap)=="Did.you.make.a.complaint.when.there.was.a.breakdown."] <- "MadeComplaint"
write.csv(xtabs(~ MadeComplaint + gender, gemap, subset=MadeComplaint != ""), "gender-MadeComplaint_freq.csv")
write.csv(prop.table(xtabs(~ MadeComplaint + gender, gemap, subset=gemap$MadeComplaint != ""),2), "gender-MadeComplaint_prop.csv")

# Treat water
names(gemap)[names(gemap)=="Do.you.treat.your.water.in.any.way.to.make.it.safer.to.drink."] <- "TreatWater"
write.csv(xtabs(~ TreatWater + gender, gemap, subset=TreatWater != ""), "gender-TreatWater_freq.csv")
write.csv(prop.table(xtabs(~ TreatWater + gender, gemap, subset=gemap$TreatWater != ""),2), "gender-TreatWater_prop.csv")


# Satisfied with water services

SatisfiedWater <- NULL
for(i in seq(243,257, by = 2)) {
  newrow <- cbind(xtabs(~ gemap$gender + gemap[,i]),prop.table(xtabs(~ gemap$gender + gemap[,i]),1))
  newrow[1,1] <- names(gemap)[i]
  SatisfiedWater <- rbind(SatisfiedWater, newrow)
  SatifiedWater
}
write.csv(SatisfiedWater,"Satisfaction with Water Services.csv")


# Satisfied with sanitation services

SatisfiedSanitation <- NULL
for(i in seq(288,298, by = 2)) {
  newrow <- cbind(xtabs(~ gemap$gender + gemap[,i]),prop.table(xtabs(~ gemap$gender + gemap[,i]),1))
  newrow[1,1] <- names(gemap)[i]
  SatisfiedSanitation <- rbind(SatisfiedSanitation, newrow)
  SatisfiedSanitation
}
write.csv(SatisfiedSanitation,"Satisfaction with Sanitation Services.csv")


# Toilet facility
names(gemap)[names(gemap)=="What.kind.of.toilet.facility.do.members.of.your.household.usually.use."] <- "Toilet"
freq <- NULL  # This is a new way of doing it I thought of midway thru!
prop <- NULL
freq <- xtabs(~ Toilet + gender, gemap)
prop <- prop.table(xtabs(~ Toilet + gender, gemap),2)
write.csv(cbind(freq,prop), "gender-Toilet.csv")

# All HH members use toilet?
names(gemap)[names(gemap)=="Do.all.of.your.household.members.use.the.toilet.facility.at.home."] <- "AllUseToilet"
freq <- NULL  # This is a new way of doing it I thought of midway thru!
prop <- NULL
freq <- xtabs(~ AllUseToilet + gender, gemap)
prop <- prop.table(xtabs(~ AllUseToilet + gender, gemap),2)
write.csv(cbind(freq,prop), "gender-AllUseToilet.csv")

# Ever use public toilet?
names(gemap)[names(gemap)=="Do.you.ever.use.a.public.latrine."] <- "UsePublicToilet"
freq <- NULL  # This is a new way of doing it I thought of midway thru!
prop <- NULL
freq <- xtabs(~ UsePublicToilet + gender, gemap)
prop <- prop.table(xtabs(~ UsePublicToilet + gender, gemap),2)
write.csv(cbind(freq,prop), "gender-UsePublicToilet.csv")

# Willing to Pay more for sanitation?
names(gemap)[names(gemap)=="Are.you.willing.to.pay.or.pay.more.for.better.sanitation.services."] <- "PayMoreSan"
freq <- NULL  # This is a new way of doing it I thought of midway thru!
prop <- NULL
freq <- xtabs(~ PayMoreSan + gender, gemap)
prop <- prop.table(xtabs(~ PayMoreSan + gender, gemap),2)
write.csv(cbind(freq,prop), "gender-PayMoreSan.csv")


# Chance to participate?
names(gemap)[names(gemap)=="Have.you.had.a.chance.in.the.past.3.years.to.participate.and.contribute.your.suggestions.on.water.policies..investments.or.services."] <- "Participate"
freq <- NULL  # This is a new way of doing it I thought of midway thru!
prop <- NULL
freq <- xtabs(~ Participate + gender, gemap)
prop <- prop.table(xtabs(~ Participate + gender, gemap),2)
write.csv(cbind(freq,prop), "gender-ChanceToParticipate.csv")
