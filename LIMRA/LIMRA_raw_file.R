  
######## Set up ########################################
limra_2.3 <- read.csv("C:/Users/ashis/Desktop/LIMRA/glwb2012_2013sub60p_2.3M.csv")
getwd()
setwd("C:\\Users\\ashis\\Desktop\\LIMRA")
library(mice)



# PART- I [Creating Single Contract Subset]
############ Creating subset of those contracts in LIMRA_2.3, that appear multiple times ###########################
class(limra_2.3$CONTRACT)
length(levels(limra_2.3$CONTRACT))
contract_count = tapply(limra_2.3$TARGET, limra_2.3$CONTRACT, length)
contract_count = as.data.frame(contract_count)
contract_count$contract = rownames(contract_count)
rownames(contract_count) = NULL
colnames(contract_count) = c("contactcount","contract")
head(contract_count)
nrow(contract_count)
contract_count$contactcount = as.numeric(contract_count$contactcount)
table(as.factor(contract_count$contactcount))
multiple_appearing_contracts = contract_count[contract_count$contactcount >1,]
nrow(multiple_appearing_contracts)
###### Multiple Appearing Contracts #####################################
MAC=limra_2.3[limra_2.3$CONTRACT %in% multiple_appearing_contracts$contract,]
nrow(MAC)
# No. of repeating contracts each year (Same - obviously)
nrow(MAC[MAC$OBSYR == 2012,])
nrow(MAC[MAC$OBSYR == 2013,])


################### Calculating distribution of repeating contracts each year with target ############################


length(MAC[MAC$OBSYR == 2012 & MAC$TARGET == 1,"CONTRACT"]) #891
length(limra_2.3[limra_2.3$CONTRACT %in% MAC[MAC$OBSYR == 2012 & MAC$TARGET == 1,"CONTRACT"] & limra_2.3$OBSYR == 2013 & limra_2.3$TARGET == 1,"CONTRACT"])
length(limra_2.3[limra_2.3$CONTRACT %in% MAC[MAC$OBSYR == 2012 & MAC$TARGET == 1,"CONTRACT"] & limra_2.3$OBSYR == 2013 & limra_2.3$TARGET == 0,"CONTRACT"])


length(MAC[MAC$OBSYR == 2012 & MAC$TARGET == 0,"CONTRACT"]) #675902
length(limra_2.3[limra_2.3$CONTRACT %in% MAC[MAC$OBSYR == 2012 & MAC$TARGET == 0,"CONTRACT"] & limra_2.3$OBSYR == 2013 & limra_2.3$TARGET == 0,"CONTRACT"])
length(limra_2.3[limra_2.3$CONTRACT %in% MAC[MAC$OBSYR == 2012 & MAC$TARGET == 0,"CONTRACT"] & limra_2.3$OBSYR == 2013 & limra_2.3$TARGET == 1,"CONTRACT"])


nrow(MAC[MAC$OBSYR == 2013 & MAC$TARGET == 1,]) #23257
nrow(MAC[MAC$OBSYR == 2013 & MAC$TARGET == 0,]) #653536


table(limra_2.3[limra_2.3$CONTRACT %in% MAC[MAC$OBSYR == 2012 & MAC$TARGET == 0,"CONTRACT"] & limra_2.3$OBSYR ==2013 & limra_2.3$TARGET == 0,"LIMRA"])
table(limra_2.3[limra_2.3$CONTRACT %in% MAC[MAC$OBSYR == 2012 & MAC$TARGET == 0,"CONTRACT"] & limra_2.3$OBSYR ==2013 & limra_2.3$TARGET == 1,"LIMRA"])




################ Calculating contracts each year (non-repeating) and repeating ##########################################################

contracts_2013_only = limra_2.3[limra_2.3$OBSYR ==2013 & !(limra_2.3$CONTRACT %in% MAC$CONTRACT),]
nrow(contracts_2013_only)
table(contracts_2013_only$TARGET)

contracts_2012_only = limra_2.3[limra_2.3$OBSYR ==2012 & !(limra_2.3$CONTRACT %in% MAC$CONTRACT),]
nrow(contracts_2012_only)
table(contracts_2012_only$TARGET)

table(MAC$TARGET)


####################################### Subsetting for Repeating Contracts................................

# For 2012 and Target == 1, 
MAC_2012_T1 = MAC[MAC$OBSYR == 2012 & MAC$TARGET == 1,]
nrow(MAC_2012_T1)

# For MAC and Target == 0 keeping only for 2013.
MAC_2013_T0 = limra_2.3[limra_2.3$CONTRACT %in% MAC[MAC$OBSYR == 2012 & MAC$TARGET == 0,"CONTRACT"] & limra_2.3$OBSYR == 2013 & limra_2.3$TARGET == 0,]
nrow(MAC_2013_T0)

MAC_2013_T1 = limra_2.3[limra_2.3$CONTRACT %in% MAC[MAC$OBSYR == 2012 & MAC$TARGET == 0,"CONTRACT"] & limra_2.3$OBSYR == 2013 & limra_2.3$TARGET == 1,]
nrow(MAC_2013_T1)



################################# Dataset with contracts appearing only once

LIMRA_SC = rbind(contracts_2013_only,contracts_2012_only,MAC_2012_T1,MAC_2013_T0,MAC_2013_T1)
nrow(LIMRA_SC)
write.csv(LIMRA_SC,file = "LIMRA_SC_woDC.csv",row.names = F)
setwd("C:\\Users\\ashis\\Desktop\\LIMRA")












###########################################################################################################
# PART - II [Data Cleanisng for LIMRA_SC]
###########################################################################################################
LIMRA_SC = read.csv("LIMRA_SC_woDC.csv")

View(LIMRA_SC)

# Basic check
str(LIMRA_SC)
ncol(LIMRA_SC)
colnames(LIMRA_SC)
nrow(LIMRA_SC[is.na(LIMRA_SC$LIMRA) == F,])
nrow(LIMRA_SC[LIMRA_SC$WBL == 1,])

# Treating LIMRA
LIMRA_SC$LIMRA = as.factor(LIMRA_SC$LIMRA)
class(LIMRA_SC$LIMRA)

# Treating M_E_1 NA's
nrow(LIMRA_SC[is.na(LIMRA_SC$M_E_1),]) # 1391 Missing values but there are no zeros.
nrow(LIMRA_SC[LIMRA_SC$M_E_1 == 0 & !is.na(LIMRA_SC$M_E_1),])
LIMRA_SC[is.na(LIMRA_SC$M_E_1) == T & LIMRA_SC$LIMRA == 0002 & LIMRA_SC$NUM_SUBACCTS == 54,'M_E_1'] = 130
LIMRA_SC[is.na(LIMRA_SC$M_E_1) == T & LIMRA_SC$LIMRA == 0027,'M_E_1'] = 140
LIMRA_SC[is.na(LIMRA_SC$M_E_1) == T & LIMRA_SC$LIMRA == 0080,'M_E_1'] = 75


# Treating Fixed Account
nrow(LIMRA_SC[is.na(LIMRA_SC$FIXED_ACCT),]) # 160764 Missing values
LIMRA_SC$FIXED_ACCT = as.factor(LIMRA_SC$FIXED_ACCT) 
table(LIMRA_SC$FIXED_ACCT,useNA = "ifany")
class(LIMRA_SC$FIXED_ACCT)
# Insights - Comapny 0015 has not reported whether policies have fixed account or not. All NA's correspond to company 0015.
LIMRA_SC[LIMRA_SC$AVFIX_BOY == 0 & LIMRA_SC$AVFIX_ANN==0 & LIMRA_SC$AVFIX_EOY==0 & is.na(LIMRA_SC$FIXED_ACCT),'FIXED_ACCT'] = 2
LIMRA_SC[is.na(LIMRA_SC$FIXED_ACCT),'FIXED_ACCT'] = 1

# temp_LIMRA_SC = mice(LIMRA_SC,method = "pmm", seed = 500)

# Treating NUM_SUBACCTS
nrow(LIMRA_SC[is.na(LIMRA_SC$NUM_SUBACCTS),]) # 130177 Missing values but no zeros....
nrow(LIMRA_SC[LIMRA_SC$NUM_SUBACCTS == 0 & !is.na(LIMRA_SC$NUM_SUBACCTS),])
# limra_2.3[is.na(limra_2.3$NUM_SUBACCTS) == T,'NUM_SUBACCTS'] = median(limra_2.3$NUM_SUBACCTS, na.rm = T) # Because the distribution is skewed
# Insights - Comapny 0080 has not reported no. of subaccounts data. All NA's correspond to company 0080.
LIMRA_SC$NUM_SUBACCTS.NA = ifelse(is.na(LIMRA_SC$NUM_SUBACCTS),1,0)
LIMRA_SC[is.na(LIMRA_SC$NUM_SUBACCTS),"NUM_SUBACCTS"] = median(LIMRA_SC$NUM_SUBACCTS, na.rm = T)
LIMRA_SC$NUM_SUBACCTS.NA = as.factor(LIMRA_SC$NUM_SUBACCTS.NA)

# Treating SURBASIS
nrow(LIMRA_SC[is.na(LIMRA_SC$SURBASIS),])
LIMRA_SC$SURBASIS = as.factor(LIMRA_SC$SURBASIS)
levels(LIMRA_SC$SURBASIS)

# Treating SURCHG_YR1 - SURCHG_YR10
nrow(LIMRA_SC[is.na(LIMRA_SC$SURCHG_YR1) == T,]) # No NA
nrow(LIMRA_SC[is.na(LIMRA_SC$SURCHG_YR2) == T,]) # No NA
nrow(LIMRA_SC[is.na(LIMRA_SC$SURCHG_YR3) == T,]) # No NA
nrow(LIMRA_SC[is.na(LIMRA_SC$SURCHG_YR4) == T,]) # No NA
nrow(LIMRA_SC[is.na(LIMRA_SC$SURCHG_YR5) == T,]) # No NA
nrow(LIMRA_SC[is.na(LIMRA_SC$SURCHG_YR6) == T,]) # No NA
nrow(LIMRA_SC[is.na(LIMRA_SC$SURCHG_YR7) == T,]) # No NA
nrow(LIMRA_SC[is.na(LIMRA_SC$SURCHG_YR8) == T,]) # No NA
nrow(LIMRA_SC[is.na(LIMRA_SC$SURCHG_YR9) == T,]) # No NA
nrow(LIMRA_SC[is.na(LIMRA_SC$SURCHG_YR10) == T,]) # No NA

surcharge_matrix = cbind(LIMRA_SC$SURCHG_YR1,LIMRA_SC$SURCHG_YR2,LIMRA_SC$SURCHG_YR3,LIMRA_SC$SURCHG_YR4,
                         LIMRA_SC$SURCHG_YR5,LIMRA_SC$SURCHG_YR6,LIMRA_SC$SURCHG_YR7,LIMRA_SC$SURCHG_YR8,
                         LIMRA_SC$SURCHG_YR9,LIMRA_SC$SURCHG_YR10)

year_0 = apply(surcharge_matrix,1,function(k) which.min(k))
LIMRA_SC$SURCHG_YR0 = year_0
# Reduced 10 columns into 2. SURCHG_YR1 and Year in which return is zero.


# Treating STATES
nrow(LIMRA_SC[is.na(LIMRA_SC$STATE),])# States NA (just 9) left as it is.
class(LIMRA_SC$STATE)


#Treating Contracts
nrow(LIMRA_SC[is.na(LIMRA_SC$CONTRACT),]) # No NA
class(LIMRA_SC$CONTRACT)

# Treating MOISS
nrow(LIMRA_SC[is.na(LIMRA_SC$MOISS),]) # No NA
class(LIMRA_SC$MOISS)
LIMRA_SC$MOISS = as.factor(LIMRA_SC$MOISS)
levels(LIMRA_SC$MOISS)

# Treating DAYISS
nrow(LIMRA_SC[is.na(LIMRA_SC$DAYISS),]) # No NA
LIMRA_SC$DAYISS = as.factor(LIMRA_SC$DAYISS)
levels(LIMRA_SC$DAYISS)

# Treating YRISS
nrow(LIMRA_SC[is.na(LIMRA_SC$YRISS) == T,]) # No NA

LIMRA_SC$MOISS = ifelse(as.integer(as.character(LIMRA_SC$MOISS)) < 10,paste("0",LIMRA_SC$MOISS,sep = ""),LIMRA_SC$MOISS)
LIMRA_SC$DAYISS = ifelse(as.integer(as.character(LIMRA_SC$DAYISS)) < 10,paste("0",LIMRA_SC$DAYISS,sep = ""),LIMRA_SC$DAYISS)
date_matrix = cbind(LIMRA_SC$YRISS,LIMRA_SC$MOISS,LIMRA_SC$DAYISS)
head(date_matrix)
date_combined = apply(date_matrix, 1, function(x) paste(x[1],x[2],x[3],sep = ""))
LIMRA_SC$Date_ISSUE = date_combined
LIMRA_SC$Date_ISSUE = as.numeric(LIMRA_SC$Date_ISSUE)# Can remove year now
class(LIMRA_SC$Date_ISSUE)

# Creating another column ANNUITY_AGE
LIMRA_SC$ANNUITY_AGE = LIMRA_SC$OBSYR - LIMRA_SC$YRISS
head(LIMRA_SC$ANNUITY_AGE)


# Treating MKTYPE
nrow(LIMRA_SC[is.na(LIMRA_SC$MKTYPE),]) # Left as it is (51 NA)
LIMRA_SC$MKTYPE = as.factor(LIMRA_SC$MKTYPE)
class(LIMRA_SC$MKTYPE)
levels(LIMRA_SC$MKTYPE)
prop.table(table(LIMRA_SC$MKTYPE))
LIMRA_SC[is.na(LIMRA_SC$MKTYPE),"MKTYPE"] = "1"


# Impute all FIx AVBOY,AVEOY,AVANN as 0 if in common they are either zero or null. Also impute fixed acct value to 2 for these contracts.
nrow(LIMRA_SC[is.na(LIMRA_SC$AVBOY),]) #  Left as it is (just 1)
LIMRA_SC[is.na(LIMRA_SC$AVBOY),"AVBOY"] = median(LIMRA_SC$AVBOY,na.rm = T)
nrow(LIMRA_SC[is.na(LIMRA_SC$AVFIX_BOY) == T,]) # 422133 NA's. Left as it is. *****
nrow(LIMRA_SC[LIMRA_SC$AVFIX_BOY == 0,]) # 1573356 0's.
1573356-422133
# Nulls are also getting counted as zero. However, 

nrow(LIMRA_SC[is.na(LIMRA_SC$AVANN),]) # 92603
nrow(LIMRA_SC[LIMRA_SC$AVANN == 0,]) 
nrow(LIMRA_SC[LIMRA_SC$AVANN == 0 & !is.na(LIMRA_SC$AVANN),]) 
29465+92603
nrow(LIMRA_SC[is.na(LIMRA_SC$AVFIX_ANN),]) 
nrow(LIMRA_SC[LIMRA_SC$AVFIX_ANN == 0,]) 
nrow(LIMRA_SC[is.na(LIMRA_SC$AVEOY),]) 
nrow(LIMRA_SC[LIMRA_SC$AVEOY == 0,]) - nrow(LIMRA_SC[is.na(LIMRA_SC$AVEOY),]) 

nrow(LIMRA_SC[is.na(LIMRA_SC$AVFIX_EOY),]) 
nrow(LIMRA_SC[LIMRA_SC$AVFIX_EOY == 0,]) - nrow(LIMRA_SC[is.na(LIMRA_SC$AVFIX_EOY),]) 
 

nrow(limra_2.3[is.na(LIMRA_SC$AVFIX_BOY) & is.na(LIMRA_SC$AVFIX_ANN) & is.na(LIMRA_SC$AVFIX_EOY),])
nrow(LIMRA_SC[LIMRA_SC$AVFIX_BOY == 0 & LIMRA_SC$AVFIX_ANN==0 & LIMRA_SC$AVFIX_EOY==0,])
nrow(LIMRA_SC[LIMRA_SC$AVFIX_BOY == 0 & LIMRA_SC$AVFIX_ANN ==0 & LIMRA_SC$AVFIX_EOY ==0 & is.na(LIMRA_SC$FIXED_ACCT),])



# Treating STATCODES
nrow(LIMRA_SC[is.na(LIMRA_SC$STATCODE) == T,]) # No Nulls
LIMRA_SC$STATCODE = as.factor(LIMRA_SC$STATCODE)
class(LIMRA_SC$STATCODE)
levels(LIMRA_SC$STATCODE)
# Insight - Target is derived from this variable, hence not used in modeling

# Treating MOSTAT
nrow(LIMRA_SC[is.na(LIMRA_SC$MOSTAT) == T,]) 
LIMRA_SC$MOSTAT = as.factor(LIMRA_SC$MOSTAT)
class(LIMRA_SC$MOSTAT)
levels(LIMRA_SC$MOSTAT)
# Insight - Data only available for this field when STATCODE != 0

# Treating DAYSTAT
nrow(LIMRA_SC[is.na(LIMRA_SC$DAYSTAT) == T,]) 
LIMRA_SC$DAYSTAT = as.factor(LIMRA_SC$DAYSTAT)
class(LIMRA_SC$DAYSTAT)
levels(LIMRA_SC$DAYSTAT)
nrow(LIMRA_SC[is.na(LIMRA_SC$DAYSTAT) == T & is.na(LIMRA_SC$MOSTAT) == T,])
# Insight - Data only avilable when STATCODE != 0

# Treating PWITHD
nrow(LIMRA_SC[is.na(LIMRA_SC$PWITHD),]) # No nulls.
nrow(LIMRA_SC[LIMRA_SC$PWITHD == 0,])

# Treating SWITHD
nrow(LIMRA_SC[is.na(LIMRA_SC$SWITHD),]) # 11259848 Nulls (Left as it is)
nrow(LIMRA_SC[!is.na(LIMRA_SC$SWITHD) & LIMRA_SC$SWITHD == 0,]) # 92864 Zeros.
nrow(LIMRA_SC[LIMRA_SC$SWITHD != 0 & !is.na(LIMRA_SC$SWITHD),])
LIMRA_SC[is.na(LIMRA_SC$SWITHD),'SWITHD'] = 0

# Treating Birth
nrow(LIMRA_SC[is.na(LIMRA_SC$BIRTH) == T,]) # No nulls.
class(LIMRA_SC$BIRTH)

# Treating SEX
nrow(LIMRA_SC[is.na(LIMRA_SC$SEX) == T,]) 
LIMRA_SC$SEX = as.factor(LIMRA_SC$SEX)
class(LIMRA_SC$SEX)
levels(LIMRA_SC$SEX)

# Treating S_BIRTH
nrow(LIMRA_SC[is.na(LIMRA_SC$S_BIRTH) == T,]) #1537671
class(LIMRA_SC$S_BIRTH)
# LIMRA_SC[LIMRA_SC$CONTRACT == '00000000000035109788','S_BIRTH'] = NA

# Treating S_SEX
nrow(LIMRA_SC[is.na(LIMRA_SC$S_SEX) == T,])  # 1557454
LIMRA_SC$S_SEX = as.factor(LIMRA_SC$S_SEX)
class(LIMRA_SC$S_SEX)
levels(LIMRA_SC$S_SEX)

LIMRA_SC$S_ANNUITANT = 1
LIMRA_SC[is.na(LIMRA_SC$S_SEX) & is.na(LIMRA_SC$S_BIRTH) & is.na(LIMRA_SC$S_CURRENT_AGE),'S_ANNUITANT'] = 0
nrow(LIMRA_SC[is.na(LIMRA_SC$S_SEX) & is.na(LIMRA_SC$S_BIRTH) & is.na(LIMRA_SC$S_CURRENT_AGE),])
LIMRA_SC$S_ANNUITANT = as.factor(LIMRA_SC$S_ANNUITANT)
class(LIMRA_SC$S_ANNUITANT)
table(LIMRA_SC$S_ANNUITANT)

# Treating DISTRIB
nrow(LIMRA_SC[is.na(LIMRA_SC$DISTRIB),]) # 28521 NA's. Left as it is.
# LIMRA_SC$DISTRIB = as.character(LIMRA_SC$DISTRIB)
# LIMRA_SC[is.na(LIMRA_SC$DISTRIB),"DISTRIB"] = "LEVEL_NA" 
LIMRA_SC$DISTRIB = as.factor(LIMRA_SC$DISTRIB)
class(LIMRA_SC$DISTRIB)
levels(LIMRA_SC$DISTRIB)


# Insight - company 0086  has most of these missing values.
# ..............


# Treating COSTSTR
nrow(LIMRA_SC[is.na(LIMRA_SC$COSTSTR),]) # 5 NA's. Left as it is.
LIMRA_SC$COSTSTR = as.factor(LIMRA_SC$COSTSTR)
class(LIMRA_SC$COSTSTR)
levels(LIMRA_SC$COSTSTR)
prop.table(table(LIMRA_SC$COSTSTR))
LIMRA_SC[is.na(LIMRA_SC$COSTSTR),"COSTSTR"] = 2

# Treating CUMLPREM
nrow(LIMRA_SC[is.na(LIMRA_SC$CUMLPREM),]) # 101598 NA's
# Insight - Company 1981 is not reporting CUMLPREM data and CUMLWITHD. Why?
# Separate model for 1981.
LIMRA_SC$CUMLPREMWITH.NA = ifelse(is.na(LIMRA_SC$CUMLPREM),1,0)
length(LIMRA_SC[LIMRA_SC$CUMLPREMWITH.NA == 1,"CUMLPREMWITH.NA"])
LIMRA_SC[is.na(LIMRA_SC$CUMLPREM),"CUMLPREM"] = median(LIMRA_SC$CUMLPREM,na.rm = T)



# Treating CURPREM
nrow(LIMRA_SC[is.na(LIMRA_SC$CURPREM),]) # 573 NA's. Left as it is.
nrow(LIMRA_SC[LIMRA_SC$CURPREM == 0,]) # 1574307 zeros.
# Insights - Out of 584 NA's, 493 are target == 1. Would that mean that CURPREM is nullified once a contract is surrendered?
# IF no, why such a high correlation.
LIMRA_SC$CURPREM.NA = ifelse(is.na(LIMRA_SC$CURPREM),1,0)
LIMRA_SC[is.na(LIMRA_SC$CURPREM),"CURPREM"] = 0
length(LIMRA_SC[LIMRA_SC$CURPREM.NA == 1,"CURPREM.NA"])


# LIMRA_SC[is.na(limra_2.3$CURPREM),'CURPREM'] = NA # NA can be put to some other level if some domain justification available
# However, if it's a post event data, it will lead to overfitting as no such data points will be avilable for test data.



# Treating FPREM
nrow(LIMRA_SC[is.na(LIMRA_SC$FPREM),]) # 593482 NA's. Imputed with 0
nrow(LIMRA_SC[!is.na(LIMRA_SC$FPREM) & LIMRA_SC$FPREM == 0,]) # 521300 0's.
LIMRA_SC[is.na(LIMRA_SC$FPREM),'FPREM'] = 0

# Treating CUMWITHD
nrow(LIMRA_SC[is.na(LIMRA_SC$CUMLWITHD) == T,]) # 101529 NA's. Left as it is.
# Insights - Company 1981 is not reporting CUMWITHD (143356/143358). Why?
# For company 1981, all contracts do not have CUMWITHD and CUMLPREM. Why this company is not reporting?
LIMRA_SC[is.na(LIMRA_SC$CUMLWITHD),"CUMLWITHD"] = median(LIMRA_SC$CUMLWITHD,na.rm = T)


# Treating WB_DAY_EFF
nrow(LIMRA_SC[is.na(LIMRA_SC$WB_DAY_EFF),]) # 185 NA's. Left as it is.
# nrow(LIMRA_SC[LIMRA_SC$WB_DAY_EFF == LIMRA_SC$DAYISS,])

plot(as.numeric(as.character(LIMRA_SC$WB_DAY_EFF)),
     as.numeric(as.character(LIMRA_SC$DAYISS)))
# - as.numeric(as.character(LIMRA_SC$WB_DAY_EFF)))
x = as.numeric(as.character(LIMRA_SC$DAYISS))
y = as.numeric(as.character(LIMRA_SC$WB_DAY_EFF))
sum(is.na(y))
plot(density(x))
plot(density(y))

LIMRA_SC$WB_DAY_EFF = as.factor(LIMRA_SC$WB_DAY_EFF)
levels(LIMRA_SC$WB_DAY_EFF)
LIMRA_SC[is.na(LIMRA_SC$WB_DAY_EFF),"WB_DAY_EFF"] = LIMRA_SC[is.na(LIMRA_SC$WB_DAY_EFF),"DAYISS"]

# Insights - The plot suggests that we should keep this variable along with DAYISS
# Couldn't find any reason of these missing values but highly correlated with target.

# Treating WB_BENBASE_BOY
nrow(LIMRA_SC[is.na(LIMRA_SC$WB_BENBASE_BOY) & is.na(LIMRA_SC$WB_BENBASE_EOY) & is.na(LIMRA_SC$WB_BENBASE_EOY),]) 
# plot(y=LIMRA_SC$WB_BENBASE_BOY - LIMRA_SC$AVBOY, x=LIMRA_SC$WB_BENBASE_BOY)
nrow(LIMRA_SC[LIMRA_SC$WB_BENBASE_BOY - LIMRA_SC$AVBOY < 0,]) # There are 321619 rows where AVBOY > WB_BENBASE_BOY
prop.table(table(LIMRA_SC[LIMRA_SC$WB_BENBASE_BOY - LIMRA_SC$AVBOY < 0,'TARGET'])) # Not much difference between the two
prop.table(table(LIMRA_SC[LIMRA_SC$WB_BENBASE_BOY - LIMRA_SC$AVBOY > 0,'TARGET']))
nrow(LIMRA_SC[LIMRA_SC$WB_BENBASE_BOY == LIMRA_SC$AVBOY,]) 
# Insights - WB_BENBASE_BOY > AVBOY for most of the contracts. However not much difference for target distribution in the two cases.
LIMRA_SC[is.na(LIMRA_SC$WB_BENBASE_BOY),"WB_BENBASE_BOY"] = LIMRA_SC[is.na(LIMRA_SC$WB_BENBASE_BOY),"WB_BENBASE_EOY"]
LIMRA_SC[is.na(LIMRA_SC$WB_BENBASE_BOY),"WB_BENBASE_BOY"] = LIMRA_SC[is.na(LIMRA_SC$WB_BENBASE_BOY),"AVBOY"]



# Treating WB_MAX_PERC
nrow(LIMRA_SC[is.na(LIMRA_SC$WB_MAX_PERC),]) # 44696 NA's. Left as it is.
LIMRA_SC[is.na(LIMRA_SC$WB_MAX_PERC),"WB_MAX_PERC"] = LIMRA_SC[is.na(LIMRA_SC$WB_MAX_PERC),"WBL_MAX_WITHD_PCT1"]
LIMRA_SC[is.na(LIMRA_SC$WB_MAX_PERC),"WB_MAX_PERC"] = LIMRA_SC[is.na(LIMRA_SC$WB_MAX_PERC),"WBL_MAX_WITHD_PCT2"]
LIMRA_SC[is.na(LIMRA_SC$WB_MAX_PERC),"WB_MAX_PERC"] = LIMRA_SC[is.na(LIMRA_SC$WB_MAX_PERC),"WBL_MAX_WITHD_PCT3"]
LIMRA_SC[is.na(LIMRA_SC$WB_MAX_PERC),"WB_MAX_PERC"] = LIMRA_SC[is.na(LIMRA_SC$WB_MAX_PERC),"WBL_MAX_WITHD_PCT4"]
LIMRA_SC[is.na(LIMRA_SC$WB_MAX_PERC),"WB_MAX_PERC"] = LIMRA_SC[is.na(LIMRA_SC$WB_MAX_PERC),"WBL_MAX_WITHD_PCT5"]
LIMRA_SC[is.na(LIMRA_SC$WB_MAX_PERC),"WB_MAX_PERC"] = LIMRA_SC[is.na(LIMRA_SC$WB_MAX_PERC),"WBL_MAX_WITHD_PCT6"]
LIMRA_SC[is.na(LIMRA_SC$WB_MAX_PERC),"WB_MAX_PERC"] = 0


# Treating WB_MAX_AMT
nrow(LIMRA_SC[is.na(LIMRA_SC$WB_MAX_AMT),]) # 2206 NA's. Left as it is.
nrow(LIMRA_SC[LIMRA_SC$WB_MAX_AMT == 0,])
LIMRA_SC[is.na(LIMRA_SC$WB_MAX_AMT),"WB_MAX_AMT"] = 0

# Treating WB_BENBASE_ANN
nrow(LIMRA_SC[is.na(LIMRA_SC$WB_BENBASE_ANN),]) # 206046 NA's. Left as it is.
# plot(y=LIMRA_SC$WB_BENBASE_ANN - LIMRA_SC$AVANN, x=LIMRA_SC$WB_BENBASE_ANN)

# Treating WB_LIFEPAYOUT
nrow(LIMRA_SC[is.na(LIMRA_SC$WB_LIFEPAYOUT),]) # 99220 NA's. Left as it is.
# LIMRA_SC$WB_LIFEPAYOUT = as.character(LIMRA_SC$WB_LIFEPAYOUT)
# LIMRA_SC[is.na(LIMRA_SC$WB_LIFEPAYOUT),"WB_LIFEPAYOUT"] = "LEVEL_NA"
LIMRA_SC$WB_LIFEPAYOUT = as.factor(LIMRA_SC$WB_LIFEPAYOUT)
class(LIMRA_SC$WB_LIFEPAYOUT)
levels(LIMRA_SC$WB_LIFEPAYOUT)
prop.table(table(LIMRA_SC$WB_LIFEPAYOUT, useNA = "ifany"))

# Treating WB_BENBASE_EOY
nrow(LIMRA_SC[is.na(LIMRA_SC$WB_BENBASE_EOY),]) # 15850 NA's. Left as it is.

# Treating WBL
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL),]) # No NA's.
LIMRA_SC$WBL = as.factor(LIMRA_SC$WBL)
class(LIMRA_SC$WBL)
levels(LIMRA_SC$WBL)

# Treating WBL_MO_INTR
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_MO_INTR),])# 40463 NA's.
prop.table(table(LIMRA_SC[is.na(LIMRA_SC$WBL_MO_INTR),"TARGET"]))
# LIMRA_SC$WBL_MO_INTR = as.character(LIMRA_SC$WBL_MO_INTR)
# LIMRA_SC[is.na(LIMRA_SC$WBL_MO_INTR),"WBL_MO_INTR"] = "LEVEL_NA"
LIMRA_SC$WBL_MO_INTR = as.factor(LIMRA_SC$WBL_MO_INTR)
class(LIMRA_SC$WBL_MO_INTR)
levels(LIMRA_SC$WBL_MO_INTR)



# Treating WBL_ELECT_POSTISSUE
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_ELECT_POSTISSUE),]) # 21448 NA's. Left as is. Categorical variables NA will be taken as another level.
# LIMRA_SC$WBL_ELECT_POSTISSUE = as.character(LIMRA_SC$WBL_ELECT_POSTISSUE)
# LIMRA_SC[is.na(LIMRA_SC$WBL_ELECT_POSTISSUE),"WBL_ELECT_POSTISSUE"] = "LEVEL_NA"
LIMRA_SC$WBL_ELECT_POSTISSUE = as.factor(LIMRA_SC$WBL_ELECT_POSTISSUE)
class(LIMRA_SC$WBL_ELECT_POSTISSUE)
levels(LIMRA_SC$WBL_ELECT_POSTISSUE)
prop.table(table(LIMRA_SC$WBL_ELECT_POSTISSUE,LIMRA_SC$TARGET))


# Treating WBL_CANCEL
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_CANCEL) == T,]) # 21448 NA's. Left as is. Categorical variables NA will be taken as another level.
class(LIMRA_SC$WBL_CANCEL)
LIMRA_SC$WBL_CANCEL = as.factor(LIMRA_SC$WBL_CANCEL)
levels(LIMRA_SC$WBL_CANCEL)
prop.table(table(LIMRA_SC$WBL_CANCEL, useNA = "ifany"))
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_CANCEL) == T & is.na(LIMRA_SC$WBL_ELECT_POSTISSUE) == T,]) # Same NA's for WBL_Cancel and WBL_ELECT_POSTISSUE
# Same contracts for which WBL_CANCEL and WBL_POSTISSUE data are missing
# Insights - Company 1981 majorly contributes to this missing data and holds good prop of 1's in that.


# Left for later ############################

nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_WAITPERIOD_1),]) # 753397 NA's. Left as is
nrow(LIMRA_SC[LIMRA_SC$WBL_WAITPERIOD_1 == 0 & !is.na(LIMRA_SC$WBL_WAITPERIOD_1),]) # 1372227 Null's plus 0's
LIMRA_SC$WBL_WAITPERIOD_1 = as.factor(LIMRA_SC$WBL_WAITPERIOD_1)
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_WAITPERIOD_2),]) # 1442265 NA's. Left as is
nrow(LIMRA_SC[LIMRA_SC$WBL_WAITPERIOD_2 == 0 & !is.na(LIMRA_SC$WBL_WAITPERIOD_2),]) # 1570893 Null's plus 0's
LIMRA_SC$WBL_WAITPERIOD_2 = as.factor(LIMRA_SC$WBL_WAITPERIOD_2)
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_WAITPERIOD_3),]) # 1495624 NA's. Left as is
nrow(LIMRA_SC[LIMRA_SC$WBL_WAITPERIOD_3 == 0 &!is.na(LIMRA_SC$WBL_WAITPERIOD_3),]) # 1935342 Null's plus 0's
LIMRA_SC$WBL_WAITPERIOD_3 = as.factor(LIMRA_SC$WBL_WAITPERIOD_3)

nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_WAITPERIOD_2) & is.na(LIMRA_SC$WBL_WAITPERIOD_3),])
# Insight - NA's of WBL_WAITPERIOD_1 are a subset of WBL_WAITPERIOD_2 and so on...


# Treating WBL_MAX_AGE_ELECT
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_MAX_AGE_ELECT),]) # 44224 NA's. Left as is
# Insight - Missing of WBL_MAX_AGE_ELECT are a superset of NA's of WBL_CANCEL and WBL_ELECT_POSTISSUE
class(LIMRA_SC$WBL_MAX_AGE_ELECT)
LIMRA_SC$WBL_MAX_AGE_ELECT = as.factor(LIMRA_SC$WBL_MAX_AGE_ELECT)
levels(LIMRA_SC$WBL_MAX_AGE_ELECT)


# Treating WBL_MIN_AGE_ONSET
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_MIN_AGE_ONSET),])# 117712 NA's. Left as is
LIMRA_SC$WBL_MIN_AGE_ONSET = as.factor(LIMRA_SC$WBL_MIN_AGE_ONSET)
class(LIMRA_SC$WBL_MIN_AGE_ONSET)
levels(LIMRA_SC$WBL_MIN_AGE_ONSET)
# Insights - Company 507 did not report this fields data.


# Treating WBL_MAX_AGE_ONSET
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_MAX_AGE_ONSET) == T,]) # 1008168 NA's. Left as is
LIMRA_SC$WBL_MAX_AGE_ONSET = as.factor(LIMRA_SC$WBL_MAX_AGE_ONSET)
class(LIMRA_SC$WBL_MAX_AGE_ONSET)
levels(LIMRA_SC$WBL_MAX_AGE_ONSET)




# Treating WBL_SPOUSAL
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_SPOUSAL),]) # 22997 NA's. Left as is
LIMRA_SC$WBL_SPOUSAL = as.factor(LIMRA_SC$WBL_SPOUSAL)
class(LIMRA_SC$WBL_SPOUSAL)
levels(LIMRA_SC$WBL_SPOUSAL)
prop.table(table(LIMRA_SC$WBL_SPOUSAL))

# Treating WBL_YRS_PREMAPPLIED
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_YRS_PREMAPPLIED),]) # 402350 NA's. Left as is
# Insights - Again comapny 1981 seems to have all NA's for this field. We can bin and convert to ordinal type but
# would not make much sense. It's better we leave NA's as is. Make ordinal. Impute with median and 
LIMRA_SC$WBL_YRS_PREMAPPLIED = as.factor(LIMRA_SC$WBL_YRS_PREMAPPLIED)
class(LIMRA_SC$WBL_YRS_PREMAPPLIED)
levels(LIMRA_SC$WBL_YRS_PREMAPPLIED)


# Treating WBL_ALLOC_REST_FORCED
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_ALLOC_REST_FORCED),]) # 21473 NA's. Left as is
LIMRA_SC$WBL_ALLOC_REST_FORCED = as.factor(LIMRA_SC$WBL_ALLOC_REST_FORCED)
class(LIMRA_SC$WBL_ALLOC_REST_FORCED)
levels(LIMRA_SC$WBL_ALLOC_REST_FORCED)
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_ALLOC_REST_FORCED) & is.na(LIMRA_SC$WBL_CANCEL),])
# Insights - NA's of WBL_ALLOC_REST_FORCED are superset of NA's of WBL_CANCEL and WBL_POSTISSUE. Same holds for this variable as well.


# Treating WBL_ALLOC_REST_LIMITS
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_ALLOC_REST_LIMITS) == T,]) # 21473 NA's. Left as is
LIMRA_SC$WBL_ALLOC_REST_LIMITS = as.factor(LIMRA_SC$WBL_ALLOC_REST_LIMITS)
class(LIMRA_SC$WBL_ALLOC_REST_LIMITS)
levels(LIMRA_SC$WBL_ALLOC_REST_LIMITS)
# Insights - NA's of WBL_ALLOC_REST_FORCED & WBL_ALLOC_REST_LIMITS are common and 
# are superset of NA's of WBL_CANCEL and WBL_POSTISSUE. Same holds for this variable as well.

# Treating WBL_ALLOC_REST_LIMITS
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_ALLOC_REST_OTHER),]) # 21473 NA's. Left as is
LIMRA_SC$WBL_ALLOC_REST_OTHER = as.factor(LIMRA_SC$WBL_ALLOC_REST_OTHER)
class(LIMRA_SC$WBL_ALLOC_REST_OTHER)
levels(LIMRA_SC$WBL_ALLOC_REST_OTHER)
# Insights - NA's of WBL_ALLOC_REST_FORCED & WBL_ALLOC_REST_LIMITS & WBL_ALLOC_REST_OTHER are common and 
# are superset of NA's of WBL_CANCEL and WBL_POSTISSUE. Same holds for this variable as well.

nrow(limra_2.3[is.na(limra_2.3$WBL_ALLOC_REST_LIMITS) == T & is.na(limra_2.3$WBL_ALLOC_REST_FORCED) == T & is.na(limra_2.3$WBL_ALLOC_REST_OTHER) == T,]) # Common NA's for WBL_ALLOC_REST_FORCED, WBL_ALLOC_REST_LIMITS and WBL_ALLOC_REST_OTHER


# Treating WBL_BEN_REDUCT_D4D
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_BEN_REDUCT_D4D),]) # 217942 NA's. Left as is
LIMRA_SC$WBL_BEN_REDUCT_D4D = as.factor(LIMRA_SC$WBL_BEN_REDUCT_D4D)
class(LIMRA_SC$WBL_BEN_REDUCT_D4D)
levels(LIMRA_SC$WBL_BEN_REDUCT_D4D)
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_BEN_REDUCT_D4D) & is.na(LIMRA_SC$WBL_ALLOC_REST_FORCED),])
# Insights - Contracts having NA's for previous fields (WBL_CANCEL,WBL_POSTISSUE,WBL_ALLOC_____) also have NA's in here.
# It confirms that it is a missing data issue or some companies do not have these flags.


# Treating WBL_BEN_REDUCT_PR
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_BEN_REDUCT_PR),]) # 217942 NA's. Left as is
LIMRA_SC$WBL_BEN_REDUCT_PR = as.factor(LIMRA_SC$WBL_BEN_REDUCT_PR)
class(LIMRA_SC$WBL_BEN_REDUCT_PR)
levels(LIMRA_SC$WBL_BEN_REDUCT_PR)


# Treating WBL_BEN_REDUCT_RMD
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_BEN_REDUCT_RMD),]) # 217942 NA's. Left as is
LIMRA_SC$WBL_BEN_REDUCT_RMD = as.factor(LIMRA_SC$WBL_BEN_REDUCT_RMD)
class(LIMRA_SC$WBL_BEN_REDUCT_RMD)
levels(LIMRA_SC$WBL_BEN_REDUCT_RMD)
# Insights - Common NA's for above three fields.


nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_BEN_REDUCT_OTH),]) # 475170 NA's. Left as is
class(LIMRA_SC$WBL_BEN_REDUCT_OTH)
LIMRA_SC$WBL_BEN_REDUCT_OTH = as.factor(LIMRA_SC$WBL_BEN_REDUCT_OTH)
levels(LIMRA_SC$WBL_BEN_REDUCT_OTH)

nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_BEN_REDUCT_OTH)&is.na(LIMRA_SC$WBL_BEN_REDUCT_RMD),])
# Insights - NA's for above fields (WBL_BEN_REDUCT_RMD,WBL_BEN_REDUCT_PR,WBL_BEN_REDUCT_D4d) are also NA here.


# Treating WBL_STEPUP_AVAIL
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_STEPUP_AVAIL),]) # 21448 NA's. Left as is
LIMRA_SC$WBL_STEPUP_AVAIL = as.factor(LIMRA_SC$WBL_STEPUP_AVAIL)
class(LIMRA_SC$WBL_STEPUP_AVAIL)
levels(LIMRA_SC$WBL_STEPUP_AVAIL)

nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_STEPUP_AVAIL)&is.na(LIMRA_SC$WBL_CANCEL),])
# Insights - NA's for WBL_CANCEL aND WBL_POSTISSUE are common with this field's NA's.


# Treating WBL_STEPUP_FREQ
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_STEPUP_FREQ),]) # 22856 NA's. Left as is
LIMRA_SC$WBL_STEPUP_FREQ = as.factor(LIMRA_SC$WBL_STEPUP_FREQ)
class(LIMRA_SC$WBL_STEPUP_FREQ)
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_STEPUP_FREQ)&is.na(LIMRA_SC$WBL_CANCEL),])
# Insights - NA's for WBL_CANCEL aND WBL_POSTISSUE are subset with NA's of this field's NA's.


# Treating WBL_STEPUP_AUTO
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_STEPUP_AUTO),]) # 119248 NA's. Left as is
LIMRA_SC$WBL_STEPUP_AUTO = as.factor(LIMRA_SC$WBL_STEPUP_AUTO)
class(LIMRA_SC$WBL_STEPUP_AUTO)
levels(LIMRA_SC$WBL_STEPUP_AUTO)


# Treating WBL_STEPUP_CHANGE
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_STEPUP_CHANGE),]) # 33545 NA's. Left as is
LIMRA_SC$WBL_STEPUP_CHANGE = as.factor(LIMRA_SC$WBL_STEPUP_CHANGE)
class(LIMRA_SC$WBL_STEPUP_CHANGE)
levels(LIMRA_SC$WBL_STEPUP_CHANGE)


# Treating WBL_STEPUP_COST
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_STEPUP_COST),]) # 422005 NA's. Left as is
LIMRA_SC$WBL_STEPUP_COST = as.factor(LIMRA_SC$WBL_STEPUP_COST)
class(LIMRA_SC$WBL_STEPUP_COST)
levels(LIMRA_SC$WBL_STEPUP_COST)
# Insights - Missing data patern in report shows company 1981 as usual do not report this data.

# Treating WBL_STEPUP_WINDOW
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_STEPUP_WINDOW),]) # 477845 NA's. Left as is
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_STEPUP_WINDOW)&is.na(LIMRA_SC$WBL_STEPUP_COST),])
LIMRA_SC$WBL_STEPUP_WINDOW = as.factor(LIMRA_SC$WBL_STEPUP_WINDOW)
class(LIMRA_SC$WBL_STEPUP_WINDOW)
levels(LIMRA_SC$WBL_STEPUP_WINDOW)
# Insights -  


# Treating WBL_MAX_WITHD_PCT_(1-6)
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_MAX_WITHD_PCT1) == T,]) # 259548 NA's. Left as is
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_MAX_WITHD_PCT2) == T,]) # 362695 NA's. Left as is
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_MAX_WITHD_PCT3) == T,]) # 518395 NA's. Left as is
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_MAX_WITHD_PCT4) == T,]) # 639451 NA's. Left as is
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_MAX_WITHD_PCT5) == T,]) # 889226 NA's. Left as is
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_MAX_WITHD_PCT6) == T,]) # 1336627 NA's. Left as is
# Make another column WBL_MAX_WTHD_COMPLEXITY

############## Checking how to impute WBL_MAX_WITHD_PCT1 #############################
nrow(LIMRA_SC[is.na(LIMRA_SC$WB_MAX_PERC),])
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_MAX_WITHD_PCT1),])
nrow(LIMRA_SC[!is.na(LIMRA_SC$WBL_MAX_WITHD_PCT1) & is.na(LIMRA_SC$WB_MAX_PERC),])
sum(ifelse(LIMRA_SC$WB_MAX_PERC == LIMRA_SC$WBL_MAX_WITHD_PCT1,0,1),na.rm = T)
# plot(density(LIMRA_SC$WB_MAX_PERC - LIMRA_SC$WBL_MAX_WITHD_PCT1))
# plot(LIMRA_SC[(LIMRA_SC$WB_MAX_PERC != LIMRA_SC$WBL_MAX_WITHD_PCT1),'WBL_MAX_WITHD_PCT1'],
# (LIMRA_SC[(LIMRA_SC$WB_MAX_PERC != LIMRA_SC$WBL_MAX_WITHD_PCT1),'WB_MAX_PERC'] -
# LIMRA_SC[(LIMRA_SC$WB_MAX_PERC != LIMRA_SC$WBL_MAX_WITHD_PCT1),'WBL_MAX_WITHD_PCT1']))

# plot(LIMRA_SC[(LIMRA_SC$WB_MAX_PERC != LIMRA_SC$WBL_MAX_WITHD_PCT1),'WBL_MAX_WITHD_PCT1'],
#      LIMRA_SC[(LIMRA_SC$WB_MAX_PERC != LIMRA_SC$WBL_MAX_WITHD_PCT1),'WB_MAX_PERC'])

length(LIMRA_SC[(LIMRA_SC$WB_MAX_PERC != LIMRA_SC$WBL_MAX_WITHD_PCT1) & !is.na(LIMRA_SC$WBL_MAX_WITHD_PCT1) &
                !is.na(LIMRA_SC$WB_MAX_PERC), 'WBL_MAX_WITHD_PCT1'])

levels(as.factor(as.character(LIMRA_SC[(LIMRA_SC$WB_MAX_PERC != LIMRA_SC$WBL_MAX_WITHD_PCT1) & !is.na(LIMRA_SC$WBL_MAX_WITHD_PCT1) &
                  !is.na(LIMRA_SC$WB_MAX_PERC), 'WBL_MAX_WITHD_PCT1'])))

levels(as.factor(as.character(LIMRA_SC[(LIMRA_SC$WB_MAX_PERC != LIMRA_SC$WBL_MAX_WITHD_PCT1) & !is.na(LIMRA_SC$WBL_MAX_WITHD_PCT1) &
           !is.na(LIMRA_SC$WB_MAX_PERC), 'WB_MAX_PERC'])))

nrow(LIMRA_SC[LIMRA_SC$WBL_MAX_WITHD_PCT1 == 0,])
table(LIMRA_SC[is.na(LIMRA_SC$WBL_MAX_WITHD_PCT1),'LIMRA'])
table(LIMRA_SC[is.na(LIMRA_SC$WBL_MAX_WITHD_PCT1),'TARGET'])
table(LIMRA_SC[LIMRA_SC$WB_MAX_PERC == LIMRA_SC$WBL_MAX_WITHD_PCT1, 'TARGET'])
table(LIMRA_SC[LIMRA_SC$WB_MAX_PERC != LIMRA_SC$WBL_MAX_WITHD_PCT1, 'TARGET'])


a = (LIMRA_SC[(LIMRA_SC$WB_MAX_PERC != LIMRA_SC$WBL_MAX_WITHD_PCT1) & !is.na(LIMRA_SC$WBL_MAX_WITHD_PCT1) & !is.na(LIMRA_SC$WB_MAX_PERC),])
nrow(a[a$WB_MAX_PERC == a$WBL_MAX_WITHD_PCT2,])
rm(a)
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_MAX_WITHD_PCT1) & is.na(LIMRA_SC$WBL_MAX_WITHD_PCT2),])

############# Treating WBL_MAX__WITHD_PCT1 #############################################################33
LIMRA_SC[is.na(LIMRA_SC$WBL_MAX_WITHD_PCT1),'WBL_MAX_WITHD_PCT1'] = LIMRA_SC[is.na(LIMRA_SC$WBL_MAX_WITHD_PCT1),'WBL_MAX_WITHD_PCT2']
LIMRA_SC[is.na(LIMRA_SC$WBL_MAX_WITHD_PCT1),'WBL_MAX_WITHD_PCT1'] = 5  # Median value of WB_MAX_PERC for WBL_PCT1 & WBL_PCT2 NULLS.


############### Creating another column as WBL_MAX_WITHD_PCT_COMPLEXITY #############################################
WBL_MAX_WITHD_PCT_MTX = cbind(LIMRA_SC$WBL_MAX_WITHD_PCT1,LIMRA_SC$WBL_MAX_WITHD_PCT2,LIMRA_SC$WBL_MAX_WITHD_PCT3,
                              LIMRA_SC$WBL_MAX_WITHD_PCT4,LIMRA_SC$WBL_MAX_WITHD_PCT5,LIMRA_SC$WBL_MAX_WITHD_PCT6)

complexity = apply(WBL_MAX_WITHD_PCT_MTX,1,function(k) ifelse(is.na(which(is.na(k))[1]),1,which(is.na(k))[1]))
head(complexity)
range(complexity)
levels(as.factor(as.character(complexity)))
table(as.factor(as.character(complexity)))

LIMRA_SC$WBL_MAX_WITHD_PCT_COMPLEXITY = complexity


# treating WBL_LTC
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_LTC),]) # 253994 NA's. Left as is
class(LIMRA_SC$WBL_LTC)
LIMRA_SC$WBL_LTC = as.factor(LIMRA_SC$WBL_LTC)
levels(LIMRA_SC$WBL_LTC)
nrow(LIMRA_SC[is.na(LIMRA_SC$WBL_LTC)&is.na(LIMRA_SC$WBL_STEPUP_AVAIL),])
# Insights - Missing data patterns show WBL_STEPUP_AVAIL and WBL_CANCEL are subset of this field's missing values


# Treating S_CURRENT_AGE
nrow(LIMRA_SC[is.na(LIMRA_SC$S_CURRENT_AGE) == T,]) # 1537671 NA's. Left as is


# Treating OWNER_AGE and AGE_AT_PURCHASE
nrow(LIMRA_SC[is.na(LIMRA_SC$OWNER_AGE) == T,]) # No NA's.
nrow(LIMRA_SC[is.na(LIMRA_SC$AGE_AT_PURCHASE) == T,]) #  No NA's.
# Relation of OWNER_AGE with AGE_AT_PURCHASE, OBSYR & YRISS
a = ifelse(LIMRA_SC$OWNER_AGE == (LIMRA_SC$AGE_AT_PURCHASE + (as.numeric(as.character(LIMRA_SC$OBSYR)) - as.numeric(as.character(LIMRA_SC$YRISS)))),1,0)
sum(a)

############### Treating PCT_CALC_MAX_WITHDRAWN #################################################
nrow(LIMRA_SC[is.na(LIMRA_SC$PCT_CALC_MAX_WITHDRAWN),]) #  1266449 NA's. Left as is.
nrow(LIMRA_SC[is.na(LIMRA_SC$S_PCT_MAX_WITHDRAWN),]) #  1373314 NA's.
nrow(LIMRA_SC[is.na(LIMRA_SC$S_PCT_CALC_MAX_WITHDRAWN),]) # 1359398 NA's.
LIMRA_SC[is.na(LIMRA_SC$PCT_CALC_MAX_WITHDRAWN),'PCT_CALC_MAX_WITHDRAWN'] = LIMRA_SC[is.na(LIMRA_SC$PCT_CALC_MAX_WITHDRAWN),'PWITHD']/LIMRA_SC[is.na(LIMRA_SC$PCT_CALC_MAX_WITHDRAWN),'WB_MAX_AMT']
LIMRA_SC[is.na(LIMRA_SC$PCT_CALC_MAX_WITHDRAWN),'PCT_CALC_MAX_WITHDRAWN'] = 0
LIMRA_SC[LIMRA_SC$PCT_CALC_MAX_WITHDRAWN == Inf,"PCT_CALC_MAX_WITHDRAWN"] = 0


# Treating RAT_BENBASE_BOY
nrow(LIMRA_SC[is.na(LIMRA_SC$RAT_BENBASE_BOY),]) #  38780 NA's.
# a = ifelse(LIMRA_SC$RAT_BENBASE_BOY == (LIMRA_SC$WB_BENBASE_BOY/LIMRA_SC$AVBOY),1,0)
# sum(a,na.rm = T)
LIMRA_SC[is.na(LIMRA_SC$RAT_BENBASE_BOY),'RAT_BENBASE_BOY'] = LIMRA_SC[is.na(LIMRA_SC$RAT_BENBASE_BOY),'WB_BENBASE_BOY']/LIMRA_SC[is.na(LIMRA_SC$RAT_BENBASE_BOY),'AVBOY']
length(LIMRA_SC[is.na(LIMRA_SC$RAT_BENBASE_BOY) & is.na(LIMRA_SC$WB_BENBASE_BOY),'AVBOY'])
LIMRA_SC[is.na(LIMRA_SC$RAT_BENBASE_BOY),'RAT_BENBASE_BOY'] = 0
LIMRA_SC[LIMRA_SC$RAT_BENBASE_BOY == Inf,"RAT_BENBASE_BOY"] = 0

# Treating RAT_BENBASE_EOY
nrow(LIMRA_SC[is.na(LIMRA_SC$RAT_BENBASE_EOY),]) #  105886 NA's.

# Treating Rollup_Ind
nrow(LIMRA_SC[is.na(LIMRA_SC$Rollup_Ind),]) #  No NA's.
LIMRA_SC$Rollup_Ind = as.factor(LIMRA_SC$Rollup_Ind)
class(LIMRA_SC$Rollup_Ind)
levels(LIMRA_SC$Rollup_Ind)

# Treating Rollup_WP
nrow(LIMRA_SC[is.na(LIMRA_SC$Rollup_WP),]) #  No NA's.
class(LIMRA_SC$Rollup_WP)


# Treating ITMrange
nrow(LIMRA_SC[is.na(LIMRA_SC$ITMrange) == T,]) #  No NA's. # 182756
levels(LIMRA_SC$ITMrange)
ITM_OVER125 = LIMRA_SC[LIMRA_SC$ITMrange == "Over 125%",'RAT_BENBASE_BOY']
LIMRA_SC[LIMRA_SC$ITMrange == "Over 125%",'ITMrange'] = ifelse(ITM_OVER125 > 1.25 & ITM_OVER125 < 1.50, 
                                                               "125% to 150%", "150% and over")
LIMRA_SC$ITMrange = factor(LIMRA_SC$ITMrange)
levels(LIMRA_SC$ITMrange)



# Treating OBSYR
nrow(LIMRA_SC[is.na(LIMRA_SC$OBSYR) == T,]) #  No NA's.
LIMRA_SC$OBSYR = as.factor(LIMRA_SC$OBSYR)
class(LIMRA_SC$OBSYR)
levels(LIMRA_SC$OBSYR)
prop.table(table(LIMRA_SC$OBSYR,useNA = "ifany"))

# Treating TARGET
nrow(LIMRA_SC[is.na(LIMRA_SC$TARGET) == T,]) #  No NA's.
LIMRA_SC$TARGET = as.factor(LIMRA_SC$TARGET)
class(LIMRA_SC$TARGET)
levels(LIMRA_SC$TARGET)



All_categorical_NA_treated = function(dataframe)
{
  for(i in colnames(dataframe))
  {
  if(class(dataframe[,i]) == "factor")
  {
    dataframe[,i] = as.character(dataframe[,i])
    dataframe[is.na(dataframe[,i]),i] = "LEVEL_NA"
    dataframe[,i] = as.factor(dataframe[,i])
  }
  }
  dataframe
}

LIMRA_SC = All_categorical_NA_treated(LIMRA_SC)

LIMRA_SC$CUMLPREMWITH.NA = as.factor(LIMRA_SC$CUMLPREMWITH.NA)
LIMRA_SC$CURPREM.NA = as.factor(LIMRA_SC$CURPREM.NA)

str(LIMRA_SC)
rm(list=setdiff(ls(), "LIMRA_SC"))
########################################################################


#################### Writing out LIMRA_SC ##############################
# write.csv(x = LIMRA_SC,file = "LIMRA_SC.csv",row.names = F)
# library(foreign)
# write.foreign(LIMRA_SC, datafile = "LIMRA_SC.csv",codefile = "LIMRA_SC.sas7bdat",   package="SAS")
# 
# library(rio)
# export(LIMRA_SC,"LIMRA_SC_rio.sas7bdat")
# export(LIMRA_SC, "LIMRA_SC_rio.csv")
###########################################################################

nrow(LIMRA_SC[LIMRA_SC$WBL_CANCEL == "LEVEL_NA" & LIMRA_SC$LIMRA == "1981",])
table(LIMRA_SC[LIMRA_SC$WBL_CANCEL == "LEVEL_NA","WBL_CANCEL"],LIMRA_SC[LIMRA_SC$WBL_CANCEL == "LEVEL_NA","LIMRA"])

mean(as.numeric(as.character(LIMRA_SC[LIMRA_SC$WBL_CANCEL == "LEVEL_NA" & LIMRA_SC$LIMRA == "1981","TARGET"])))





