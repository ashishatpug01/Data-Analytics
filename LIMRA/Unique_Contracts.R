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

