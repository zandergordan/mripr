library(stringr)
library(survey)
options(survey.lonely.psu="certainty")

#Load SE MRIP data
#trips: Wave 6 private boat trips to Florida
#catch: Wave 6 private boat trips to West Florida that caught (A+B1+B2) gag

trips=readRDS("data/clean_data/trips.rds")
catch=readRDS("data/clean_data/catch.rds")

#Create county FIPS code and indicator for Florida residents
trips$FIP=paste0(str_pad(trips$ST_RES,2,pad="0"),str_pad(trips$CNTY_RES,3,pad="0"))
trips$FL_RES=trips$ST_RES=="12"

#Merge trip and grouper catch data
trips=dplyr::left_join(trips,catch[,c("ID_CODE","wp_catch","tot_cat")],by="ID_CODE")
trips$tot_cat[is.na(trips$tot_cat)]=0

#Trips that targeted or caught gag grouper in the GOM
trips$tgtGagGOM=with(trips,SUB_REG==7 & (prim1_common=="GAG" | prim2_common=="GAG" | tot_cat>0) )

# The following table presents the estimates of Florida private boat trips and WFL gag grouper
# catch for Nov-Dec from 2005 to 2017. These estimates can be compared with the estimates on the MRIP
# website for the same period to verify we are using the correct data.
estv=aggregate(cbind(wp_int,I(wp_int*tot_cat))~YEAR,FUN=sum,data=trips)
names(estv)[2:3]=c("TRIPS","CATCH")
estv$YEAR=as.character(estv$YEAR)
pander::pander(estv[,c("YEAR","TRIPS","CATCH")],
               row.names = FALSE,
               caption = "Florida Private Boat Trips and West Florida Catch of Gag Grouper: 2005-2017, Nov-Dec",
               round=0, big.mark=',',justify="center")

#Counter variable
trips$one=1

#Missing zip codes after 2004; working to get codes added back
aggregate(is.na(trips$ZIP)~YEAR,FUN=sum,data=trips)

#Count of FL resident interviews in wave 6 by zip of origin
intsByZip=aggregate(tot_cat~ZIP,FUN=length,data=subset(trips,FL_RES & YEAR<2004))
intsByZipByYr=aggregate(tot_cat~ZIP+YEAR,FUN=length,data=subset(trips,FL_RES & YEAR<2004))


#Trips survey design object
tripss=svydesign(id=~psu_id, strata=~strat_id,weights=~wp_int, data=trips)

#FL resident gag total catch (2000-2004) in wave 6 by zip of origin
catchByZipT=svyby(~one+tot_cat,~ZIP,FUN=svytotal,
                  design=subset(tripss,FL_RES  & YEAR<2004),vartype="ci",multicore=TRUE)

#FL resident gag average catch per trip (2000-2004) in wave 6 by zip of origin
catchByZipM=svyby(~tot_cat,~ZIP,FUN=svymean,
                  design=subset(tripss,FL_RES & YEAR<2004),vartype="ci",multicore=TRUE)