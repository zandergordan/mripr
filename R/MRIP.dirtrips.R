#' Calculate directed trips
#'
#' @param intdir data directory
#' @param common species names(s)
#' @param st The state(s)
#' @param styr Start year
#' @param endyr End year
#' @param trips type of trips
#' @param dom optional custom domain
#'
#' @return a dataframe with directed trip estimates and PSEs
#' @export
#'
MRIP.dirtrips<-function(intdir=NULL,common=NULL, st=NULL,styr=NULL,
   endyr=NULL, trips=c(1,2,3,4,5),dom=NULL){
    if(is.null(intdir)) stop("Need main directory location of intercept files.")
    if(is.null(common)) stop("Need Common Name for common.")
    if(is.null(st)) stop("No state code(s) was specified.")
    if(is.null(styr)) stop("Starting year is missing.")
    if(is.null(endyr)) stop("Ending year is missing.")

    if(length(trips)!=1) stop("Only one trip option can be used at a time for accurate calculations.")
  
    if(length(grep("/",intdir))==1){
        din<-ifelse(substr(intdir,nchar(intdir),nchar(intdir)) %in% c("/"),
          c(paste(intdir,"int",sep="")),c(paste(intdir,"/int",sep="")))
    }
    if(length(grep("\\\\",intdir))==1){
        din<-ifelse(substr(intdir,nchar(intdir),nchar(intdir)) %in% c("\\"),
          c(paste(intdir,"int",sep="")),c(paste(intdir,"\\int",sep="")))
    }
  common<-tolower(common)
  st<-as.character(st)
  styr<-as.character(styr)
  endyr<-as.character(endyr)
  wave<-as.character(c(1,2,3,4,5,6))
  dom_id<-NULL
  dataset<-NULL
  temp<-NULL
  temp1<-NULL
   rbind2<- function(input1, input2){
   if(!is.null(ncol(input1))){
   n.input1 <- ncol(input1)
   n.input2 <- ncol(input2)
   
   if (n.input2 < n.input1) {
     TF.names <- which(names(input2) %in% names(input1))         
     column.names <- names(input2[, TF.names])
     } 
   else {TF.names <- which(names(input1) %in% names(input2))
     column.names <- names(input1[, TF.names])}
   return(rbind(input1[, column.names], input2[, column.names]))
   }
    if(is.null(ncol(input1))) return(rbind(input1,input2))
}
	for(yr in styr:endyr){
 	   for (j in 1:as.numeric(length(wave))){ 
             #Get catch
      	     wv<-wave[j] 
             t3<-utils::read.csv(paste(din,yr,"/","catch_",yr,wv,".csv",sep=""),
               colClasses=c("character"),na.strings= "../../../../Downloads/MRIP Direct Angler Trip Estimation Template Program in R")
               t3<-t3[t3$ST %in% c(st),]
             names(t3)<-tolower(names(t3))
            temp<-methods::rbind2(temp,t3)
      #get trips
             t4<-utils::read.csv(paste(din,yr,"/","trip_",yr,wv,".csv",sep=""),
              colClasses=c("character"),na.strings= "../../../../Downloads/MRIP Direct Angler Trip Estimation Template Program in R")
             t4<-t4[t4$ST %in% c(st),]
         names(t4)<-tolower(names(t4))    
        temp1<-methods::rbind2(temp1,t4)
 	   }
	}
  convtolow<-function(x){
    for(i in 1:ncol(x)) x[,i]<-tolower(x[,i])
    return(x)
  }
  temp<-convtolow(temp)
  temp1<-convtolow(temp1)
  temp<-temp[,c("common","strat_id","psu_id","st","id_code","sp_code",
        "claim","release","harvest","tot_len_a","wgt_a","tot_len_b1","wgt_b1","fl_reg","tot_cat",
     "wgt_ab1","tot_len","landing")]
  temp<-temp[order(temp$strat_id,temp$psu_id,temp$id_code),]
  temp1<-temp1[order(temp1$strat_id,temp1$psu_id,temp1$id_code),]
 
  dataset<-merge(temp1,temp,by.x=c("strat_id","psu_id","id_code","st"),
                by.y=c("strat_id","psu_id","id_code","st"),all.x=FALSE,all.y=FALSE)
  dataset$common<-as.character(dataset$common)
  dataset$common<-ifelse(is.na(dataset$common),"",dataset$common)
  
  if(!any(dataset$common==common)) stop("common not found.")
    
  #Construct Domain
  dom_ids<-NULL
  mainlev<-length(dom)
  if(length(dom)>0){
    for(l in 1:mainlev){
      if (!any(names(dom)[l]==names(dataset))) stop(paste("Variable ",names(dom[l]),"not found in MRIP dataset"))
      if (any(names(dom)[l]==names(dataset))){
          dataset[,ncol(dataset)+1]<-"DELETE"
          names(dataset)[ncol(dataset)]<-c(paste(names(dom)[l],"1",sep=""))
          colpos<-which(names(dataset)==names(dom[l]))
           sublev<-length(dom[[l]])
         for (k in 1:sublev) dataset[,ncol(dataset)]<-ifelse(dataset[,colpos] %in% c(as.character(dom[[l]][[k]])),c(paste(names(dom)[l],k,sep="")),dataset[,ncol(dataset)])  
        dom_ids<-c(dom_ids,names(dom[l]))   
      }
    }
    test<-c("year","wave","st","sub_reg","mode_fx","area_x")
    for(gg in 1:as.numeric(length(dom_ids))){
      if(!any(dom_ids[gg]==test)) test[as.numeric(length(test)+1)]<-c(paste(dom_ids[gg],"1",sep="")) 
      if(any(dom_ids[gg]==test)){
        colpos<-which(test==dom_ids[gg])
        test[colpos]<-c(paste(dom_ids[gg],"1",sep=""))
      }
    }
    ## add Non-domain names to values in columns
      for(gg in 1:as.numeric(length(test))){
        if(substr(test[gg],nchar(test[gg]),nchar(test[gg]))!="1"){
          eval(parse(text=paste("dataset$",test[gg],'<-paste("',as.character(test[gg]),'",dataset$',test[gg],',sep="")',sep="")))
        }
      }
   
   for(hh in 1:as.numeric(length(test))){
     if(hh==1) texter<-paste("dataset$",test[hh],sep="") 
     if(hh>1) texter<-paste(texter,",","dataset$",test[hh],sep="")  
   }
     eval(parse(text=paste("dataset$dom_id<-c(paste(",texter,",sep=''))",sep="")))
  }# dom>0 
 
  if(length(dom)==0){
   dataset$year<-c(paste("year",dataset$year,sep=""))
   dataset$wave<-c(paste("wave",dataset$wave,sep=""))
   dataset$st<-c(paste("st",dataset$st,sep=""))
   dataset$sub_reg<-c(paste("sub_reg",dataset$sub_reg,sep=""))
   dataset$mode_fx<-c(paste("mode_fx",dataset$mode_fx,sep=""))
   dataset$area_x<-c(paste("area_x",dataset$area_x,sep=""))
   dataset$dom_id<-c(paste(dataset$year,dataset$wave,dataset$st,dataset$sub_reg,
                   dataset$mode_fx,dataset$area_x,sep=""))
  }


#This is additional code to account for grouped catch.
#It looks for the max claim value for a given leader code and common value of interest, 
#and assigns that value to all records with the same leader code (regardless of common value).
  if(trips==3){
	subset <- dataset[which(dataset$common==common),]

	maxclaim <- stats::aggregate(claim ~ leader, subset, max)
	combined <- merge(dataset,maxclaim, by="leader", all=TRUE)

	dataset <- dataset[order(dataset$leader),]
	combined <- combined[order(combined$leader),]

	dataset$claim <- combined$claim.y

	sub1 <- dataset[which(as.numeric(dataset$claim)> 0 & dataset$common!=common),]
	sub2 <- dataset[-which(as.numeric(dataset$claim)> 0 & dataset$common!=common),]
	sub1$common <- common
	dataset <- rbind(sub1, sub2)
  }

  dataset$tot_cat<-as.numeric(dataset$tot_cat)
  dataset$landing<-as.numeric(dataset$landing)
  dataset$claim<-as.numeric(dataset$claim)
  dataset$harvest<-as.numeric(dataset$harvest)
  dataset$release<-as.numeric(dataset$release)
  dataset$wgt_ab1<-as.numeric(dataset$wgt_ab1)
  dataset$wp_int<-as.numeric(dataset$wp_int)
  dataset$dcomm<-common
  dataset$dtotcat<-ifelse(dataset$common==common,dataset$tot_cat,0)
  dataset$dlandings<-ifelse(dataset$common==common,dataset$landing,0)
  dataset$dclaim<-ifelse(dataset$common==common,dataset$claim,0)
  dataset$dharvest<-ifelse(dataset$common==common,dataset$harvest,0)
  dataset$drelease<-ifelse(dataset$common==common,dataset$release,0)
  dataset$dwgt_ab1<-ifelse(dataset$common==common,dataset$wgt_ab1,0)
  temlab=NULL
  cnt=0
  for(i in 1:5){
    if(any(trips==i)){  
      if(i==1){
        cnt=cnt+1
        if(cnt==1) temlab=paste(temlab,"dataset$prim1_common==common",sep="")
      }  
      if(i==2){
        cnt=cnt+1
        if(cnt==1) temlab=paste(temlab,"dataset$prim2_common==common",sep="")
        if(cnt>1) temlab=paste(temlab,"dataset$prim2_common==common",sep=" & ")
      }
      if(i==3){
        cnt=cnt+1
        if(cnt==1) temlab=paste(temlab,"dataset$common==common & as.numeric(dataset$claim)>0",sep="")
        if(cnt>1) temlab=paste(temlab,"dataset$common==common & as.numeric(dataset$claim)>0",sep=" & ")
      }
      if(i==4){
        cnt=cnt+1
        if(cnt==1) temlab=paste(temlab,"dataset$common==common & as.numeric(dataset$harvest)>0",sep="")
        if(cnt>1) temlab=paste(temlab,"dataset$common==common & as.numeric(dataset$harvest)>0",sep=" & ")
      }
      if(i==5){
        cnt=cnt+1
        if(cnt==1) temlab=paste(temlab,"dataset$common==common & as.numeric(dataset$release)>0",sep="")
        if(cnt>1) temlab=paste(temlab,"dataset$common==common & as.numeric(dataset$release)>0",sep=" & ")
      }
    }
  }
  texter1<-paste("ifelse(",temlab,",","dataset$dom_id,2)",sep="")
   eval(parse(text=paste("dataset$dom_id<-",texter1,sep="")))
  dataset1<-stats::aggregate(cbind(dataset$dtotcat,dataset$dlandings,dataset$dclaim,dataset$dharvest,
    dataset$drelease,dataset$dwgt_ab1)
    ,list(dataset$strat_id,
    dataset$psu_id,dataset$id_code,dataset$wp_int,dataset$prim1_common,dataset$prim2_common,dataset$dcomm,dataset$dom_id),sum)
    names(dataset1)<-c("strat_id","psu_id","id_code","wp_int","prim1_common","prim2_common","common","dom_id_add","total.catch",
    "harvest.A.B1","claim.A","reported.B1","released.B2","weight")

  dataset1$dtrip<-1
  dfpc<-survey::svydesign(ids=~psu_id,strata=~strat_id,
   weights=~wp_int,nest=TRUE,data=dataset1)
   options(survey.lonely.psu = "certainty")
   results<-survey::svyby(~dtrip,
     ~dom_id_add,dfpc,survey::svytotal,vartype=c("se","cv"),keep.names=FALSE) 
  names(results)<-c("Domain","Trips","SE","PSE")
   if(length(grep("DELETE",results$Domain,fixed=TRUE))>0){
     results<-results[-c(grep("DELETE",results$Domain,fixed=TRUE)),]
   }
  results<-results[results$Domain!="2",]
   return(results[order(results$Domain),])
}
