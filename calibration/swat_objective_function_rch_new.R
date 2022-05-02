source("https://raw.githubusercontent.com/Rojakaveh/VSA_Calibration/main/new_alter_files_fn/alter_files.R")
library(SWATmodel)
flowgage_id="04282650" #Little Otter Creek at Ferrisburg, VT.
flowgage=get_usgs_gage(flowgage_id,begin_date = "2010-01-01",end_date= "2022-01-01")
flowgage$flowdata$Qmm=(flowgage$flowdata$flow)/(flowgage$area*1000)
######remove extreme datapoint
#max(flowgage$flowdata$Qmm)
#flowgage$flowdata=subset(flowgage$flowdata, flowgage$flowdata$Qmm<18)


dir.create("/dev/shm/rojakaveh")
setwd("/dev/shm/rojakaveh")
dir.create("locfer2010")
file.copy(list.files("~/SWATplusR/txtinout/",full.names = TRUE,recursive = TRUE),"/dev/shm/rojakaveh/locfer2010/",recursive = TRUE)
####load updated functions
#########################################
#######in basin.bsn ICRK==1
########################################
setwd("locfer2010/")
#setwd("CrackFlowFer2015/")

pacman::p_load(httr,EcoHydRology,curl,GSODR,data.table,DEoptim,tidyverse)

change_params=""
rm(change_params)
load(paste(path.package("EcoHydRology"), "data/change_params.rda", sep = "/"))
##############################################################
#here we have list of parameters, their file types, ranges, alter_type,.....
#firts modify alter_files function/ define the new alter type for CN as additive: my github 
#https://raw.githubusercontent.com/Rojakaveh/VSA_Calibration/main/new_alter_files_fn/alter_files.R
#then we should modify change_params dataframe to modify alter type of CN to additive
###########################################################
levels(change_params$alter_type) <- c("new", "percent","additive")
change_params[19,3]="additive"
change_params[19,4]=-0.2
change_params[19,5]=0.2
change_params[19,6]=0.1
##############
library(SWATmodel)
#########################################################
#  Load updated functions
source("https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/SWATmodel/R/readSWAT.R?root=ecohydrology")
save(readSWAT,file="readSWAT.R")
source("https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/EcoHydRology/R/setup_swatcal.R?root=ecohydrology")
source("https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/EcoHydRology/R/swat_objective_function_rch.R?root=ecohydrology")
calib_range=c("1999-12-31","2021-12-31")

######################################
#####here we should enter the parameters that are sensitive (not all of the params that are below) !!!be carful
########################################
params_select=c(1,2,3,6,7,8,9,10,11,14,19,21,23,24,32,33)
calib_params=change_params[params_select,]
View(calib_params)
calib_params$min[1]=0
calib_params$min[2]=0
calib_params$max[2]=1
calib_params$current[2]=0.5
calib_params$max[3]=600

calib_params$min[7]=0
calib_params$min[8]=0
calib_params$current[7]=2.5
calib_params$current[8]=2.5
calib_params$min[9]=0.01
calib_params$max[9]=1
calib_params$max[12]=3
calib_params$min[12]=0.5
calib_params$current[13]=2.25
calib_params$max[13]=2.5
calib_params$min[13]=2
calib_params$min[14]=0.5
calib_params$max[14]=3
calib_params$min[16]=0.1



calib_params[1:7]
setup_swatcal(calib_params)
rch=1

# Test calibration
x=calib_params$current

swat_objective_function_rch_new=function (x, calib_range, calib_params, flowgage, rch,save_results=F)
{
  pacman::p_load(SWATmodel,dplyr,EcoHydRology,topmodel,utils)
  calib_params$current <- x
  tmpdir=as.character(as.integer((runif(1)+1)*10000))
  tmpdir=paste(c(format(Sys.time(), "%s"),tmpdir,Sys.getpid()),sep="",collapse="")
  print(tmpdir)
  dir.create(tmpdir)
  file.copy(list.files(),tmpdir)
  setwd(tmpdir)
  file.remove(list.files(pattern="output."))
  alter_filesfn(calib_params)
  libarch = if (nzchar(base::version$arch)) paste("libs", base::version$arch, sep = "/") else "libs"
  swatbin <- "rswat2012.exe"
  junkout=system(shQuote(paste(path.package("SWATmodel"), libarch, swatbin, sep = "/")),intern = TRUE)
  start_year = read.fortran(textConnection(readLines("file.cio")[9]), "f20")
  load("readSWAT.R")
  outdata = readSWAT("rch",".")
  outdata$FLOW_OUTmm=(outdata$FLOW_OUTcms*24*3600)/(outdata$AREAkm2*1000)
  test2 = subset(outdata, outdata$RCH == rch)
  test3 = merge(flowgage$flowdata, test2, all = F)
  NS = topmodel::NSeff(test3$Qmm, test3$FLOW_OUTmm)
  print(NS)
  if(save_results){file.copy(list.files(),"../")}
  file.remove(list.files())
  setwd("../")
  file.remove(tmpdir)
  return(abs(NS - 1))
}
install.packages("EcoHydRology", repos="http://R-Forge.R-project.org")
library(EcoHydRology)

swat_objective_function_rch_new(x,calib_range,calib_params,flowgage,rch,save_results=F)

cl <- parallel::makeCluster(16)
outDEoptimID2010<-DEoptim(swat_objective_function_rch_new,calib_params$min,calib_params$max,
                          DEoptim.control(cluster=cl,strategy = 6,NP = 16,itermax=10,parallelType = 1,
                                          packages = c("SWATmodel","dplyr","EcoHydRology","base","topmodel","utils","cl"),parVar=c("alter_filesfn","%<%","NSeff","read.fortran","readSWAT")),calib_range,calib_params,flowgage,rch)
