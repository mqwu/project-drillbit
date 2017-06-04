
#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
#setwd("Z:/project/DrillBit/")
setwd('C:/Apps/projects/DrillBit/')

#--------------------------------------------------------
# Libs and tools
#-------------------------------------------------------- 
rm(list=ls(all=TRUE))  # clean memory
library(tidyverse)
library(readxl)


#--------------------------------------------------------
# Paths
#-------------------------------------------------------- 
header_path = "./data/column_headers2.xlsx" 
d1_path = "./data/20well_40 bit runs_csv/ALLEN-55-2-44-E1H.csv"
d2_path = "./data/20well_40 bit runs_csv/Bernard-C-27-2-1H.csv"
d3_path = "./data/20well_40 bit runs_csv/ODELL-54-1-6-2H.csv"
d4_path = "./data/20well_40 bit runs_csv/Roan53-2-2002H.csv"
d5_path = "./data/20well_40 bit runs_csv/Roan53-2-2004H.csv"
d6_path = "./data/20well_40 bit runs_csv/Roan53-2-2005H.csv"
d7_path = "./data/20well_40 bit runs_csv/Roan53-2-2006H.csv"
d8_path = "./data/20well_40 bit runs_csv/Roan53-2-2007H.csv"
d9_path = "./data/20well_40 bit runs_csv/Roan53-2LOV1901H.csv"
d10_path = "./data/20well_40 bit runs_csv/Roan53-2LOV1902H.csv"
d11_path = "./data/20well_40 bit runs_csv/Roan53-2LOV1903H.csv"
d12_path = "./data/20well_40 bit runs_csv/University19-2503H.csv"
d13_path = "./data/20well_40 bit runs_csv/University19-2504H.csv"
d14_path = "./data/20well_40 bit runs_csv/University19A2303H.csv"
d15_path = "./data/20well_40 bit runs_csv/University19A2304H.csv"
d16_path = "./data/20well_40 bit runs_csv/University19-U2-2505.csv"
d17_path = "./data/20well_40 bit runs_csv/University19-U2-2506.csv"
d18_path = "./data/20well_40 bit runs_csv/University20-1503H.csv"
d19_path = "./data/20well_40 bit runs_csv/University20-2003H.csv"
d20_path = "./data/20well_40 bit runs_csv/University20-2004H.csv"


#--------------------------------------------------------
# Load and processing data
#-------------------------------------------------------- 
header <- read_excel(header_path)

for (i in 1:20){
  path <- paste0("d", i,"_path")
  d.frame <- paste0("d", i)
  assign(d.frame, setNames(read.csv(eval(as.name(path)), header=F), names(header)) )
}

all <- list(d1=d1,
            d2=d2,
            d3=d3,
            d4=d4,
            d5=d5,
            d6=d6,
            d7=d7,
            d8=d8,
            d9=d9,
            d10=d10,
            d11=d11,
            d12=d12,
            d13=d13,
            d14=d14,
            d15=d15,
            d16=d16,
            d17=d17,
            d18=d18,
            d19=d19,
            d20=d20
)

#saveRDS(all, "./data/all.rds")

dat <- lapply(all, function(dat){
  dat %>%
    mutate(calWF=ifelse(SWOB*SROP*DRPM==0,0,0.00875*SWOB/(0.2*SROP/DRPM))) %>%   # TDiff: time gap; calWF: calculated WF
    filter(WF>=0, WF<=100, Flow>10, SWOB>1, SRPM>5, SPP>10, abs(BitDp-HoleDp)<=0.2) %>%
    mutate(TDiff=Time-lag(Time))
}
)
#saveRDS(dat, "./data/dat.rds")
#dat <- readRDS("./data/dat.rds")

# x20 <- dat$d20
# summary(x20)
# 
# plot_ly(dat$d20, x=~Time, y=~log(WF), type='scatter', mode='lines+markers')
