
#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
setwd("Z:/project/DrillBit/")


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
#all <- readRDS("./data/all.rds")

# filter data during drilling activity
dat <- lapply(all, function(dat){
  dat %>% 
    mutate(calWF=ifelse(SWOB*SROP*DRPM==0,0,0.00875*SWOB/(0.2*SROP/DRPM))) %>%   # TDiff: time gap; calWF: calculated WF
    filter(WF>=0, WF<=100, Flow>10, SWOB>1, SRPM>5, SPP>10, abs(BitDp-HoleDp)<=0.2) %>% 
    mutate(TDiff=Time-lag(Time))
  }
)
#saveRDS(dat, "./data/dat.rds")
#dat <- readRDS("./data/dat.rds")


# derive data for each run
bit <- list(r1=cbind(dat$d1, flag=0), #ALLEN-55-2-44-E1H.R1
            r2=cbind(dat$d2 %>% filter(Time>10), flag=0), #Bernard-C-27-2-1H.R1
            r3=cbind(dat$d3 %>% filter(Time<40), flag=0), #ODELL-54-1-6-2H.R1
            r4=cbind(dat$d3 %>% filter(Time>40), flag=1), #ODELL-54-1-6-2H.R2
            r5=cbind(dat$d4 %>% filter(Time>30, Time<60), flag=0), # Roan53-2-2002H.R1
            r6=cbind(dat$d4 %>% filter(Time<30), flag=1), # Roan53-2-2002H.R2
            r7=cbind(dat$d5, flag=1), # Roan53-2-2004H.R1
            r8=cbind(dat$d6 %>% filter(Time>13, Time<50), flag=1), # Roan53-2-2005H.R1
            r9=cbind(dat$d6 %>% filter(Time>50), flag=0), # Roan53-2-2005H.R2
            r10=cbind(dat$d7 %>% filter(Time>40), flag=1), # Roan53-2-2006H.R1
            r11=cbind(dat$d7 %>% filter(Time<40), flag=1), # Roan53-2-2006H.R2
            r12=cbind(dat$d8 %>% filter(Time<40), flag=1), # Roan53-2-2007H.R1
            r13=cbind(dat$d9 %>% filter(Time<30), flag=1), # Roan53-2LOV1901H.R1
            r14=cbind(dat$d9 %>% filter(Time>40), flag=0), # Roan53-2LOV1901H.R2
            r15=cbind(dat$d10 %>% filter(Time<60), flag=0), # Roan53-2LOV1902H.R2
            r16=cbind(dat$d10 %>% filter(Time>60), flag=0), # Roan53-2LOV1902H.R1
            r17=cbind(dat$d11 %>% filter(Time>25), flag=0), # Roan53-2LOV1903H.R1
            r18=cbind(dat$d12 %>% filter(Time<60), flag=0), # University19-2503H.R1
            r19=cbind(dat$d13 %>% filter(Time<70), flag=0), # University19-2504H.R1
            r20=cbind(dat$d13 %>% filter(Time>70), flag=0), # University19-2504H.R2
            r21=cbind(dat$d14 %>% filter(Time<40), flag=1), # University19A2303H.R1
            r22=cbind(dat$d14 %>% filter(Time>40), flag=1), # University19A2303H.R2
            r23=cbind(dat$d15 %>% filter(Time<60), flag=1), # University19A2304H.R1
            r24=cbind(dat$d15 %>% filter(Time>80), flag=0), # University19A2304H.R2
            r25=cbind(dat$d16 %>% filter(Time<60), flag=0), # University19-U2-2505.R1
            r26=cbind(dat$d16 %>% filter(Time>60), flag=0), # University19-U2-2505.R2
            r27=cbind(dat$d17 %>% filter(Time<60), flag=0), # University19-U2-2506.R1
            r28=cbind(dat$d17 %>% filter(Time>60), flag=0), # University19-U2-2506.R2
            r29=cbind(dat$d18 %>% filter(Time<50), flag=0), # University20-1503H.R1
            r30=cbind(dat$d18 %>% filter(Time>50, Time<80), flag=1), # University20-1503H.R2
            r31=cbind(dat$d18 %>% filter(Time>80), flag=0), # University20-1503H.R3
            r32=cbind(dat$d19 %>% filter(Time<40), flag=0), # University20-2003H.R1
            r33=cbind(dat$d19 %>% filter(Time>40, Time<55), flag=0), # University20-2003H.R3
            r34=cbind(dat$d19 %>% filter(Time>55, Time<80), flag=0), # University20-2003H.R2
            r35=cbind(dat$d20 %>% filter(Time<30), flag=0), # University20-2004H.R1
            r36=cbind(dat$d20 %>% filter(Time>30), flag=1) # University20-2004H.R2
)
