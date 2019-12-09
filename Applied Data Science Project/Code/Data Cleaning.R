rm(list=ls())
install.packages("tidyverse")
install.packages("janitor")
install.packages("reprex")
install.packages("rio")
install.packages("tidyr")
install.packages("xlsx")
library("reprex")
library("tidyr")
library("tidyverse")
library(readxl)
library(xlsx)

# Import MIT Data ---------------------------------------------------------
RS <<- read.csv("Data/Results/1976-2018-senate.csv")
RS <<- RS%>% filter(candidate!="") %>%
  filter(special=="FALSE") %>%
  filter(stage=="gen")%>%
  filter(writein=="FALSE")
RS <<- RS[ -c(2,4,5,6,8,7,9,10,14,17,18)]
RS <<- RS%>%mutate(state=state_po)
RS <<- RS[ -c(2)]
RS <<- RS[,c(1,7,3,2,4,5,6)]
# Import FEC Data ---------------------------------------------------------------
F90S <<- read_excel("Data/Fundraising/1990_Senate.xlsx", skip=3)
F92S <<- read_excel("Data/Fundraising/1992_Senate.xlsx", skip=3)
F94S <<- read_excel("Data/Fundraising/1994_Senate.xlsx", skip=3)
F96S <<- read_excel("Data/Fundraising/1996_Senate.xlsx", skip=3)
F98S <<- read_excel("Data/Fundraising/1998_Senate.xlsx", skip=3)
F00S <<- read_excel("Data/Fundraising/2000_Senate.xlsx", skip=3)
F02S <<- read_excel("Data/Fundraising/2002_Senate.xlsx", skip=3)
F04S <<- read_excel("Data/Fundraising/2004_Senate.xlsx", skip=3)
F06S <<- read_excel("Data/Fundraising/2006_Senate.xlsx", skip=3)
F08S <<- read_excel("Data/Fundraising/2008_Senate.xlsx", skip=3)
F10S <<- read_excel("Data/Fundraising/2010_Senate.xlsx", skip=3)
# Correct Row Names ---------------------------------------------------------------
janitor::row_to_names(F90S,1, remove_row = TRUE)
janitor::row_to_names(F92S,1, remove_row = TRUE)
janitor::row_to_names(F94S,1, remove_row = TRUE)
janitor::row_to_names(F96S,1, remove_row = TRUE)
janitor::row_to_names(F98S,1, remove_row = TRUE)
janitor::row_to_names(F00S,1, remove_row = TRUE)
janitor::row_to_names(F02S,1, remove_row = TRUE)
janitor::row_to_names(F04S,1, remove_row = TRUE)
janitor::row_to_names(F06S,1, remove_row = TRUE)
janitor::row_to_names(F08S,1, remove_row = TRUE)
janitor::row_to_names(F10S,1, remove_row = TRUE)
# Create a function to look up state postal codes-------------------------------------------------------------------------
States <<- read.csv("Data/States.csv")
get_state_abb <<- function(x){
  a<-States%>%filter(State==x)
  a<-a[-c(1)]
  a<-a[[1]]
  return(a)
}

States <<- read.csv("../Data/States.csv")
get_state_abb <<- States$Abbreviation
names(get_state_abb) <<- States$State

# Change State Names to Abbreviations ---------------------------------------------------------------
F90S <<- F90S%>%mutate(state=state.abb[match(F90S$State,state.name)])
F92S <<- F92S %>%mutate(state=state.abb[match(F92S$State,state.name)])
F94S <<- F94S %>%mutate(state=state.abb[match(F94S$State,state.name)])
F96S <<- F96S %>%mutate(state=state.abb[match(F96S$State,state.name)])
F98S <<- F98S %>%mutate(state=state.abb[match(F98S$State,state.name)])
F00S <<- F00S %>%mutate(state=state.abb[match(F00S$State,state.name)])
F02S <<- F02S %>%mutate(state=state.abb[match(F02S$State,state.name)])
F04S <<- F04S %>%mutate(state=state.abb[match(F04S$State,state.name)])
F06S <<- F06S %>%mutate(state=state.abb[match(F06S$State,state.name)])
F08S <<- F08S %>%mutate(state=state.abb[match(F08S$State,state.name)])
F10S <<- F10S %>%mutate(state=state.abb[match(F10S$State,state.name)])
# Obtaining Candidate totals ----------------------------------------------
# Create new Tables grouped by candidate
C90S <<- F90S %>%group_by(Candidate)
C92S <<- F92S %>%group_by(Candidate)
C94S <<- F94S %>%group_by(Candidate)
C96S <<- F96S %>%group_by(Candidate)
C98S <<- F98S %>%group_by(Candidate)
C00S <<- F00S %>%group_by(Candidate)
C02S <<- F02S %>%group_by(Candidate)
C04S <<- F04S %>%group_by(Candidate)
C06S <<- F06S %>%group_by(Candidate)
C08S <<- F08S %>%group_by(Candidate)
C10S <<- F10S %>%group_by(Candidate)
# Remove Unnecessary Columns
C90S<<-C90S[ -c(1,2,6,7,8,9,10,11,12,13,14)]
C92S<<-C92S[ -c(1,2,6,7,8,9,10,11,12,13,14)]
C94S<<-C94S[ -c(1,2,6,7,8,9,10,11,12,13,14)]
C96S<<-C96S[ -c(1,2,6,7,8,9,10,11,12,13,14)]
C98S<<-C98S[ -c(1,2,6,7,8,9,10,11,12,13,14)]
C00S<<-C00S[ -c(1,2,6,7,8,9,10,11,12,13,14)]
C02S<<-C02S[ -c(1,2,6,7,8,9,10,11,12,13,14)]
C04S<<-C04S[ -c(1,2,6,7,8,9,10,11,12,13,14)]
C06S<<-C06S[ -c(1,2,6,7,8,9,10,11,12,13,14)]
C08S<<-C08S[ -c(1,2,6,7,8,9,10,11,12,13,14)]
C10S<<-C10S[ -c(1,2,6,7,8,9,10,11,12,13,14)]
# Get Fundraising totals for candidates
F90S <<- F90S %>%
  group_by(Candidate) %>%
  summarise(
    raised=sum(Receipts),
    spent=sum(Disbursements))%>%
  mutate(year=1990)
F92S <<- F92S %>%
  group_by(Candidate) %>%
  summarise(
    raised=sum(Receipts),
    spent=sum(Disbursements))%>%
  mutate(year=1992)
F94S <<- F94S %>%
  group_by(Candidate) %>%
  summarise(
    raised=sum(Receipts),
    spent=sum(Disbursements))%>%
  mutate(year=1994)
F96S <<- F96S %>%
  group_by(Candidate) %>%
  summarise(
    raised=sum(Receipts),
    spent=sum(Disbursements))%>%
  mutate(year=1996)
F98S <<- F98S %>%
  group_by(Candidate) %>%
  summarise(
    raised=sum(Receipts),
    spent=sum(Disbursements))%>%
  mutate(year=1998)
F00S <<- F00S %>%
  group_by(Candidate) %>%
  summarise(
    raised=sum(Receipts),
    spent=sum(Disbursements))%>%
  mutate(year=2000)
F02S <<- F02S %>%
  group_by(Candidate) %>%
  summarise(
    raised=sum(Receipts),
    spent=sum(Disbursements))%>%
  mutate(year=2002)
F04S <<- F04S %>%
  group_by(Candidate) %>%
  summarise(
    raised=sum(Receipts),
    spent=sum(Disbursements)) %>%
  mutate(year=2004)
F06S <<- F06S %>%
  group_by(Candidate) %>%
  summarise(
    raised=sum(Receipts),
    spent=sum(Disbursements))%>%
  mutate(year=2006)
F08S <<- F08S %>%
  group_by(Candidate) %>%
  summarise(
    raised=sum(Receipts),
    spent=sum(Disbursements))%>%
  mutate(year=2008)
F10S <<- F10S %>%
  group_by(Candidate) %>%
  summarise(
    raised=sum(Receipts),
    spent=sum(Disbursements))%>%
  mutate(year=2010)
# Join tibbles
F90S<<-inner_join(F90S,C90S)
F92S<<-inner_join(F92S,C92S)
F94S<<-inner_join(F94S,C94S)
F96S<<-inner_join(F96S,C96S)
F98S<<-inner_join(F98S,C98S)
F00S<<-inner_join(F00S,C00S)
F02S<<-inner_join(F02S,C02S)
F04S<<-inner_join(F04S,C04S)
F06S<<-inner_join(F06S,C06S)
F08S<<-inner_join(F08S,C08S)
F10S<<-inner_join(F10S,C10S)
# Combine Fundraising Lists
FS1<<-full_join(F90S,F92S)
FS2<<-full_join(FS1,F94S)
FS3<<-full_join(FS2,F96S)
FS4<<-full_join(FS3,F98S)
FS5<<-full_join(FS4,F00S)
FS6<<-full_join(FS5,F02S)
FS7<<-full_join(FS6,F04S)
FS8<<-full_join(FS7,F06S)
FS9<<-full_join(FS8,F08S)
FS<<-full_join(FS9,F10S)
# Standardizing candidate names -------------------------------------------------------------------
FS <<- FS %>%mutate(last_name=Candidate)
FS <<- FS %>%mutate(first_name=Candidate)
FS <<- FS %>%mutate(first_name=sub('.*,\\s*','', FS$first_name))
FS <<- FS %>%mutate(last_name=sub('\\s*,.*','', FS$last_name))
FS <<- FS %>%mutate(first_name=sub("\\s+\\S+$", '', FS$first_name))
FS <<- FS %>%mutate(first_name=sub("\\s+\\S+$", '', FS$first_name))
FS <<- FS %>%mutate(first_name=sub("\\s+\\S+$", '', FS$first_name))
FS <<- FS %>%mutate(first_name=sub("\\s+\\S+$", '', FS$first_name))
FS$candidate <<- paste(FS$first_name,FS$last_name)
FS <<- FS[ -c(1)]
FS <<- FS[,c(3,6,4,9,5,1,2,8,7)]
#Save this as a new tibble to save time
FSN<<-FS
# Standardizing Party Names ------------------------------------------------
FS <<- FS %>%
  mutate(Party=sub(" Party", '', FS$Party))%>%
  mutate(party=Party)
FS <<- FS[ -c(3)]
FS <<- FS[,c(1,2,9,3,4,5,6,7,8)]
FS <<- FS %>%mutate(party=sub("Democratic-Farm-Labor", 'democrat', FS$party))
FS <<- FS %>%
  mutate(party=sub("Democratic", 'democrat', FS$party))%>%
  mutate(party=tolower(party))
#Some Duplicates were created during this process, likely due to running joins more than once. The distinct code will remove these
FS <<- distinct(FS)
FS <<- FS[,c(1,2,3,9,5,6,7,8)]
# Results Name Matching ---------------------------------------------------
RS <<- distinct(RS)
RSALL<<-RS
RSR <<-RS%>%filter(party =="republican")
RSD <<-RS%>%filter(party =="democrat")
RS<<-full_join(RSR,RSD)
RS<<-RS%>%
  filter(year >=1990)%>%
  filter(year <=2010)
RS<<-RS%>%mutate(percentage=candidatevotes/totalvotes)
RS<<-RS%>%filter(percentage >=.1)
RS<<-RS%>%filter(percentage <=.9)
#Create new column with candidate last names
RS <<- RS %>%mutate(last_name=candidate)
RS <<- RS %>%mutate(last_name=sub(".*? ", "", RS$last_name))
RS <<- RS %>%mutate(last_name=sub(".*? ", "", RS$last_name))
RS <<- RS %>%mutate(last_name=sub(".*? ", "", RS$last_name))
RS <<- RS %>%mutate(last_name=sub(".*? ", "", RS$last_name))
RS <<- RS %>%mutate(last_name=sub(".*? ", "", RS$last_name))
RS <<- RS %>%mutate(last_name=sub(".*? ", "", RS$last_name))
RS <<- RS %>%mutate(last_name=toupper(last_name))
#Remove un-necessary and redundant columns
RS <<- RS[,c(1,2,3,9,4,5,6,7,8)]
# Remove states with jungle primaries
RSNJ<<-RS%>%
  filter(state !="AK")%>%
  filter(state !="WA")%>%
  filter(state !="CA")%>%
  filter(state !="LA")
Senate1<<-left_join(RSNJ,FS)
Senate1<<-distinct(Senate1)
# Add Pervious Election Data ----------------------------------------------
RSREF<<-full_join(RSR,RSD)
RSREF<<-RSREF%>%mutate(previous_percentage=candidatevotes/totalvotes)
RSREF<<-RSREF%>%
  filter(state !="AK")%>%
  filter(state !="WA")%>%
  filter(state !="CA")%>%
  filter(state !="LA")
RSREF<<-RSREF[ -c(4,5,6,7)]
RSREF<<-RSREF%>%mutate(year=year+6)
Senate2<<-left_join(Senate1,RSREF)
# Calculate Demonstrated PVI's from previous results and current r
Senate3 <<- Senate2%>%
  mutate(Previous_RPVI=previous_percentage-.5)%>%
  mutate(Previous_RPVI=2*Previous_RPVI)%>%
  mutate(RPVI=percentage-.5)%>%
  mutate(RPVI=2*RPVI)
Senate3<<-unique(Senate3)
Senate3<<-Senate3[ -c(6,13)]
# Re-divide fundraising into years ---------------------------------------
R90S<<-Senate3%>%filter(year==1990)
R92S<<-Senate3%>%filter(year==1992)
R94S<<-Senate3%>%filter(year==1994)
R96S<<-Senate3%>%filter(year==1996)
R98S<<-Senate3%>%filter(year==1998)
R00S<<-Senate3%>%filter(year==2000)
R02S<<-Senate3%>%filter(year==2002)
R04S<<-Senate3%>%filter(year==2004)
R06S<<-Senate3%>%filter(year==2006)
R08S<<-Senate3%>%filter(year==2008)
R10S<<-Senate3%>%filter(year==2010)
# Group By Candidate again ------------------------------------------------
C90S <<- R90S %>%
  group_by(candidate)%>%
  summarise(
    rcpt=sum(raised),
    disb=sum(spent))
C92S <<- R92S %>%
  group_by(candidate)%>%
  summarise(
    rcpt=sum(raised),
    disb=sum(spent))
C94S <<- R94S %>%
  group_by(candidate)%>%
  summarise(
    rcpt=sum(raised),
    disb=sum(spent))
C96S <<- R96S %>%
  group_by(candidate)%>%
  summarise(
    rcpt=sum(raised),
    disb=sum(spent))
C98S <<- R98S %>%
  group_by(candidate)%>%
  summarise(
    rcpt=sum(raised),
    disb=sum(spent))
C00S <<- R00S %>%
  group_by(candidate)%>%
  summarise(
    rcpt=sum(raised),
    disb=sum(spent))
C02S <<- R02S %>%
  group_by(candidate)%>%
  summarise(
    rcpt=sum(raised),
    disb=sum(spent))
C04S <<- R04S %>%
  group_by(candidate)%>%
  summarise(
    rcpt=sum(raised),
    disb=sum(spent))
C06S <<- R06S %>%
  group_by(candidate)%>%
  summarise(
    rcpt=sum(raised),
    disb=sum(spent))
C08S <<- R08S %>%
  group_by(candidate)%>%
  summarise(
    rcpt=sum(raised),
    disb=sum(spent))
C10S <<- R10S %>%
  group_by(candidate)%>%
  summarise(
    rcpt=sum(raised),
    disb=sum(spent))
# Re-join with results ----------------------------------------------------
R90S<<-left_join(R90S,C90S)
R92S<<-left_join(R92S,C92S)
R94S<<-left_join(R94S,C94S)
R96S<<-left_join(R96S,C96S)
R98S<<-left_join(R98S,C98S)
R00S<<-left_join(R00S,C00S)
R02S<<-left_join(R02S,C02S)
R04S<<-left_join(R04S,C04S)
R06S<<-left_join(R06S,C06S)
R08S<<-left_join(R08S,C08S)
R10S<<-left_join(R10S,C10S)
# Re-name fundraising columns ---------------------------------------------
R90S<<-R90S%>%
  mutate(raised=rcpt)%>%
  mutate(spent=disb)
R92S<<-R92S%>%
  mutate(raised=rcpt)%>%
  mutate(spent=disb)
R94S<<-R94S%>%
  mutate(raised=rcpt)%>%
  mutate(spent=disb)
R96S<<-R96S%>%
  mutate(raised=rcpt)%>%
  mutate(spent=disb)
R98S<<-R98S%>%
  mutate(raised=rcpt)%>%
  mutate(spent=disb)
R00S<<-R00S%>%
  mutate(raised=rcpt)%>%
  mutate(spent=disb)
R02S<<-R02S%>%
  mutate(raised=rcpt)%>%
  mutate(spent=disb)
R04S<<-R04S%>%
  mutate(raised=rcpt)%>%
  mutate(spent=disb)
R06S<<-R06S%>%
  mutate(raised=rcpt)%>%
  mutate(spent=disb)
R08S<<-R08S%>%
  mutate(raised=rcpt)%>%
  mutate(spent=disb)
R10S<<-R10S%>%
  mutate(raised=rcpt)%>%
  mutate(spent=disb)
# Remove un-necessary columns ---------------------------------------------
R90S<<-R90S[ -c(15,16)]
R92S<<-R92S[ -c(15,16)]
R94S<<-R94S[ -c(15,16)]
R96S<<-R96S[ -c(15,16)]
R98S<<-R98S[ -c(15,16)]
R00S<<-R00S[ -c(15,16)]
R02S<<-R02S[ -c(15,16)]
R04S<<-R04S[ -c(15,16)]
R06S<<-R06S[ -c(15,16)]
R08S<<-R08S[ -c(15,16)]
R10S<<-R10S[ -c(15,16)]
# remove duplicate data ---------------------------------------------------
R90S<<-unique(R90S)
R92S<<-unique(R92S)
R94S<<-unique(R94S)
R96S<<-unique(R96S)
R98S<<-unique(R98S)
R00S<<-unique(R00S)
R02S<<-unique(R02S)
R04S<<-unique(R04S)
R06S<<-unique(R06S)
R08S<<-unique(R08S)
R10S<<-unique(R10S)
# Get totals for each state -----------------------------------------------
#Create new tibbles for each year
S90S<<-R90S
S92S<<-R92S
S94S<<-R94S
S96S<<-R96S
S98S<<-R98S
S00S<<-R00S
S02S<<-R02S
S04S<<-R04S
S06S<<-R06S
S08S<<-R08S
S10S<<-R10S
# Get totals for each state
S90S <<- S90S %>%
  group_by(state) %>%
  summarise(
    total_raised=sum(raised),
    total_spent=sum(spent))
S92S <<- S92S %>%
  group_by(state) %>%
  summarise(
    total_raised=sum(raised),
    total_spent=sum(spent))
S94S <<- S94S %>%
  group_by(state) %>%
  summarise(
    total_raised=sum(raised),
    total_spent=sum(spent))
S96S <<- S96S %>%
  group_by(state) %>%
  summarise(
    total_raised=sum(raised),
    total_spent=sum(spent))
S98S <<- S98S %>%
  group_by(state) %>%
  summarise(
    total_raised=sum(raised),
    total_spent=sum(spent))
S00S <<- S00S %>%
  group_by(state) %>%
  summarise(
    total_raised=sum(raised),
    total_spent=sum(spent))
S02S <<- S02S %>%
  group_by(state) %>%
  summarise(
    total_raised=sum(raised),
    total_spent=sum(spent))
S04S <<- S04S %>%
  group_by(state) %>%
  summarise(
    total_raised=sum(raised),
    total_spent=sum(spent))
S06S <<- S06S %>%
  group_by(state) %>%
  summarise(
    total_raised=sum(raised),
    total_spent=sum(spent))
S08S <<- S08S %>%
  group_by(state) %>%
  summarise(
    total_raised=sum(raised),
    total_spent=sum(spent))
S10S <<- R10S %>%
  group_by(state)%>%
  summarise(
    total_raised=sum(raised),
    total_spent=sum(spent))
#Remove duplicates
S90S <<- distinct(S90S)
S92S <<- distinct(S92S)
S94S <<- distinct(S94S)
S96S <<- distinct(S96S)
S98S <<- distinct(S98S)
S00S <<- distinct(S00S)
S02S <<- distinct(S02S)
S04S <<- distinct(S04S)
S06S <<- distinct(S06S)
S08S <<- distinct(S08S)
S10S <<- distinct(S10S)
#Join tibbles
R90S<<-left_join(R90S,S90S)
R92S<<-left_join(R92S,S92S)
R94S<<-left_join(R94S,S94S)
R96S<<-left_join(R96S,S96S)
R98S<<-left_join(R98S,S98S)
R00S<<-left_join(R00S,S00S)
R02S<<-left_join(R02S,S02S)
R04S<<-left_join(R04S,S04S)
R06S<<-left_join(R06S,S06S)
R08S<<-left_join(R08S,S08S)
R10S<<-left_join(R10S,S10S)
# Two candidates only -----------------------------------------------------
#Create new tibbles to count number of candidates in each state
CN90S<<-R90S%>% count(state)
CN92S<<-R92S%>% count(state)
CN94S<<-R94S%>% count(state)
CN96S<<-R96S%>% count(state)
CN98S<<-R98S%>% count(state)
CN00S<<-R00S%>% count(state)
CN02S<<-R02S%>% count(state)
CN04S<<-R04S%>% count(state)
CN06S<<-R06S%>% count(state)
CN08S<<-R08S%>%count(state)
CN10S<<-R10S%>%count(state)
#Join number of candidates column
R90S<<-left_join(R90S,CN90S)
R92S<<-left_join(R92S,CN92S)
R94S<<-left_join(R94S,CN94S)
R96S<<-left_join(R96S,CN96S)
R98S<<-left_join(R98S,CN98S)
R00S<<-left_join(R00S,CN00S)
R02S<<-left_join(R02S,CN02S)
R04S<<-left_join(R04S,CN04S)
R06S<<-left_join(R06S,CN06S)
R08S<<-left_join(R08S,CN08S)
R10S<<-left_join(R10S,CN10S)
#Only look at states with a republican and a democrat running
R90S<<-R90S%>%filter(n==2)
R92S<<-R92S%>%filter(n==2)
R94S<<-R94S%>%filter(n==2)
R96S<<-R96S%>%filter(n==2)
R98S<<-R98S%>%filter(n==2)
R00S<<-R00S%>%filter(n==2)
R02S<<-R02S%>%filter(n==2)
R04S<<-R04S%>%filter(n==2)
R06S<<-R06S%>%filter(n==2)
R08S<<-R08S%>%filter(n==2)
R10S<<-R10S%>%filter(n==2)
#get rid of N column
R90S<<-R90S[-c(17)]
R92S<<-R92S[-c(17)]
R94S<<-R94S[-c(17)]
R96S<<-R96S[-c(17)]
R98S<<-R98S[-c(17)]
R00S<<-R00S[-c(17)]
R02S<<-R02S[-c(17)]
R04S<<-R04S[-c(17)]
R06S<<-R06S[-c(17)]
R08S<<-R08S[-c(17)]
R10S<<-R10S[-c(17)]
# JOIN SECTIONS -----------------------------------------------------------
S4A<<-full_join(R90S,R92S)
S4B<<-full_join(S4A,R94S)
S4C<<-full_join(S4B,R96S)
S4D<<-full_join(S4C,R98S)
S4E<<-full_join(S4D,R00S)
S4F<<-full_join(S4E,R02S)
S4G<<-full_join(S4F,R04S)
S4H<<-full_join(S4G,R06S)
S4I<<-full_join(S4H,R08S)
Senate4<<-full_join(S4I,R10S)
# Fundraising Wrangling ---------------------------------------------------
Senate4 <<- Senate4%>%mutate(share_of_spent=spent/total_spent)
#Export Data
write.table(Senate4, "~/Documents/GitHub/Final-Data-Project/Tables/Senate.csv", sep="\t")
