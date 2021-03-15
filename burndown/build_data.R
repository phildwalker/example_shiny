# Pulling in data to build forecast from.
# Mon Apr 13 18:36:50 2020 ------------------------------
 

library(tidyverse)
library(glue)
library(DBI)

EDW = DBI::dbConnect(odbc::odbc(), dsn = "MCCBIEDW1")

Surgery_sql <- glue_sql("
SELECT DISTINCT
      a.PatientID, a.PatientEncounterID, a.LinkCSNID,a.LogID,
      a.SurgeryDTS, a.LocationID, e.PlaceOfServiceID, e.PlaceOfServiceDSC,
      e.DepartmentID, e.DepartmentDSC, c.LocationTypeCD, c.LocationTypeDSC,
      a.CaseID, a.OperatingRoomID, b.ProcedureID,  d.ProcedureCD,
      b.ProcedureDisplayNM, d.ShortNM, a.ServiceCD, a.ServiceDSC,
      a.CaseClassCD, a.CaseClassDSC, a.PriorityCD, a.PriorityDSC,
      a.PreOperationVisitNeededFLG, a.PatientTotalTimeMinutesNBR,
      a.TotalTimeNeededMinutesNBR, a.BeginDTS, a.EndDTS,  a.ScheduledDTS, a.CancelDTS,  a.CancelUserID,
      a.CancelReasonCD, a.CancelReasonDSC, a.BumpedDTS, a.BumpedFLG
    FROM EPIC.Surgery.SurgicalCase a
    INNER JOIN EPIC.Surgery.LogAllProcedure b ON a.LogID = b.LogID
    INNER JOIN EPIC.Surgery.Location c  ON a.LocationID = c.LocationID
    INNER JOIN EPIC.Reference.Procedure1 d ON b.ProcedureID = d.ProcedureID
    INNER JOIN EPIC.Surgery.LogCharges e ON b.LogID = e.LogID
  WHERE a.SurgeryDTS >= '2017-01-01 00:00:00.0000000'
", 
                        .con = EDW)

Surgery_ <- dbSendQuery(EDW, Surgery_sql)
Surgery_Output <- dbFetch(Surgery_)


Surg_Clean <-
  Surgery_Output %>% 
  filter(is.na(CancelDTS)) %>% 
  mutate(SchedDate = as.Date(ScheduledDTS),
         SchedMo = lubridate::floor_date(SchedDate, "week")) %>% 
  count(SchedMo, ServiceDSC, DepartmentDSC)


saveRDS(Surgery_Output, here::here("data", "AllSurg_Detail.rds"))
saveRDS(Surg_Clean, here::here("data", "Surgical_Aggreg.rds"))


# Getting the list of OR and Cath Labs at MC/WL ------
OR_SurgCent <- c("MC-OPERATING ROOM", "WL-OPERATING ROOM", "MC-NEURO OPERATING RM", "MC-CATH LAB")
saveRDS(OR_SurgCent, here::here("data", "OR_SurgCnt_Names.rds"))


# Surgery_Output %>% 
#   filter(str_detect(DepartmentDSC, "WL-|MC-")) %>% 
#   count(DepartmentDSC) %>% 
#   arrange(desc(n))

# Setting filter rules ------
# From here, it looks like it's worth setting the rule as needs more than 50 weeks of data

# Groups:   ServiceDSC [23]
# ServiceDSC           n
# <chr>            <int>
# 1 Anesthesiology       1
# 2 Cardiopulmonary      8
# 3 Pulmonary           18
# 4 Gastroenterology    41
# 5 Radiology           50
# 6 Pediatrics         139
# 7 Cardiovascular     157
# 8 Dentistry          157
# 9 Endoscopy          159
# 10 Oral Surgery       164

Surg_Clean %>% 
  group_by(ServiceDSC, SchedMo) %>% 
  summarise(sum_n = sum(n, na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(ServiceDSC) %>% 
  count() %>% 
  arrange(n) %>% 
  ggplot(aes(n))+
    geom_bar()+
    theme_bw()+
    scale_y_continuous(breaks = c(1:10))+
    labs(title = "Number of Weeks with Data")


SL_drop <- 
  Surg_Clean %>% 
  group_by(ServiceDSC, SchedMo) %>% 
  summarise(sum_n = sum(n, na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(ServiceDSC) %>% 
  count() %>% 
  filter(n < 100) %>% 
  pull(ServiceDSC)

saveRDS(SL_drop, here::here("data", "SL_toDrop.rds"))

