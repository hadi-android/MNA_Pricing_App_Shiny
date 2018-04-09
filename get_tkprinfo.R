get_tkprinfo <- function(name){
  library(RODBC)
  library(RJDBC)
  conn <- odbcDriverConnect('driver={SQL Server};server=Tor3e06;database=TE_3E_PROD;trusted_connection=true')
  for (i in 1:length(name)){
    sqlText = paste("select   timekeeper.DisplayName, TkprRateDate.DefaultRate, section.Description as section, 
      office.Description as office, Department.Description as department, year(GETDATE())-year(Timekeeper.JDDate) as yrsExperience from timekeeper
      inner join tkprdate on tkprdate.TimekeeperLkUp = Timekeeper.TkprIndex
      inner join Section on section.Code = tkprdate.Section
      inner join Office on Office.Code = TkprDate.Office
      inner join TkprRate on TkprRate.Timekeeper = Timekeeper.tkprIndex
      inner join TkprRateDate on TkprRateDate.TkprRateLkUp = TkprRate.TkprRateID
      inner join Department on Department.code = TkprDate.Department
      where timekeeper.DisplayName like ", paste("'",name[i],"',", sep=""), 
      "and Timekeeper.TkprStatus='10'
      and TkprRate.RateType = 'Base'
      and GETDATE() >= tkprdate.NxStartDate and GETDATE() <= tkprdate.NxEndDate
      and GETDATE() >= TkprRateDate.NxStartDate and GETDATE() <= TkprRateDate.NxEndDate", sep=" ")
  }
}