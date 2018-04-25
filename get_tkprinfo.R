get_tkprinfo <- function(name, rate_type){
  library(RODBC)
  library(RJDBC)
  if(length(name)>0){
    conn <- odbcDriverConnect('driver={SQL Server};server=Tor3e06;database=TE_3E_PROD;trusted_connection=true')
    for (i in 1:length(name)){
      sqlText = paste("select   timekeeper.DisplayName, TkprRateDate.DefaultRate, tkprdateHR.DirectCost, section.Description as section, 
        office.Description as office, Department.Description as department, year(GETDATE())-year(Timekeeper.JDDate) as yrsExperience from timekeeper
        inner join tkprdate on tkprdate.TimekeeperLkUp = Timekeeper.TkprIndex
        INNER JOIN TKPRDATEHR ON TKPRDATEHR.TIMEKEEPERLKUP = Timekeeper.TkprIndex
        inner join Section on section.Code = tkprdate.Section
        inner join Office on Office.Code = TkprDate.Office
        inner join TkprRate on TkprRate.Timekeeper = Timekeeper.tkprIndex
        inner join TkprRateDate on TkprRateDate.TkprRateLkUp = TkprRate.TkprRateID
        inner join Department on Department.code = TkprDate.Department
        where timekeeper.DisplayName like ", paste("'%",name[i],"%'", sep=""), 
        "and Timekeeper.TkprStatus='10'",
        "and TkprRate.RateType = ", paste("'", rate_type, "'", sep=""),
        "and GETDATE() >= tkprdate.NxStartDate and GETDATE() <= tkprdate.NxEndDate
        and GETDATE() >= TkprRateDate.NxStartDate and GETDATE() <= TkprRateDate.NxEndDate
        and GETDATE() >= TkprDateHR.NxStartDate and GETDATE() <= TkprDateHR.NxEndDate", sep=" ")
      # browser()
      result = sqlQuery(conn, sqlText)
      if(i==1)
        output = result
      else
        output = rbind(output, result)
    }
    close(conn)
    if(dim(output)[1]==0)
      output=NA
  }
  else
    output=NA
  return(output)
}