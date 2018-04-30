get_tkprinfo <- function(name, rate_type){
  library(RODBC)
  library(RJDBC)
  names_found = character(length(name))
  output = list()
  # browser()
  # if(length(name)>0){
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
        "and TkprRate.RateType = ", paste("'", rate_type[i], "'", sep=""),
        "and GETDATE() >= tkprdate.NxStartDate and GETDATE() <= tkprdate.NxEndDate
        and GETDATE() >= TkprRateDate.NxStartDate and GETDATE() <= TkprRateDate.NxEndDate
        and GETDATE() >= TkprDateHR.NxStartDate and GETDATE() <= TkprDateHR.NxEndDate", sep=" ")
      # browser()
      result = sqlQuery(conn, sqlText, stringsAsFactors=F)
      names_found[i] = ifelse(dim(result)[1]>0, "found","not found")
      if(i==1)
        result1 = result
      else
        result1 = rbind(result1, result)
    }
    close(conn)
    # if(dim(result1)[1]==0)
    #   result1=NA
  # }
  # else
  #   result1=NA
  output[[1]] = result1
  output[[2]] = names_found
  return(output)
}