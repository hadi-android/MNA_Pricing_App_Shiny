calculate_fee_margin<- function(hours_partner, hours_associate, hours_other,
                                load_partner, load_associate, load_other,
                                rate_partner, rate_associate, rate_other,
                                  cost_partner, cost_associate, cost_other){
  # browser()
  fees=0
  if(length(load_partner)==0)
    load_partner=0
  else{
   if(all(load_partner==0)==TRUE)
      load_partner = rep(100/length(load_partner),length(load_partner))
   else
     load_partner = 100*load_partner/hours_partner
   if(sum(load_partner) != 100)
     fees = NA
  }
  if(length(load_associate)==0)
    load_associate=0
  else{
    load_associate = ifelse(load_associate==0, 100/length(load_associate), 100*load_associate/hours_associate)
    if(sum(load_associate) != 100)
      fees = NA
  }
  if(length(load_other)==0)
    load_other=0
  else{
    load_other = ifelse(load_other==0, 100/length(load_other), 100*load_other/hours_other)
    if(sum(load_other) != 100)
      fees = NA
  }
  
  if(!is.na(fees)){
  
    rate_partner = sum(load_partner*rate_partner)/100
    rate_associate = sum(load_associate*rate_associate)/100
    rate_other = sum(load_other*rate_other)/100
  
    cost_partner = sum(load_partner*cost_partner)/100
    cost_associate = sum(load_associate*cost_associate)/100
    cost_other = sum(load_other*cost_other)/100
  
    fees = hours_partner*rate_partner + hours_associate*rate_associate + hours_other*rate_other
    margin = hours_partner*(rate_partner - cost_partner) + hours_associate*(rate_associate-cost_associate) + hours_other*(rate_other-cost_other)
    margin_percent = 100*margin/fees
  }
  else
    margin_percent = NA
  
  output = c(fees, margin_percent)
  return(output)
  
    
  
}