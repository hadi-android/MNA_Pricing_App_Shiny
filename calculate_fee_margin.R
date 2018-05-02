calculate_fee_margin<- function(hours_partner, hours_associate, hours_other,
                                load_partner, load_associate, load_other,
                                rate_partner, rate_associate, rate_other,
                                  cost_partner, cost_associate, cost_other){
  # browser()
  load_partner = ifelse(load_partner==0, 100/length(load_partner), load_partner)
  load_associate = ifelse(load_associate==0, 100/length(load_associate), load_associate)
  load_other = ifelse(load_other==0, 100/length(load_other), load_other)
  
  rate_partner = sum(load_partner*rate_partner)/100
  rate_associate = sum(load_associate*rate_associate)/100
  rate_other = sum(load_other*rate_other)/100
  
  cost_partner = sum(load_partner*cost_partner)/100
  cost_associate = sum(load_associate*cost_associate)/100
  cost_other = sum(load_other*cost_other)/100
  
  fees = hours_partner*rate_partner + hours_associate*rate_associate + hours_other*rate_other
  margin = hours_partner*(rate_partner - cost_partner) + hours_associate*(rate_associate-cost_associate) + hours_other*(rate_other-cost_other)
  margin_percent = 100*margin/fees
  output = c(fees, margin_percent)
  return(output)
  
}