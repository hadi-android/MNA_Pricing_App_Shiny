calculate_fee_margin<- function(hours_partner, hours_associate, hours_other,
                                rate_partner, rate_associate, rate_other,
                                  cost_partner, cost_associate, cost_other){
  # browser()
  rate_partner = mean(rate_partner)
  rate_associate = mean(rate_associate)
  rate_other = mean(rate_other)
  
  cost_partner = mean(cost_partner)
  cost_associate = mean(cost_associate)
  cost_other = mean(cost_other)
  
  fees = hours_partner*rate_partner + hours_associate*rate_associate + hours_other*rate_other
  margin = hours_partner*(rate_partner - cost_partner) + hours_associate*(rate_associate-cost_associate) + hours_other*(rate_other-cost_other)
  margin_percent = 100*margin/fees
  output = c(fees, margin_percent)
  return(output)
  
}