library(dplyr)

get_cost <- function(K){
  if (K==100){
    return(88000000)
  } else if (K==110) {
    return(94500000)
  } else if (K==120) {
    return(100000000)
  } else if (K==130) {
    return(105000000)
  } else if (K==140) {
    return(110000000)
  }
}

get_cost_vectorized <- function(K){
  sapply(K, get_cost)
}

results <- read_csv("./data/adaptation_results.csv")

grouped <- group_by(results, temp, precip, K, prio, alpha, beta)

new_results <- summarise(grouped, 
                         dom_rel  = mean(dom_rel), 
                         irr_rel  = mean(irr_rel), 
                         eco_rel  = mean(eco_rel), 
                         dom_crel = mean(dom_crel),
                         irr_crel = mean(irr_crel),
                         eco_crel = mean(eco_crel),
                         dom_res  = mean(Res_dom),
                         irr_res  = mean(Res_irr),
                         eco_res  = mean(Res_eco),
                         dom_vul  = mean(Vul_dom),
                         irr_vul  = mean(Vul_irr),
                         eco_vul  = mean(Vul_eco)) %>%
               mutate(cost = get_cost_vectorized(K))

write_csv(new_results, "./data/mean_adaptation_results.csv")
