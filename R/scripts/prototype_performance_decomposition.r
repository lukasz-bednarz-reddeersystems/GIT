wA <- function(wAm1,wm1,wt,rm1,pt,pm1,Rm1){
  rval <- (wAm1-wm1*rm1+(wt-wm1)*(prod(Rm1)-1)-(pt-pm1))/(prod(Rm1)-1)
  return(rval)
}

rtn <- 0.8
initial_active <- 1.5
initial_total <- 2.5
total_w <- rtn*initial_total
pnl_last <- 0
pnl <- total_w-initial_total

wA(initial_active,initial_total,total_w,rtn-1,pnl,pnl_last,rtn)

