#Startup code
build.nsim <- function(nsim,nvec){
  simdx <- c()
  for(i in 1:length(nvec)) 
    simdx <- c(simdx,rep(1:nsim,each=nvec[i])+(i-1)*nsim)
  dt <- data.table(sim=simdx)
  bigN <- nrow(dt)
  dt$n <- rep(rep(nvec,nvec),each=nsim)
  dt$one <- 1
  dt$simc <- dt[,cumsum(one),by=sim]$V1
  dt$one <- NULL
  return(dt)
}
wpull <- function(strinput){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(con,strinput)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste0(strinput,'_labels')))
  DBI::dbDisconnect(con)
  return(dt)
}


hprice1 <-wpull('hprice1')
model1 <- lm(log(price)~bdrms + lotsize + sqrft, data = hprice1)

summary(model1, vcov. = vcovHC)
ic <- function(x){
  return(c(AIC(x),BIC(x)))
}
ic(model1)

lm(log(price)~bdrms + lotsize + sqrft + colonial +  I(bdrms^2), data = hprice1 ) %>% ic
lm(log(price)~bdrms + lotsize + sqrft + colonial +I(lotsize^2), data = hprice1 ) %>% ic ## Lowest AIC & BIC
lm(log(price)~bdrms + lotsize + sqrft + colonial +I(sqrft^2), data = hprice1 ) %>% ic
lm(log(price)~bdrms + lotsize + sqrft + colonial +I(colonial^2), data = hprice1 ) %>% ic

lm(log(price)~bdrms + lotsize + sqrft + colonial +I(bdrms^2) + I(lotsize^2) + I(sqrft^2)+I(colonial^2),  data = hprice1 ) %>% ic

model_stepwise <- lm(log(price)~bdrms + lotsize + sqrft + colonial +I(bdrms^2) + I(lotsize^2) + I(sqrft^2)+I(colonial^2)+I(bdrms^3)+I(lotsize^3)+I(sqrft^3)+I(colonial^3),  data = hprice1 )
summary(step(model_stepwise))

# Best MOdel is using AIC is lm(log(price) ~ lotsize + colonial + I(sqrft^2) + I(lotsize^3) + I(sqrft^3), data = hprice1)

summary(step(model_stepwise,k=log(nrow(hprice1))))

# Best model using BIC is lm(log(price) ~ lotsize + colonial + I(sqrft^2) + I(lotsize^3), data = HPRICEDATA)




gpa2 <- wpull('gpa2')
model2 <-lm(colgpa~sat+athlete+tothrs+white+female+hsrank+hsperc+I(sat^2)+I(athlete^2)+I(tothrs^2)+I(white^2)+I(female^2)+I(hsrank^2)+I(hsperc^2)+I(sat^3)+I(athlete^3)+I(tothrs^3)+I(white^3)+I(female^3)+I(hsrank^3)+I(hsperc^3), data = gpa2)
ic <- function(x){
  return(c(AIC(x),BIC(x)))
}
ic(model2)

summary(step(model2))
## Best MOdel is using AIC is lm(colgpa ~ sat + athlete + tothrs + white + female + hsrank + hsperc + I(sat^2) + I(tothrs^2) + I(hsperc^2) + I(tothrs^3) + I(hsperc^3), data = GPA)

summary(step(model2,k=log(nrow(gpa2))))

## Best MOdel is using BIC is lm(colgpa ~ athlete + tothrs + white + female + hsrank + hsperc + I(sat^2) + I(hsperc^2) + I(hsperc^3), data = GPA)



mlb1 <- wpull('mlb1')
model3.1 <- lm(log(salary)~teamsal+hruns+runsyr+hits+catcher, data = mlb1)
summary(model3.1)
ic(model3.1)

### Quadratic
model3.2 <- lm(log(salary)~teamsal+hruns+runsyr+hits+catcher+I(teamsal^2)+I(hruns^2)+I(runsyr^2)+I(hits^2)+I(catcher^2)++I(teamsal^3)+I(hruns^3)+I(runsyr^3)+I(hits^3)+I(catcher^3), data = mlb1)
summary(model3.2) 
ic <- function(x){
  return(c(AIC(x),BIC(x)))
}
ic(model3.2)
summary(step(model3.2))

### Best model using AIC log(salary) ~ hruns + hits + catcher + I(hruns^2) + I(runsyr^2) + I(hits^2) + I(teamsal^3) + I(hits^3)

summary(step(model3.2,k=log(nrow(mlb1))))

### Best model using BIC lm(formula = log(salary) ~ hruns + hits + catcher + I(runsyr^2) + I(hits^2) + I(hits^3), data = MLB)
