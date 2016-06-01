# misc code

# ggplot for lasso
ggplot(tidied_cv, aes(lambda, estimate)) + geom_line(color = "red") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
  scale_x_log10() +
  geom_vline(xintercept = glance_cv$lambda.min) +
  geom_vline(xintercept = glance_cv$lambda.1se, lty = 2)


## Backward Logistic
```{r hccBackwardLog, eval=F}
hccbackward <- list()

for (i in 1:length(cohorts)) {
  hccmodeldata <- train %>% filter(cohort==cohorts[[i]])
  glmhccfull = glm(hccfm, data=hccmodeldata, family="binomial")
  hccbackward[[i]] <- step(glmhccfull, direction = "backward")
}
```

## Forward Logistic
```{r hccForwardLog, eval=F}
library(foreach)
library(doMC)
registerDoMC(cores=8)

hccforward <- vector('list', 1)

for(i in seq_along(hccforward)){
  a <- replicate(4, list())
  hccforward[[i]] <- a
}

hccforward <- list(c(0))


for (i in 1:2) {
  hccmodeldata <- train %>% filter(cohort==cohorts[[i]]) %>% ungroup()
  glmhccfull = glm(hccfm, data=hccmodeldata, family="binomial")
  glmhccempty = glm(isreadmit30dc ~ 1, data=hccmodeldata, family="binomial")
  for (j in 1:2) {
    hccforward[[i]] <- list(hccforward[[i]], step(glmhccempty, scope = list(
      lower = formula(glmhccempty), 
      upper = formula(glmhccfull)),
      direction = "forward",
      steps=j))
  }
}

t <- step(glmhccempty, scope = list(
  lower = formula(glmhccempty), 
  upper = formula(glmhccfull)),
  direction = "forward",
  steps=1)[c("aic", "call")]



t <- hccmodeldata %>% ungroup %>% select(isreadmit30dc, starts_with("hcc_"))
glmhccfull = glm(hccfm, data=hccmodeldata, family="binomial")
glmhccempty = glm(isreadmit30dc ~ 1, data=hccmodeldata, family="binomial")

for (i in 1:10) {
  hccforward[[i]] <- step(glmhccempty, scope = list(
    lower = formula(glmhccempty), 
    upper = formula(glmhccfull)),
    direction = "forward",
    steps = i)
}


bestglm(Xy = t, family=binomial, IC = "AIC", method="forward")

?bestglm
# Generate plot of AIC vs. Number of predictors for each cohort
hccmodeldata <- train %>% filter(cohort==cohorts[[1]])
hccf <- regsubsets(hccfm, data=hccmodeldata, method="backward", nvmax = 49)

```

