# ------------------------------------------------------------------------------
# Title: Matching weights for multiple comparitor groups

# israel vax data August 2021

age_group <- factor( c('<50', '<50', '>50', '>50') )
vax <- factor( c('No', 'Yes', 'No', 'Yes') )
denom <- c(1116834, 3501118, 186078, 2170563)
severe_cases <- c(43, 11, 171, 290)


df <- data.frame(age_group, vax, severe_cases, denom)


mod <- glm(
  severe_cases ~ age_group + vax + age_group*vax + offset(log(denom)) , 
  data = df, 
  family = 'poisson'
  )

df
summary(mod)
1 - exp(-2.5059)

1 - exp((-10.1648 + 3.1726 + -2.5059 + 0.5775) - ( -10.1648 + 3.1726)  )
exp


exp( -10.1648) * 100000
exp( -7.9073 + 0.9151) * 100000

no_vax_mean <- mean(predict(mod2, newdata = df2_no_vax, type = 'response'))
vax_mean <- mean(predict(mod2, newdata = df2_vax, type = 'response')) 


# individual level data ----

lt50_novax <- c( rep(1, 43), rep(0, ( 1116834 - 43 ) ))
lt50_vax <- c( rep(1, 11), rep(0, ( 3501118 - 11 ) ) )
gt50_novax <- c( rep(1, 171), rep(0, ( 186078 - 171 ) ))
gt50_vax <- c( rep(1, 290), rep(0, ( 2170563 - 290 ) ) )

# event vector
severe_case <- c(lt50_novax, lt50_vax, gt50_novax, gt50_vax)


age_gt_50 <- c( 
  rep(0, 1116834 + 3501118), # under 50 
  rep(1, 186078 + 2170563) # over 50
  )

vax <- c( 
  rep(0, length(lt50_novax)), # under 50 no vax
  rep(1, length(lt50_vax)),  # under 50 vax
  rep(0, length(gt50_novax)), # over 50 no vax
  rep(1, length(gt50_vax)) # over 50 vax
  )

# dataframe 2 individual records
df2 <- data.frame(severe_case, age_gt_50, vax)

df2_no_vax <- df2_vax <- df2

df2_no_vax$vax <- 0
df2_vax$vax <- 1


mod2 <- glm(
  severe_case ~ age_gt_50 + vax + age_gt_50*vax , 
  data = df2, 
  family = binomial(link='logit')
)


no_vax_mean <- mean(predict(mod2, newdata = df2_no_vax, type = 'response'))
vax_mean <- mean(predict(mod2, newdata = df2_vax, type = 'response')) 

# vax eff marginal
1 -  ( vax_mean / no_vax_mean )

predict(mod2, newdata = df2_no_vax[1:10, ], type = 'response')


mod3 <- glm(
  severe_case ~ age_gt_50 + vax,  
  data = df2, 
  family = binomial(link='logit')
)

summary(mod3)
1 - exp(-1.97842)
