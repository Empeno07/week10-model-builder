rm(list = ls())

stores <- read.csv("store_sales.csv")

# promo is a category; "No" is the baseline
stores$promo <- factor(stores$promo, levels = c("No", "Yes"))

str(stores)
summary(stores)

cor(stores$ad_spend_k, stores$foot_traffic)
# Comment: ad spend and foot traffic are highly correlated (about 0.81), so they overlap. When both are in one model they share information,
# which can make each coefficient smaller and less certain than alone.
# So I should not judge either predictor by its p-value alone.

m1 <- lm(sales_k ~ ad_spend_k, data = stores)
m2 <- lm(sales_k ~ ad_spend_k + foot_traffic, data = stores)
m3 <- lm(sales_k ~ ad_spend_k + foot_traffic + promo, data = stores)

summary(m1)
summary(m2)
summary(m3)

# m3 check: intercept 17.46, ad_spend_k 1.496, foot_traffic 0.1235,
# promoYes 18.85; residual SE 13.19 on 56 df;
# R-squared 0.8185, adjusted R-squared 0.8088 (matches expected values)

models <- list(m1 = m1, m2 = m2, m3 = m3)

comp <- data.frame(
  model  = names(models),
  r2     = sapply(models, function(m) summary(m)$r.squared),
  adj_r2 = sapply(models, function(m) summary(m)$adj.r.squared),
  aic    = sapply(models, AIC),
  rse    = sapply(models, sigma)
)
comp[, -1] <- round(comp[, -1], 3)
comp

# Comment: m3 has the best (lowest) AIC at about 485.7, vs 505.7 for m2
# and 513.4 for m1. Its residual SE is about 13.19, vs 16.89 for m1.
# For the manager, the typical miss in a monthly sales prediction falls
# from about PHP 16,890 to about PHP 13,190, roughly PHP 3,700 closer to the real figure.

anova(m1, m2)   # F = 10.07, p = 0.0024
anova(m2, m3)   # F = 24.82, p = 0.000006

# Comment: both additions earn their place. Adding foot_traffic to m1
# is significant (p = 0.0024), and adding promo to m2 is even stronger (p < 0.001).

# CHOSEN MODEL: m3 (ad_spend_k + foot_traffic + promo), 3 predictors.
#
# Reasons:
# 1. m3 has the lowest AIC (485.66 vs 505.67 for m2 and 513.43 for m1),
#    and AIC already penalizes extra predictors.
# 2. Adjusted R-squared is highest for m3 (0.809 vs 0.729 and 0.686),
#    so the gain is not just from adding more terms.
# 3. anova() shows each step is worth it: m1 vs m2 p = 0.0024 and
#    m2 vs m3 p = 0.000006.
# 4. Residual SE drops to 13.19, the smallest of the three.
#
# I kept all three predictors because a simpler model was not enough:
# m2 leaves out the promo, which adds about PHP 18,850 to a month's
# sales, and its AIC is about 20 points worse than m3.
#
# What the model cannot do: it uses only 60 months of ONE store's
# synthetic data, so it cannot be applied to other stores without
# checking. Promo months were not assigned at random, so the results
# show associations, not proof of cause.

summary(m3)

# (Intercept) 17.46: a month with PHP 0 ad spend, 0 visitors, and no
# promo is predicted at about PHP 17,460 in sales. This is outside the data range, so it is only a starting point.

# ad_spend_k: each extra PHP 1,000 of ad spend goes with about
# PHP 1,500 more sales (1.496 thousand pesos), holding foot traffic and promo constant.

# foot_traffic: each extra 100 visitors goes with about PHP 12,350
# more sales (100 x 0.1235 = 12.35 thousand pesos), holding ad spend and promo constant.

# promoYes: months with a promo have about PHP 18,850 higher sales
# than months without a promo (the baseline is "No"), holding ad spend and foot traffic constant.

confint(m3)

# Interval sentence: holding the other predictors constant, each extra
# PHP 1,000 of ad spend goes with PHP 973 to PHP 2,020 more sales
# (95% CI 0.973 to 2.020 thousand pesos). The interval is fairly wide but stays above zero.
# I do not use the word "causes" because these data show associations only.

# 1. We wanted to know what drives monthly sales, using 60 months of your store's records.
# 2. The best model looks at three things together: how much you spend on ads, how many visitors come in, and whether a promo ran.
# 3. In this data, every extra PHP 1,000 in ads goes with about PHP 1,500 more in sales, and months with a promo had about
# PHP 18,850 more in sales than months without one, when the other things are equal.
# 4. The model explains about 82 out of every 100 pesos of the ups and downs in monthly sales,
# and its predictions are usually off by about PHP 13,000.
# 5. One limit: this is only one store over 60 months, so please use it for ad spend of PHP 6,000 to 40,000 and 258 to 605 visitors, and
# treat the results as patterns, not guarantees.

# Prediction inside the data range
predict(m3, data.frame(ad_spend_k = 25, foot_traffic = 450, promo = "Yes"))
# about 129.3 thousand pesos (PHP 129,300)