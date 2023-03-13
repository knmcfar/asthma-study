library(tidyverse)
library(reshape2)
library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)
library(rigr)

#load in data ------------------------------------------------------------------
AsthmaNO2 <- read.csv("AsthmaNO2.csv")
AsthmaNO2 <- AsthmaNO2[, 2:8]
View(AsthmaNO2)

#exploratory analyses ----------------------------------------------------------

#create age and height categories and find means/SDs
AsthmaNO2$age_cat <- cut(AsthmaNO2$age, 
                        c(8, 10, 12, 14, 16, 19), 
                        include.lowest = TRUE)
table(AsthmaNO2$age_cat)
AsthmaNO2$height_cat <- cut(AsthmaNO2$height, 
                           c(120, 135, 150, 165, 180, 195), 
                           include.lowest = TRUE)
table(AsthmaNO2$age_cat)

sum_Asthma_age <- AsthmaNO2 %>% 
                      group_by(no2high, age_cat) %>%
                      summarise(mean_fev = mean(fev1), sd_fev = sd(fev1))
sum_Asthma_age

sum_Asthma_height <- AsthmaNO2 %>% 
                        group_by(no2high, height_cat) %>%
                        summarise(mean_fev = mean(fev1), sd_fev = sd(fev1))
sum_Asthma_height

## Plot of individual regression lines
AsthmaNO2$no2high[AsthmaNO2$no2high == 1] <- "High"
AsthmaNO2$no2high[AsthmaNO2$no2high == 0] <- "Low"

ggplot(AsthmaNO2, aes(age, fev1, group = id)) + 
  facet_wrap(. ~ no2high) + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
  xlab("Age (years)") + 
  ylab("FEV1 value (ml)") + 
  theme_bw() + 
  theme(legend.position = "none")

slopes <- by(AsthmaNO2, INDICES = AsthmaNO2$id, FUN = function(slope.data) {
  lm(fev1 ~ age, data = slope.data)$coef[2]}
  )

dat_slopes <- data.frame("id" = names(slopes), "slope" = as.numeric(slopes))
Asthma_sum <- unique(AsthmaNO2[, c("id", "no2high")])
Asthma_sum <- merge(Asthma_sum, dat_slopes, by = "id")

##t test of slopes dichotomized by pollution level
t.test(slope ~ no2high, data = Asthma_sum)

#create wide data organized by year (rounded) since first visit 
AsthmaNO2 <- AsthmaNO2 %>% 
                group_by(id) %>% 
                mutate(agechange = age - age[1])
AsthmaNO2$agechange_re <- round(AsthmaNO2$agechange, 0)

Asthma_w <- AsthmaNO2 %>% 
                group_by(id) %>% 
                summarise(ast_0 = mean(fev1[agechange_re == 0], na.rm = TRUE),
                          ast_1 = mean(fev1[agechange_re == 1], na.rm = TRUE),
                          ast_2 = mean(fev1[agechange_re == 2], na.rm = TRUE), 
                          ast_3 = mean(fev1[agechange_re == 3], na.rm = TRUE),
                          ast_4 = mean(fev1[agechange_re == 4], na.rm = TRUE))

Asthma_long <- unique(AsthmaNO2[, c("id", "male", "asthma", "no2high")]) 
wide_data <- merge(Asthma_long, Asthma_w, by = "id")

#create correlation matrix based on years since first visit
cor_mat <- cor(wide_data[, c("ast_0", "ast_1", "ast_2", "ast_3", "ast_4")],
               use = "pairwise.complete.obs")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor_mat, 
         method = "color", 
         col = col(200),  
         type = "upper", 
         order = "original", 
         addCoef.col = "black",
         tl.col = "black", 
         tl.srt = 45, 
         mar = c(0, 0, 1.5, 0),
         diag = FALSE)

#confirmatory analysis ---------------------------------------------------------
AsthmaNO2 <- AsthmaNO2[order(AsthmaNO2$id), ]
AsthmaNO2$no2high[AsthmaNO2$no2high == "High"] <- 1
AsthmaNO2$no2high[AsthmaNO2$no2high == "Low"] <- 0

mod1 <- geeglm(fev1 ~ no2high*age, 
               data = AsthmaNO2, 
               id = id, 
               corstr = "ar1")
summary(mod1)
tidy(mod1, conf.int = TRUE)


mod1_alt <- geeglm(fev1 ~ no2high+age, 
                   data = AsthmaNO2, 
                   id = id, 
                   corstr = "ar1")

#compare interaction model to model w/o interaction term
anova(mod1_alt, mod1)

mod2 <- geeglm(fev1 ~ no2high*height, 
               data = AsthmaNO2, 
               id = id, 
               corstr = "ar1")

summary(mod2)
tidy(mod2, conf.int = TRUE)


mod2_alt <- geeglm(fev1 ~ no2high+height, 
                   data = AsthmaNO2, 
                   id = id, 
                   corstr = "ar1")

anova(mod2_alt, mod2)

#diagnostics: look at residuals from model -------------------------------------

pop.res <- resid(mod3, level = 0)
cluster.res <- resid(mod3, level = 1)
AsthmaNO2$pred_mod3 <- predict(mod3, level = 0)
AsthmaNO2$pred_mod3_dif <- AsthmaNO2$fev1 - AsthmaNO2$pred.mod3

plot(pred_mod3_dif~age, data = AsthmaNO2)
abline(lm(pred_mod3_dif~age, data=AsthmaNO2), col = "red")
