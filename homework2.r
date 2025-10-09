ca_pa <- read.csv("data/calif_penn_2011.csv", stringsAsFactors = FALSE)
nrow(ca_pa); ncol(ca_pa)
dim(ca_pa)
colSums(apply(ca_pa, c(1, 2), is.na)) 
## 含义：按“先行后列”判断是否为 NA，再对每一列求 NA 的个数
ca_pa_clean <- na.omit(ca_pa); 
n_deleted <- nrow(ca_pa) - nrow(ca_pa_clean); n_deleted
## 1c 的结果给出“每列 NA 个数”；1e 给出“含任意 NA 的行数”, 因此它们并不一定相等。



plot(ca_pa$Built_2005_or_later, ca_pa$Median_house_value,
     xlab = "Built 2005 or later (%)",
     ylab = "Median house value",
     main = "Median house value vs % built since 2005",
     pch = 16)
par(mfrow = c(1, 2))
with(subset(ca_pa, STATEFP == 6),  # California
     plot(Built_2005_or_later, Median_house_value,
          xlab = "Built 2005+ (%)",
          ylab = "Median house value",
          main = "California (STATEFP=6)",
          pch = 16, col = "steelblue"))
with(subset(ca_pa, STATEFP == 42), # Pennsylvania
     plot(Built_2005_or_later, Median_house_value,
          xlab = "Built 2005+ (%)",
          ylab = "Median house value",
          main = "Pennsylvania (STATEFP=42)",
          pch = 16, col = "tomato"))
par(mfrow = c(1, 1))


ca_pa$vacancy_rate <- with(ca_pa, Vacant_units / Total_units) 
min(ca_pa$vacancy_rate, na.rm = TRUE)
max(ca_pa$vacancy_rate, na.rm = TRUE)
mean(ca_pa$vacancy_rate, na.rm = TRUE)
median(ca_pa$vacancy_rate, na.rm = TRUE)

plot(ca_pa$vacancy_rate, ca_pa$Median_house_value,
     xlab = "Vacancy rate", ylab = "Median house value",
     main = "Median house value vs vacancy rate",
     pch = 16)

par(mfrow = c(1, 2))
with(subset(ca_pa, STATEFP == 6),
     plot(vacancy_rate, Median_house_value,
          xlab = "Vacancy rate", ylab = "Median house value",
          main = "California (STATEFP=6)", pch = 16, col = "steelblue"))
with(subset(ca_pa, STATEFP == 42),
     plot(vacancy_rate, Median_house_value,
          xlab = "Vacancy rate", ylab = "Median house value",
          main = "Pennsylvania (STATEFP=42)", pch = 16, col = "tomato"))
par(mfrow = c(1, 1))

acca <- c()
for (tract in 1:nrow(ca_pa)) {
  if (ca_pa$STATEFP[tract] == 6) {
    if (ca_pa$COUNTYFP[tract] == 1) {
      acca <- c(acca, tract)
    }
  }
}
accamhv <- c()
for (tract in acca) {
  accamhv <- c(accamhv, ca_pa[tract,10])
}
median(accamhv,na.rm=TRUE)
median(subset(ca_pa, STATEFP == 6 & COUNTYFP == 1)[[col_value]], na.rm = TRUE)
mean(ca_pa$Built_2005_or_later[
  ca_pa$STATEFP == 6 & ca_pa$COUNTYFP == 1
], na.rm = TRUE)   # Alameda
mean(ca_pa$Built_2005_or_later[
  ca_pa$STATEFP == 6 & ca_pa$COUNTYFP == 85
], na.rm = TRUE)   # Santa Clara
mean(ca_pa$Built_2005_or_later[
  ca_pa$STATEFP == 42 & ca_pa$COUNTYFP == 3
], na.rm = TRUE)   # Allegheny
cor(ca_pa$Median_house_value, ca_pa$Built_2005_or_later, use = "complete.obs")            
cor(ca_pa$Median_house_value[ca_pa$STATEFP == 6],
    ca_pa$Built_2005_or_later[ca_pa$STATEFP == 6],
    use = "complete.obs")                                                                 
cor(ca_pa$Median_house_value[ca_pa$STATEFP == 42],
    ca_pa$Built_2005_or_later[ca_pa$STATEFP == 42],
    use = "complete.obs")                                                                 
cor(ca_pa$Median_house_value[ca_pa$STATEFP == 6  & ca_pa$COUNTYFP == 1],
    ca_pa$Built_2005_or_later[ca_pa$STATEFP == 6  & ca_pa$COUNTYFP == 1],
    use = "complete.obs")                                                                
cor(ca_pa$Median_house_value[ca_pa$STATEFP == 6  & ca_pa$COUNTYFP == 85],
    ca_pa$Built_2005_or_later[ca_pa$STATEFP == 6  & ca_pa$COUNTYFP == 85],
    use = "complete.obs")                                                                
cor(ca_pa$Median_house_value[ca_pa$STATEFP == 42 & ca_pa$COUNTYFP == 3],
    ca_pa$Built_2005_or_later[ca_pa$STATEFP == 42 & ca_pa$COUNTYFP == 3],
    use = "complete.obs")
par(mfrow = c(1, 3))
with(subset(ca_pa, STATEFP == 6 & COUNTYFP == 1),
     plot(Median_household_income, Median_house_value,
          xlab = "Median household income",
          ylab = "Median house value",
          main = "Alameda County", pch = 16, col = "steelblue"))

with(subset(ca_pa, STATEFP == 6 & COUNTYFP == 85),
     plot(Median_household_income, Median_house_value,
          xlab = "Median household income",
          ylab = "Median house value",
          main = "Santa Clara County", pch = 16, col = "orange"))

with(subset(ca_pa, STATEFP == 42 & COUNTYFP == 3),
     plot(Median_household_income, Median_house_value,
          xlab = "Median household income",
          ylab = "Median house value",
          main = "Allegheny County", pch = 16, col = "tomato"))
par(mfrow = c(1, 1))

gender <- factor(c(rep("female", 91), rep("male", 92)))
table(gender)
#因子水平自动按字母序记录为 c("female","male")。
gender <- factor(gender, levels=c("male", "female"))
table(gender)
# 显式设定水平顺序为 c("male","female")，计数未变，但列出顺序变为 male 在前。
gender <- factor(gender, levels=c("Male", "female"))
# Note the mistake: "Male" should be "male"
table(gender)
# "Male" 与原数据 "male" 大小写不一致 → "Male" 在数据中实际不存在
table(gender, exclude=NULL)
# 把 NA 也统计出来
rm(gender)
# 移除

prop_above <- function(x, cutoff) {
  mean(x > cutoff, na.rm = TRUE)
}
prop_above(1:100, 50)
prop_above(1:100, 70)

install.packages("Devore7")
library(Devore7)
data("ex01.36", package = "Devore7")
x <- as.numeric(ex01.36[[1]])
install.packages("lattice")
library(lattice)
dotplot(x, main = "Escape times (seconds)", xlab = "Time (s)")
prop_over_7min <- prop_above(x, 420)
prop_over_7min  # 0.03846154

library(MASS)
data("Rabbit", package = "MASS")
df <- Rabbit
df <- df[order(df$Treatment, df$Dose, df$Animal), ]
bp_wide   <- unstack(df, BPchange ~ Animal)    
dose_wide <- unstack(df, Dose ~ Animal)
treat_wide <- unstack(df, Treatment ~ Animal)
Dose      <- dose_wide[[1]]
Treatment <- treat_wide[[1]]
out <- data.frame(
  Treatment = as.character(Treatment),
  Dose      = as.numeric(as.character(Dose)),  
  bp_wide[, c("R1","R2","R3","R4","R5")]
)
out <- out[order(out$Treatment, out$Dose), ]
row.names(out) <- NULL
out
6










