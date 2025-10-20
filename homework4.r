install.packages("tidyverse")
library(tidyverse)
ckm_nodes <- read_csv('data/ckm_nodes.csv')
noinfor <- which(is.na(ckm_nodes$adoption_date))
ckm_nodes <- ckm_nodes[-noinfor, ]
ckm_network <- read.table('data/ckm_network.dat')
ckm_network <- ckm_network[-noinfor, -noinfor]

adopt_m <- ckm_nodes$adoption_date              # 每位医生的采纳月份（数值）
n_doctor <- nrow(ckm_nodes)
A <- as.matrix(ckm_network)
nbr_list <- apply(A, 1, function(row) which(row != 0), simplify = FALSE)
months <- seq(min(adopt_m), max(adopt_m))

# 2) 生成“医生 × 月份”的面板骨架
panel <- tidyr::expand_grid(
  doctor = seq_len(n_doctor),
  month  = months
) |>
  arrange(doctor, month) |>
  mutate(
    adopt_month      = adopt_m[doctor],
    start_this_month = as.integer(adopt_month == month),
    adopted_before   = as.integer(adopt_month <  month)
  )

# 3) 计算 k_prior 与 k_prior_or_curr
#    对每一行 (doctor i, month t)，统计其邻居在 t 之前/至 t 为止的已采纳人数
panel <- panel |>
  mutate(
    k_prior = purrr::pmap_int(
      list(doctor, month),
      ~ {
        nb <- nbr_list[[..1]]
        if (length(nb) == 0) 0L else sum(adopt_m[nb] <  ..2)
      }
    ),
    k_prior_or_curr = purrr::pmap_int(
      list(doctor, month),
      ~ {
        nb <- nbr_list[[..1]]
        if (length(nb) == 0) 0L else sum(adopt_m[nb] <= ..2)
      }
    )
  ) |>
  select(doctor, month, start_this_month, adopted_before, k_prior, k_prior_or_curr)

# 4) 自检（经典数据应是 85×25=2125 行、6 列；若你们样本量不同会相应变化）
dim(panel)
head(panel, 10)

