# 加载必要的包
library(readxl)
library(cluster)
library(factoextra)
library(ggplot2)
library(dplyr)
library(tibble)

# 设置工作目录和读取数据
setwd("F:/")
data <- read_excel("草鞋山墓葬.xlsx", sheet = "1", range = "A1:Q25") 
selected_cols <- c("墓号","鼎","豆","壶","簋","甑","盉","盘","匜","钺","璧","琮","斧","锛","凿","珠坠饰","璜")
df <- data[, selected_cols]

# 只对数值列替换NA为0，跳过墓号列
numeric_cols <- setdiff(colnames(df), "墓号")
df[numeric_cols] <- lapply(df[numeric_cols], function(x) ifelse(is.na(x), 0, x))

# 确保墓号列没有缺失值
df <- df[complete.cases(df["墓号"]), ]

# 将数据转换为普通数据框(非tibble)并设置行名
df <- as.data.frame(df)
rownames(df) <- df$墓号
df <- df[, -1, drop = FALSE]  # 确保结果仍然是数据框

# 数据标准化
df_scaled <- scale(df)  

# 计算距离矩阵和层次聚类
dist_matrix <- dist(df_scaled, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")

# 确定最佳聚类数
wss_plot <- fviz_nbclust(df_scaled, hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  theme(text = element_text(family = "sans", size = 12)) + # 使用sans字体
  labs(title = "肘部法则确定最佳聚类数")


png("D:/dendrogram.png", width = 1000, height = 800)
fviz_dend(hc, k = 3, 
          cex = 1.2,
          rect = TRUE,
          rect_fill = TRUE,
          labels_track_height = 0.8,
          rect_border = "black",
          rect_lty = 1,
          k_colors = c("red", "green", "blue"),  
          type = "rectangle",
          main = "Hierarchical Clustering Dendrogram",
          ggtheme = theme_minimal(base_family = "sans")) + 
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        text = element_text(family = "sans", size = 14)) # 修复了这行
dev.off()

# Load necessary libraries
library(ggplot2) # Already loaded, but good to keep for consistency
library(FactoMineR) # Already loaded
library(factoextra) # Already loaded
library(dplyr) # Already loaded
library(readxl) # Already loaded
library(ggrepel) # Already loaded

# For 3D plotting
library(rgl) # New library for interactive 3D plots

# --- Data Preparation (from your original script) ---
# Set working directory & read data
# setwd("D:/") # Uncomment and modify if you need to set a specific working directory
data <- read_excel("草鞋山墓葬.xlsx", sheet = "2")

# Rename columns
colnames(data) <- c("墓号", "墓葬等级", "陶礼器数量", "玉礼器数量", "工具数量", "装饰品数量")

# Add factor '等级' and convert to English
data$等级 <- factor(data$墓葬等级,
                  levels = c("高等级", "中等级", "低等级"),
                  labels = c("High-rank", "Middle-rank", "Low-rank"))

# Select variables for PCA
pca_vars <- c("陶礼器数量", "玉礼器数量", "工具数量", "装饰品数量")

# Data cleaning function
clean_numeric <- function(x) {
  x <- gsub("[^0-9.]", "", x)  # Remove non-numeric characters
  x <- ifelse(x == "", "0", x) # Replace empty strings with 0
  as.numeric(x)
}

# Apply data cleaning
for (var in pca_vars) {
  data[[var]] <- clean_numeric(data[[var]])
}

# Check and remove all-zero columns
valid_vars <- pca_vars[sapply(data[pca_vars], function(x) sd(x, na.rm = TRUE) > 0)]
if (length(valid_vars) < 2) stop("有效变量不足，无法进行PCA分析")

# Remove rows containing NA
data_clean <- data[complete.cases(data[valid_vars]), ]

# Standardize data
data_scaled <- scale(data_clean[valid_vars])

# Perform PCA
pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

# Calculate explained variance
var_exp <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)

# Extract scores for the first three principal components
scores_3d <- as.data.frame(pca_result$x)[, 1:3] # Get PC1, PC2, PC3
scores_3d$Group <- data_clean$等级

# --- 3D PCA Plotting ---

# Prepare colors based on groups
# Using a colorblind-friendly palette from your 2D plot
group_colors <- c("High-rank" = "#E41A1C",
                  "Middle-rank" = "#377EB8",
                  "Low-rank" = "#4DAF4A")

# Assign colors to each point based on its group
point_colors <- group_colors[as.character(scores_3d$Group)]

# Open a new rgl window
open3d()

# Plot points in 3D
plot3d(x = scores_3d$PC1,
       y = scores_3d$PC2,
       z = scores_3d$PC3,
       col = point_colors,
       size = 10, # Adjust point size
       type = "p", # 'p' for points
       xlab = paste0("PC1 (", var_exp[1], "%)"),
       ylab = paste0("PC2 (", var_exp[2], "%)"),
       zlab = paste0("PC3 (", var_exp[3], "%)"),
       main = "3D PCA by Burial Rank",
       box = TRUE, # Display bounding box
       axes = TRUE # Display axes
)

# Add sphere for each point (optional, can make points more visible)
# spheres3d(x = scores_3d$PC1,
#           y = scores_3d$PC2,
#           z = scores_3d$PC3,
#           radius = 0.05, # Adjust radius as needed
#           col = point_colors)

# Add a legend
legend3d("topright",
         legend = levels(scores_3d$Group),
         col = group_colors[levels(scores_3d$Group)],
         pch = 16, # Corresponds to solid circles
         title = "Burial Rank",
         cex = 1.2, # Adjust legend text size
         bty = "n") # No box around the legend

# You can interact with the 3D plot (rotate, zoom) in the rgl window.

# To save the 3D plot (as an image or interactive HTML)
# For static image (e.g., PNG)
# rgl.snapshot("PCA_3D_by_burial_rank.png", fmt = "png", top = TRUE)

# For interactive HTML (requires 'htmlwidgets' and 'rglwidget')
# library(htmlwidgets)
# library(rglwidget)
# rglwidget(width = 800, height = 600) %>%
#   saveWidget(file = "PCA_3D_by_burial_rank.html")

# You can close the rgl window manually or using:
# rgl.close()

library(ggplot2)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(readxl)
library(ggrepel)
library(plotly)

# 设置工作目录 & 读取数据
setwd("D:/")
data <- read_excel("草鞋山墓葬.xlsx", sheet = "2")

# 重命名列
colnames(data) <- c("墓号", "墓葬等级", "陶礼器数量", "玉礼器数量", "工具数量", "装饰品数量")

# 转换为因子型
data$等级 <- factor(data$墓葬等级,
                  levels = c("高等级", "中等级", "低等级"),
                  labels = c("High-rank", "Middle-rank", "Low-rank"))

# 选择用于PCA的变量
pca_vars <- c("陶礼器数量", "玉礼器数量", "工具数量", "装饰品数量")

# 数据清洗函数
clean_numeric <- function(x) {
  x <- as.character(x)
  x <- gsub("[^0-9.]", "", x)  # 移除非数字字符
  x <- ifelse(x == "", "0", x) # 空字符串替换为0
  as.numeric(x)
}

# 应用数据清洗
data[pca_vars] <- lapply(data[pca_vars], clean_numeric)

# 修复错误：正确的变量筛选方式
valid_vars <- pca_vars[sapply(data[pca_vars], function(x) sd(x, na.rm = TRUE) > 0)]
if(length(valid_vars) < 2) stop("有效变量不足，至少需要2个有方差的变量")

# 移除NA行
data_clean <- na.omit(data[c("等级", valid_vars)])

# 标准化数据
data_scaled <- scale(data_clean[valid_vars])

# 执行PCA
pca_result <- prcomp(data_scaled)
var_exp <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)


# 3D PCA图 ----------------------------------------------------------------

# 提取得分（前3个主成分）
scores_3d <- as.data.frame(pca_result$x[, 1:3])
colnames(scores_3d) <- c("PC1", "PC2", "PC3")
scores_3d$Group <- data_clean$等级

# 创建交互式3D图
p_3d <- plot_ly(scores_3d, 
                x = ~PC1, y = ~PC2, z = ~PC3,
                color = ~Group,
                colors = c("#E41A1C", "#377EB8", "#4DAF4A"),
                symbol = ~Group,
                symbols = c("circle", "square", "diamond"),
                marker = list(size = 5)) %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = paste0("PC1 (", var_exp[1], "%)")),
    yaxis = list(title = paste0("PC2 (", var_exp[2], "%)")),
    zaxis = list(title = paste0("PC3 (", var_exp[3], "%)"))
  ))

print(p_3d)

# 保存3D图
htmlwidgets::saveWidget(p_3d, "D:/PCA_3D.html")

# 输出结果
cat("分析完成，结果已保存至D盘:\n")
cat("2D PCA图: D:/PCA_2D.png\n")
cat("交互式3D PCA图: D:/PCA_3D.png\n")
cat("静态3D PCA图 (PNG): D:/PCA_3D_Static.png\n")

# 执行PCA后，查看主成分的载荷（即原始变量对每个主成分的影响）
pca_loadings <- as.data.frame(pca_result$rotation)

# 查看前三个主成分的载荷
print("原始变量对PC1, PC2, PC3的载荷:")
print(pca_loadings[, 1:3])

# 如果想要更直观地表示，可以绘制载荷图
library(ggplot2)
p_loadings <- ggplot(data = pca_loadings, aes(x = PC1, y = PC2)) +
  geom_text(aes(label = rownames(pca_loadings)), color = "red") +
  labs(title = "载荷图 (PC1 vs PC2)", x = "PC1", y ="PC2") +
  theme_minimal()

print(p_loadings)


# 加载必要的包
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# 设置工作目录
setwd("D:/")

# 读取 Excel 数据
data <- read_excel("草鞋山墓葬.xlsx", sheet = "3", range = "A1:H25")

# 查看列名（非常重要！确认是否为“性别”、“年龄”）
print(colnames(data))

# 假设列名为："时期", "性别", "年龄", "陶礼器数量", "玉礼器数量", "工具数量", "装饰品数量"

# 将数据从宽格式转换为长格式
data_long <- data %>%
  pivot_longer(
    cols = c("陶礼器数量", "玉礼器数量", "工具数量", "装饰品数量"),
    names_to = "Artifact_Type",
    values_to = "Count"
  )

# 绘图：按“时期”分组，颜色填充为“随葬品类型”，并根据“性别”和“年龄”进行分面
p <- ggplot(data_long, aes(x = 时期, y = Count, fill = Artifact_Type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Comparison of Grave Goods by Period",
    x = "Period",
    y = "Number of Artifacts",
    fill = "Artifact Type"
  ) +
  theme_classic() +  # 不显示网格线
  theme(
    panel.grid = element_blank(),         # 移除所有网格线
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_grid(性别 ~ 年龄)  # 使用原始中文列名来分面！

# 显示图形
print(p)

# 保存图像到 D 盘
ggsave("D:/GraveGoods_StackedBarChart.png", plot = p, width = 12, height = 8, dpi = 300, bg = "white")

install.packages("ineq", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
library(ineq)
# 加载必要包
library(ineq)    # 基尼系数和洛伦兹曲线
library(dplyr)   # 数据操作
library(ggplot2) # 绘图

# 设置工作路径并读取数据
setwd("F:/")
df <- read.csv("caoxieshan1.csv", fileEncoding = 'GBK')

# 检查数据列名是否包含'墓葬值'（若列名不同需修改）
if (!"墓葬值" %in% colnames(df)) {
  stop("数据中需包含名为'墓葬值'的列，请检查数据列名！")
}

# ---------- 数据预处理 ----------
# 处理零值（替换为极小值避免计算错误）
df$墓葬值_adj <- df$墓葬值
df$墓葬值_adj[df$墓葬值_adj == 0] <- 1e-9

# ---------- 基尼系数计算 ----------
# 整体基尼系数
cat("整体基尼系数:", Gini(df$墓葬值), "\n")

# 按文化时期分组计算基尼系数
gini_by_period <- df %>%
  group_by(时期) %>%
  summarise(
    基尼系数 = Gini(墓葬值),
    .groups = "drop"
  )
print(gini_by_period)

# ---------- 洛伦兹曲线绘制 ----------
# 1. 整体洛伦兹曲线
lc_all <- Lc(df$墓葬值)
plot(lc_all, 
     main = "整体洛伦兹曲线（墓葬值分布）", 
     col = "red", 
     lwd = 2,
     xlab = "墓葬累计比例", 
     ylab = "价值累计比例")
abline(0, 1, lty = 2, col = "gray") # 添加对角线

# 2. 分时期洛伦兹曲线（使用ggplot2）
generate_lc_data <- function(data, period) {
  subset_data <- filter(data, 时期 == period)
  lc <- Lc(subset_data$墓葬值)
  data.frame(Period = period, p = lc$p, L = lc$L)
}

# 生成各时期曲线数据
periods <- unique(df$时期)
lc_data <- lapply(periods, function(p) generate_lc_data(df, p)) %>% 
  bind_rows()

# 绘制分时期洛伦兹曲线
ggplot(lc_data, aes(x = p, y = L, color = Period)) +
  geom_line(linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(title = "分文化时期洛伦兹曲线", 
       x = "墓葬累计比例", 
       y = "价值累计比例") +
  theme_minimal() +
  theme(legend.position = "bottom")# 完善后的绘图代码

ggplot(lc_data, aes(x = p, y = L, color = Period, linetype = Period)) + 
  geom_line(linewidth = 1) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "分文化时期洛伦兹曲线", 
    x = "墓葬累计比例", 
    y = "价值累计比例",
    color = "文化时期", 
    linetype = "文化时期"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),  # 增大图例标题字号
    legend.text = element_text(size = 10)                   # 增大图例文字字号
  ) +
  # 手动定义颜色和线型（新增早期良渚）
  scale_color_manual(
    values = c(
      "马家浜文化" = "#E41A1C",    # 红色
      "崧泽文化" = "#377EB8",      # 蓝色
      "良渚文化" = "#4DAF4A",      # 绿色
      "早期良渚文化" = "#984EA3"   # 紫色
    )
  ) +
  scale_linetype_manual(
    values = c(
      "马家浜文化" = "solid",      # 实线
      "崧泽文化" = "longdash",     # 长虚线
      "良渚文化" = "dotted",       # 点线
      "早期良渚文化" = "dotdash"   # 点划线
    )
  )



# ---------- 广义熵指数验证 ----------
# 自定义广义熵指数函数
ge_index <- function(x, alpha) {
  n <- length(x)
  mu <- mean(x)
  if (alpha == 0) {
    sum(log(mu / x)) / n  # Theil-T指数（α=0）
  } else if (alpha == 1) {
    sum((x / mu) * log(x / mu)) / n  # Theil-L指数（α=1）
  } else {
    sum(((x / mu)^alpha - 1) / (n * alpha * (alpha - 1)))  # 广义熵（α≠0,1）
  }
}

# 按时期分组计算广义熵指数
gem_by_period <- df %>%
  group_by(时期) %>%
  summarise(
    GE0 = ge_index(墓葬值_adj, 0),
    GE1 = ge_index(墓葬值_adj, 1),
    GE2 = ge_index(墓葬值_adj, 2),
    .groups = "drop"
  )
print(gem_by_period) 

# 加载必要包
library(DescTools)
library(ggplot2)

# ================== 1. 数据准备 ==================
wealth_data <- data.frame(
  power = c(rep("无权力", 8), rep("有权力", 25)),
  value = c(
    1.188, 1.188, 0.5511, 0.5511, 0.5511, 0, 0.3861, 0.3861,
    17.316, 4.752, 0, 4.356, 4.8312, 1.6104, 4.926, 1.1022,
    7.7293, 13.6263, 14.2212, 1.6104, 8.2359, 4.3815, 1.1022,
    17.569, 14.7616, 25.2191, 15.5188, 6.9366, 47.7433, 
    51.0311, 17.36, 15.5188, 47.7433
  )
)

# ================== 2. Bootstrap 函数 ==================
compute_gini_ci <- function(values, n_boot = 1000, conf_levels = c(0.8, 0.9, 0.99)) {
  gini_values <- numeric(n_boot)
  set.seed(123)
  for (i in 1:n_boot) {
    sample_values <- sample(values, length(values), replace = TRUE)
    gini_values[i] <- Gini(sample_values)
  }
  
  ci <- sapply(conf_levels, function(cl) {
    q <- quantile(gini_values, probs = c((1 - cl)/2, 1 - (1 - cl)/2))
    names(q) <- c("low", "high")
    return(q)
  })
  
  list(median = median(gini_values), ci = ci)
}

# ================== 3. 分组计算 ==================
no_power_values <- subset(wealth_data, power == "无权力")$value
power_values <- subset(wealth_data, power == "有权力")$value

results_nopower <- compute_gini_ci(no_power_values)
results_power   <- compute_gini_ci(power_values)

# ================== 4. 构建汇总数据框 ==================
summary_df <- data.frame(
  group = c("无权力", "有权力"),
  median = c(results_nopower$median, results_power$median),
  low_80 = c(results_nopower$ci[1,1], results_power$ci[1,1]),
  high_80 = c(results_nopower$ci[2,1], results_power$ci[2,1]),
  low_90 = c(results_nopower$ci[1,2], results_power$ci[1,2]),
  high_90 = c(results_nopower$ci[2,2], results_power$ci[2,2]),
  low_99 = c(results_nopower$ci[1,3], results_power$ci[1,3]),
  high_99 = c(results_nopower$ci[2,3], results_power$ci[2,3])
)

# ================== 5. 绘图部分 ==================
p <- ggplot(summary_df, aes(x = group)) +
  # 99% CI: 最细的竖线
  geom_linerange(aes(ymin = low_99, ymax = high_99),
                 color = "black", size = 0.5, alpha = 1) +
  # 90% CI: 中等宽度的竖线
  geom_linerange(aes(ymin = low_90, ymax = high_90),
                 color = "black", size = 1, alpha = 0.8) +
  # 80% CI: 最粗的竖线
  geom_linerange(aes(ymin = low_80, ymax = high_80),
                 color = "black", size = 2, alpha = 0.6) +
  # 中位数：用红色横线表示
  geom_point(aes(y = median), color = "red", shape = "_",
             size = 6, stroke = 2) +
  labs(y = "基尼系数中位数", x = "墓葬类型", title = "有无权力墓葬的基尼系数中位数与置信区间") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "white"),  # 设置背景为白色
        panel.background = element_rect(fill = "white")) +
  ylim(0, max(summary_df$high_99) * 1.1)

# 显示图形
print(p)

library(ggplot2)
library(dplyr)

# 修正后的数据准备 - 确保power_type和gini长度一致
power_data <- data.frame(
  power_type = c(rep("无政治军事权力", 26), rep("有政治军事权力", 4)),  # 共30个
  gini = c(1.188, 1.188, 0.5511, 0.5511, 0, 0.3861, 0.3861,
           17.316, 4.752, 0, 4.356, 4.8312, 1.6104, 4.926, 1.1022, 7.7293,
           13.6263, 14.2212, 1.6104, 8.2359, 4.3815, 1.1022, 17.569, 14.7616,
           25.2191, 6.9366,  # 26个无政治军事权力
           15.5188, 15.5188, 47.7433, 17.36)  # 4个有政治军事权力
)  # 总共30个观测值

# 计算统计量函数
calculate_stats <- function(data) {
  set.seed(123)
  medians <- replicate(1000, median(sample(data$gini, size = nrow(data), replace = TRUE)))
  
  list(
    median = median(data$gini),
    ci_80 = quantile(medians, c(0.1, 0.9)),
    ci_90 = quantile(medians, c(0.05, 0.95)),
    ci_99 = quantile(medians, c(0.01, 0.99))
  )
}

# 计算各组统计量
stats_no_power <- power_data %>% filter(power_type == "无政治军事权力") %>% calculate_stats()
stats_with_power <- power_data %>% filter(power_type == "有政治军事权力") %>% calculate_stats()

# 准备绘图数据
bullet_data <- data.frame(
  category = factor(c("无政治军事权力", "有政治军事权力"), 
                    levels = c("无政治军事权力", "有政治军事权力")),
  median = c(stats_no_power$median, stats_with_power$median),
  ci_80_low = c(stats_no_power$ci_80[1], stats_with_power$ci_80[1]),
  ci_80_high = c(stats_no_power$ci_80[2], stats_with_power$ci_80[2]),
  ci_90_low = c(stats_no_power$ci_90[1], stats_with_power$ci_90[1]),
  ci_90_high = c(stats_no_power$ci_90[2], stats_with_power$ci_90[2]),
  ci_99_low = c(stats_no_power$ci_99[1], stats_with_power$ci_99[1]),
  ci_99_high = c(stats_no_power$ci_99[2], stats_with_power$ci_99[2])
)

# 绘制子弹图
ggplot(bullet_data, aes(x = category)) +
  # 99%置信区间 - 最宽的柱 (底层)
  geom_rect(aes(xmin = as.numeric(category) - 0.005, 
                xmax = as.numeric(category) + 0.005,
                ymin = ci_99_low, ymax = ci_99_high),
            fill = "black", alpha = 0.7) +
  # 90%置信区间 - 中等宽度的柱 (中层)
  geom_rect(aes(xmin = as.numeric(category) - 0.03,
                xmax = as.numeric(category) + 0.03,
                ymin = ci_90_low, ymax = ci_90_high),
            fill = "gray60", alpha = 0.7) +
  # 80%置信区间 - 最窄的柱 (上层)
  geom_rect(aes(xmin = as.numeric(category) - 0.05,
                xmax = as.numeric(category) + 0.05,
                ymin = ci_80_low, ymax = ci_80_high),
            fill = "gray40", alpha = 0.7) +
  # 中位数 - 水平红线
  geom_segment(aes(x = as.numeric(category) - 0.1,
                   xend = as.numeric(category) + 0.1,
                   y = median, yend = median),
               color = "red", size = 1.5) +
  # 坐标轴和标签
  labs(title = "有无政治军事权力墓葬的基尼系数比较",
       subtitle = "柱状图表示置信区间: 80%(最窄), 90%(中等), 99%(最宽)",
       x = "政治军事权力类型",
       y = "基尼系数中位数") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  # 添加图例
  annotate("rect", xmin = 0.5, xmax = 0.7, ymin = max(bullet_data$ci_99_high)*0.9, 
           ymax = max(bullet_data$ci_99_high)*0.93, fill = "gray40") +
  annotate("rect", xmin = 0.5, xmax = 0.7, ymin = max(bullet_data$ci_99_high)*0.8, 
           ymax = max(bullet_data$ci_99_high)*0.83, fill = "gray60") +
  annotate("rect", xmin = 0.5, xmax = 0.7, ymin = max(bullet_data$ci_99_high)*0.7, 
           ymax = max(bullet_data$ci_99_high)*0.73, fill = "gray80") +
  annotate("segment", x = 0.5, xend = 0.7, y = max(bullet_data$ci_99_high)*0.6, 
           yend = max(bullet_data$ci_99_high)*0.6, color = "red", size = 1.5) +
  annotate("text", x = 0.75, y = max(bullet_data$ci_99_high)*0.915, 
           label = "80% CI", hjust = 0, size = 3.5) +
  annotate("text", x = 0.75, y = max(bullet_data$ci_99_high)*0.815, 
           label = "90% CI", hjust = 0, size = 3.5) +
  annotate("text", x = 0.75, y = max(bullet_data$ci_99_high)*0.715, 
           label = "99% CI", hjust = 0, size = 3.5) +
  annotate("text", x = 0.75, y = max(bullet_data$ci_99_high)*0.615, 
           label = "中位数", hjust = 0, size = 3.5)
