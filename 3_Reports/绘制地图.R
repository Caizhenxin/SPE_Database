library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
if (!require(cowplot)) {
  install.packages("cowplot")
  library(cowplot)
}

# 获取世界国家边界数据
world <- ne_countries(scale = "medium", returnclass = "sf")

# 创建国家数据
country_data <- data.frame(
  country = c("Belgium", "Hungary", "Japan", "Italy", 
              "United Kingdom", "China", "Taiwan", "Canada", "Spain", 
              "Israel", "United States", "Germany"),
  value = c(59, 418, 40, 174, 603, 1139, 1139,31, 68, 94, 288, 143)
)

# 将数据合并到地图数据 - 使用正确的列名"name_long"
world <- world %>%
  left_join(country_data, by = c("name_long" = "country"))

# 移除南极洲和其他无人居住地区
uninhabited <- c("Antarctica", "Fr. S. Antarctic Lands", "Heard I. and McDonald Is.")
world <- world %>% filter(!name_long %in% uninhabited)

# 创建地图
final_plot <- ggplot() +
  # 绘制有数据的国家
  geom_sf(data = filter(world, !is.na(value)), 
          aes(fill = value), color = "white", size = 0.2) +
  
  # 绘制没有数据的国家
  geom_sf(data = filter(world, is.na(value)), 
          fill = "darkgrey", color = "white", size = 0.2) +
  
  # 连续颜色标度
  scale_fill_gradientn(
    colours = c("#a1d99b", "#74c476", "#41ab5d", "#2e8b57", "#008080", "#1f77b4", "#2c6fb0", "#265ca0", "#1f4a8a"),
    name = "The source of the participants",
    na.value = NA,  # 确保NA值不显示颜色
    breaks = c(30, 200, 400, 600, 800, 1000, 1200),  # 设置合适的断点
    labels = c("30", "200", "400", "600", "800","1000", "1200"),  # 设置标签
    guide = guide_colorbar(
      barwidth = unit(0.5, "cm"),
      barheight = unit(2, "cm"),
      frame.colour = "black",
      ticks.colour = "black",
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  # 添加虚拟点用于创建missing data图例（不会在地图上显示）
  geom_point(data = dummy_data, 
             aes(x = x, y = y, shape = category), 
             size = 4, color = "darkgrey", fill = "darkgrey") +
  
  scale_shape_manual(
    values = c("Missing data" = 15),
    name = NULL,
    guide = guide_legend(
      override.aes = list(size = 4, color = "darkgrey", fill = "darkgrey"),
      order = 2
    )
  ) +
  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  
  theme_void() +
  theme(
    text = element_text(family = "Times New Roman"),  # 全局字体设置
    legend.title = element_text(size = 10, face = "bold", family = "Times New Roman"),
    legend.text = element_text(size = 9, family = "Times New Roman"),
    legend.position = c(0.01, 0.01),
    legend.justification = c(0, 0),
    legend.box = "vertical",
    legend.box.just = "left",
    legend.spacing.y = unit(0.2, "cm"),
    legend.margin = margin(5, 5, 5, 5),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  ) +
  # 为missing data创建shape图例
  scale_shape_manual(
    values = c("Missing data" = 15),  # 使用正方形
    name = "",
    guide = guide_legend(
      override.aes = list(color = "darkgrey", fill = "darkgrey", size = 6.5),
      keywidth = unit(2.5, "cm"),
      keyheight = unit(0, "cm"),
      order = 2,
      label.hjust = 0
    )
  ) 


# 显示图形
print(final_plot)
ggsave("final_plot.pdf", plot = final_plot, width = 10, height = 6, device = "pdf")
ggsave("final_plot.png", plot = final_plot, width = 10, height = 6, dpi = 100)



