# ==== 安装需要的R包 ====
install.packages(c(
  "httr2", "httr", "jsonlite", "dplyr", "tibble", "lubridate",
  "openxlsx", "xml2", "rvest", "cli", "glue"
))

# ==== 抓取Steam数据需要的包 ====
library(httr2) # 网络请求相关
library(httr) # 网络请求相关

library(rvest) # 网页和 XML 解析
library(xml2) # 网页和 XML 解析

library(dplyr) # 数据处理
library(tibble) # 数据处理
library(jsonlite) # 数据处理

library(lubridate) # 时间处理
library(openxlsx) # Excel 导出

# ==== 将导出数据上传至Notion所需要的包（如果只想手动导入，可不加载） ====
library(cli)  # 命令行美化输出       
library(glue)  # 字符串拼接（提示消息、上传状态）
