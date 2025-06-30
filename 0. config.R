# 配置项，需手动填写自己的Steam API、Steam ID、Notion API、Database ID
API_key <- "你的32位Steam API，保留双引号"
steam_id <- "你的17位Steam ID，保留双引号"
notion_token <- "你的Notion Integration Token（API），保留双引号"
database_id <- "你的32位Notion Database ID，保留双引号" 

# 安装需要的R包
install.packages(c(
  "httr2", "httr", "jsonlite", "dplyr", "tibble", "lubridate",
  "openxlsx", "xml2", "rvest", "cli", "glue"
))
