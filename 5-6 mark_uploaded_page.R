# 上传steam图标并标记上传（属于个性化配置，可选）
mark_uploaded_page <- function(page_id, token) {
  tryCatch({
    # 设置页面图标为 Steam Logo
    PATCH(paste0("https://api.notion.com/v1/pages/", page_id),
          add_headers(Authorization = paste("Bearer", token),
                      "Content-Type" = "application/json",
                      "Notion-Version" = "2022-06-28"),
          body = toJSON(
			      list(icon = list(
				      type = "external", 
							external = list(url = "https://upload.wikimedia.org/wikipedia/commons/8/83/Steam_icon_logo.svg")
						)), 
						auto_unbox = TRUE))
    # 设置“已上传”字段为 TRUE（Notion 数据库中必须存在名为“已上传”的 checkbox 字段）
    PATCH(paste0("https://api.notion.com/v1/pages/", page_id),
          add_headers(Authorization = paste("Bearer", token),
                      "Content-Type" = "application/json",
                      "Notion-Version" = "2022-06-28"),
          body = toJSON(list(properties = list(`已上传` = list(checkbox = TRUE))), auto_unbox = TRUE))
  }, error = function(e) {
    message("[警告] 设置图标或上传标记失败：", e$message)
  })
}
