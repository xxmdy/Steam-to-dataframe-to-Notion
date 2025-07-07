# 网页来源数据过滤函数
# 跳过来源为网页的"游玩时间"、"最后游玩日期"、"总时长/h"等不靠谱字段
filter_props_for_web_source <- function(props, row, update_fields = NULL,
                                        skip_playtime_web = TRUE,
                                        log = NULL, i = NULL, total = NULL) {
 
  source <- tolower(trimws(safe$safe_text(row$来源)))
  display_name <- safe$safe_text(row$游戏名称)
  game_id_str <- safe$safe_text(row$游戏ID)
  
  # 定义默认要跳过的字段（这些字段在网页来源下可能不准确）
  if (source == "网页" && skip_playtime_web) {
    skip_fields <- c("游玩时间", "最后游玩日期", "总时长/h")
    
    # 若传入了update_fields，仅跳过需要更新的字段中的交集（更精细控制）
    if (!is.null(update_fields)) {
      skip_fields <- intersect(skip_fields, update_fields)
    }
    
    # 实际上述字段
    props <- props[!names(props) %in% skip_fields]
    
    if (!is.null(log) && !is.null(i) && !is.null(total) && length(skip_fields) > 0) {
      log$subtle(glue::glue("⏩ [{i}/{total}] 跳过网页来源字段：{paste(skip_fields, collapse = '、')}"))
    }
  }
  
  return(props)
}

# 更新/上传函数
push_notion_record <- function(page_id, props, database_id, token, is_main_game, update_fields) {
  tryCatch({
    if (!is.null(page_id)) {
	    # 🔁 如果已有 page_id，说明是更新已有记录
      if (is_main_game && !is.null(update_fields)) {
        props <- props[names(props) %in% update_fields]
      }
      res <- PATCH(paste0("https://api.notion.com/v1/pages/", page_id),
                   add_headers(Authorization = paste("Bearer", token),
                               "Content-Type" = "application/json",
                               "Notion-Version" = "2022-06-28"),
                   body = toJSON(list(properties = props), auto_unbox = TRUE, null = "null"))
      if (status_code(res) == 200) return(list(success = TRUE, action = "🔁 更新", page_id = page_id))
      message("[更新失败] 响应内容：", content(res, "text", encoding = "UTF-8"))
      return(list(success = FALSE, action = "⚠️ 更新失败", page_id = NULL))
    } else {
      # ➕ 无 page_id，说明是新记录，执行插入操作
      res <- POST("https://api.notion.com/v1/pages",
                  add_headers(Authorization = paste("Bearer", token),
                              "Content-Type" = "application/json",
                              "Notion-Version" = "2022-06-28"),
                  body = toJSON(list(parent = list(database_id = database_id), properties = props), auto_unbox = TRUE, null = "null"))
      if (status_code(res) == 200) {
        new_id <- fromJSON(content(res, "text", encoding = "UTF-8"))$id
        return(list(success = TRUE, action = "✅ 插入", page_id = new_id))
      }
      message("[插入失败] 响应内容：", content(res, "text", encoding = "UTF-8"))
      return(list(success = FALSE, action = "⚠️ 插入失败", page_id = NULL))
    }
  }, error = function(e) {
    # ❌ 网络错误或结构错误，统一捕获异常
    message("[异常] push_notion_record(): ", e$message)
    return(list(success = FALSE, action = "❌ 错误", page_id = NULL))
  })
}
