# Notion 安全上传包装函数（带重试与限速控制）
notion_safe_upload <- function(page_id, props, database_id, token,
                               is_main_game, update_fields,
                               max_retries = 3, delay = 2) {
  for (attempt in 1:max_retries) {
    
    # 尝试上传（调用底层函数 push_notion_record）
    result <- tryCatch({
      push_notion_record(
        page_id = page_id,
        props = props,
        database_id = database_id,
        token = token,
        is_main_game = is_main_game,
        update_fields = update_fields
      )
    }, error = function(e) {
      # ❌ 捕获异常，返回结构化错误
      return(list(success = FALSE, action = "❌ 异常", error = e$message))
    })
    # ✅ 上传成功，直接返回结果
    if (isTRUE(result$success)) return(result) 
    
    # ⚠️ 上传失败，记录失败信息
    if (!is.null(result$error)) { 
      message(glue::glue("⚠️ 上传失败（第 {attempt} 次）：{result$error}"))
    } else if (grepl("限速|429", result$action)) {
      # 遇到限速（HTTP 429），暂停 delay 秒
      message(glue::glue("🚫 第 {attempt} 次遇到限速，等待 {delay} 秒…"))
    } else if (grepl("^⚠️ 插入失败|⚠️ 更新失败", result$action)) {
      # 插入/更新失败，可能为临时问题
      message(glue::glue("⚠️ 第 {attempt} 次上传失败：{result$action}，等待 {delay} 秒…"))
    }
    
    Sys.sleep(delay)  # 等待指定时间后重试
  }
  
  # ❌ 多次尝试失败，返回最终失败信息
  return(list(success = FALSE, action = "❌ 重试失败"))
}
