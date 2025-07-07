# 获取Notion中已有字段（属性）
get_notion_property_names <- function(database_id, token) {
  url <- paste0("https://api.notion.com/v1/databases/", database_id)
  res <- httr::GET(
    url,
    httr::add_headers(
      "Authorization" = paste("Bearer", token),
      "Notion-Version" = "2022-06-28"
    )
  )

  if (httr::status_code(res) != 200) {
    stop("❌ 获取 Notion 数据库字段失败，状态码：", httr::status_code(res))
  }

  db <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  props <- names(db$properties)
  return(props)
}

# 检查插入/更新字段与Notion中已有字段的一致性
check_field_consistency <- function(database_id, token,
                                    insert_fields = NULL,
                                    update_fields = NULL) {
  # 获取当前 Notion 数据库中的字段名
  notion_fields <- get_notion_property_names(database_id, token)

  # 找出 insert 和 update 中缺失字段
  missing_insert <- if (!is.null(insert_fields)) setdiff(insert_fields, notion_fields) else character()
  missing_update <- if (!is.null(update_fields)) setdiff(update_fields, notion_fields) else character()

  # 输出提醒信息
  if (length(missing_insert) > 0) {
    cli::cli_alert_danger("❌ insert_fields 中缺失字段：{paste(missing_insert, collapse = ', ')}")
  }
  if (length(missing_update) > 0) {
    cli::cli_alert_danger("❌ update_fields 中缺失字段：{paste(missing_update, collapse = ', ')}")
  }

  # 可选：提醒 update_fields 比 insert_fields 多了哪些字段
  if (!is.null(insert_fields) && !is.null(update_fields)) {
    unmatched_update <- setdiff(update_fields, insert_fields)
    if (length(unmatched_update) > 0) {
      cli::cli_alert_info(
        "🔁 update_fields 中包含 insert_fields 未指定的字段：{paste(unmatched_update, collapse = ', ')}"
      )
    }
  }

  # 若有缺失字段，终止程序
  if (length(missing_insert) > 0 || length(missing_update) > 0) {
    stop("🛑 上传终止：Notion 数据库字段（即「属性」）不完整，请先补充再运行脚本。")
  }
}
