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
