# 游玩状态判断函数（只是大致分类，具体可根据自己需求修改；不完全准确）
status_utils <- function(safe) {
  get_game_status <- function(row) {
    type <- tolower(safe$safe_text(row$内容类型))
    if (!grepl("游戏本体", type)) return(NULL) # 只判断“游戏本体”，其它类型直接跳过
    
    # 提取字段并转换为安全格式
    hours <- safe$safe_number(row$总时长小时)
    last_played <- safe$safe_date(row$最后游玩)
    first_achieve <- safe$safe_date(row$首个成就时间)
    achievements <- safe$safe_number(row$已解锁成就)
    total_achievements <- safe$safe_number(row$成就总数)
    
    # 设置默认值，避免 NULL 带来逻辑错误
    if (is.null(hours)) hours <- 0
    if (is.null(achievements)) achievements <- 0
    if (is.null(total_achievements) || total_achievements == 0) total_achievements <- NA
    
    today <- Sys.Date()
    
    # 完全未游玩的状态（没时长 + 没成就 + 没解锁过成就）
    if (hours == 0 && achievements == 0 && is.null(first_achieve)) return("未开始")
    
    # 全成就
    if (!is.na(total_achievements) && achievements >= total_achievements) return("全成就")
    
    # 视成就进度超过40%的游戏为“已通关”（不完全准确）
    if (!is.na(total_achievements) && total_achievements > 0 && (achievements / total_achievements) >= 0.4) return("已通关")
    
    # 没有成就、未解锁任何成就、游玩时长很短的游戏，标记为“未开始”或“已弃坑”（不完全准确）
    if (achievements == 0 && is.null(first_achieve)) {
      if (!is.na(total_achievements) && hours <= 5 && total_achievements > 10) return("未开始")
      if (hours < 2) return("已弃坑")
    }
    
    # 成就解锁时间久远 + 时长短，可能属于“已弃坑”
    if (!is.null(last_played) && !is.null(first_achieve)) {
      days_between <- as.numeric(as.Date(last_played) - as.Date(first_achieve))
      if (days_between >= 180 && hours < 5 && (achievements / total_achievements) < 0.2) return("已弃坑")
    }
    
    # 没有成就系统的游戏，时长较高 + 最后游玩时间较久，也算“已通关”
    if (is.na(total_achievements) && hours >= 3 && (is.null(last_played) || as.numeric(today - as.Date(last_played)) > 30)) return("已通关")
   
    # 根据“最后游玩时间”判断是“游玩中”、“暂搁置”或“已弃坑”
    if (!is.null(last_played)) {
      days_since <- as.numeric(today - as.Date(last_played))
      if (days_since <= 30) return("游玩中")
      if (days_since <= 550) return("暂搁置")
      return("已弃坑")
    }
    
    # 有游戏时长但没有其他记录的默认处理
    if (hours > 0) return("已弃坑")
    
    # 其它极端情况默认认为“未开始”
    return("未开始")
  }
  
  return(list(get_game_status = get_game_status))
}
