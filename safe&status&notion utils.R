# 工具函数：将各种字段安全转换为 Notion 所需的格式
safe_utils <- function() {
  safe_text <- function(x) {
  if (is.null(x) || is.na(x)) "" else as.character(x) # NULL/NA -> 空字符
	}
  safe_number <- function(x) {
    if (is.null(x) || is.na(x) || x == "" || (is.character(x) && x %in% c("NA", "null"))) return(NULL)
    num <- suppressWarnings(as.numeric(x))
    if (is.na(num)) return(NULL) # NULL/NA/空/非法字符 -> NULL
    return(num)
  }
  safe_date <- function(x) {
    if (is.null(x) || is.na(x)) return(NULL)
    original_locale <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", original_locale), add = TRUE)
    # 尽量识别各种日期格式
    formats <- c("%d %b, %Y", 
		 "%Y-%m-%d", 
		 "%Y/%m/%d",
		 "%Y-%m-%d %H:%M:%S",
		 "%m/%d/%Y",
		 "%Y.%m.%d",
		 "%Y年%m月%d日") 
    for (fmt in formats) {
      parsed <- tryCatch(as.Date(x, format = fmt), error = function(e) NA)
      if (!is.na(parsed)) {
        return(format(parsed, "%Y-%m-%d")) # 统一输出为“年-月-日”格式
      }
    }
    Sys.setlocale("LC_TIME", original_locale)
    return(NULL)
  }
  
  list(
    # 原始值转换函数
    safe_text = safe_text,
    safe_number = safe_number,
    safe_date = safe_date,
    # Notion 格式构造函数
    safe_text_prop = function(x) { # 文本
	    if (is.null(x) || is.na(x)) NULL 
	    else list(rich_text = list(list(text = list(content = as.character(x)))))
	  },
    safe_title_prop = function(x) { # 标题
	    if (is.null(x) || is.na(x)) NULL 
		  else list(title = list(list(text = list(content = as.character(x)))))
	  },
    safe_number_prop = function(x) { # 数字
	    val <- safe_number(x)
	    if (is.null(val)) NULL 
	    else list(number = val) 
	  },
    safe_date_prop = function(x) { # 日期
	    val <- safe_date(x)
	    if (is.null(val)) NULL 
	    else list(date = list(start = val)) 
	  },
    safe_status_prop = function(x) { # 状态
	    if (is.null(x) || is.na(x) || x == "") return(NULL)
	    list(status = list(name = as.character(x))) 
	  },
    safe_select_prop = function(x) { # 单选（选择）
      if (is.null(x) || is.na(x) || x == "") return(NULL)
      list(select = list(name = as.character(x)))
    },
    safe_multi_select_prop = function(x) { # 多选
      if (is.null(x) || is.na(x) || x == "") return(NULL)
      # 支持中英文逗号、中文顿号、竖线、斜杠、中英文分号
      tags <- trimws(unlist(strsplit(as.character(x), "[,，、|/；;]")))
      tags <- tags[tags != ""] # 去除空字符串
      if (length(tags) == 0) return(NULL)
      list(multi_select = lapply(tags, function(tag) list(name = tag)))
    },
    safe_file_prop = function(url, name = "Steam Header") {
      if (is.null(url) || is.na(url) || url == "") return(NULL)
      list(files = list(list(type = "external", name = name, external = list(url = url))))
    }
  )
}	   

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

# Notion 属性构造函数
notion_utils <- function(safe) {
  build_notion_props <- function(row, status = NULL,
				 insert_fields = insert_fields_global,
				 update_fields = update_fields_global) {
    props <- list(
      `游戏名称` = safe$safe_title_prop(row$游戏名称),        # 标题类型
      `游戏英文名` = safe$safe_text_prop(row$游戏英文名),     # 文本类型
      `游戏封面` = safe$safe_file_prop(row$封面),             # 文件/链接类型
      `游戏ID` = safe$safe_number_prop(row$游戏ID),           # 数字类型
      `游玩时间` = safe$safe_text_prop(row$游玩时间),         # 文本类型
      `总时长/h` = safe$safe_number_prop(row$总时长小时),     # 数字类型
      `总成就数` = safe$safe_number_prop(row$成就总数),       # 数字类型
      `已解锁成就` = safe$safe_number_prop(row$已解锁成就),   # 数字类型
      `开发商` = safe$safe_text_prop(row$开发商),             # 文本类型
      `发行商` = safe$safe_text_prop(row$发行商),             # 文本类型
      `原价` = safe$safe_text_prop(row$原价),                 # 文本类型
      `当前价格` = safe$safe_text_prop(row$当前价格),         # 文本类型
      `当前折扣` = safe$safe_text_prop(row$当前折扣),         # 文本类型
      `游玩状态` = safe$safe_status_prop(status),            # 状态类型
      `商店标签` = safe$safe_multi_select_prop(row$商店标签), # 多选类型
      `内容类型` = safe$safe_select_prop(row$内容类型),       # 选择类型
      `发售日期` = safe$safe_date_prop(row$发售日期),         # 日期类型
      `首个成就解锁于` = safe$safe_date_prop(row$首个成就时间),# 日期类型
      `最后游玩日期` = safe$safe_date_prop(row$最后游玩),     # 日期类型
      `所属账户` = safe$safe_select_prop(row$所属账户)         # 选择类型
    )

    # 如果设置了插入字段，则进行字段筛选
    if (!is.null(insert_fields)) {
      keep_fields <- union(insert_fields, update_fields)  # ✅ 保留后续可能更新的字段
	    props <- props[names(props) %in% keep_fields]
    }

    # 过滤 NULL 字段，返回
    return(Filter(Negate(is.null), props))
  }

  return(list(build_notion_props = build_notion_props))
}

# 工具模块初始化
safe <- safe_utils() # 各类字段转换工具
status <- status_utils(safe) # 状态判断器
notion <- notion_utils(safe) # Notion 属性构造器
