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

# 日志输出工具，兼容 cli 控制台输出与日志写入文件
log_utils <- function() {
  info <- function(msg) cli::cli_alert_info("{msg}")
  success <- function(msg) cli::cli_alert_success("{msg}")
  warning <- function(msg) cli::cli_alert_warning("{msg}")
  error <- function(msg) cli::cli_alert_danger("{msg}")
  subtle <- function(msg) cli::cli_alert("{.subtle {msg}}")
  
  # 写入日志文件（默认文件名为 upload_log.txt）
  log_to_file <- function(msg, type = "INFO") {
    log_entry <- paste0(
	    format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), 
	    "[", toupper(type), "] ", # 转为大写以统一格式
	    msg, "\n") # 日志内容
    cat(log_entry, file = "upload_log.txt", append = TRUE)
  }
  
  list(
    info = info,
    success = success,
    warning = warning,
    error = error,
    subtle = subtle,
    log_to_file = log_to_file
  )
}

# 获取Notion中已存在页面
fetch_existing_game_pages <- function(database_id, token,
				      use_account_field = use_account_field_global) {
  url <- paste0("https://api.notion.com/v1/databases/", database_id, "/query")
  page_map <- list() # 存储结果：key 为 "游戏ID_账户"，value 为 page_id
  start_cursor <- NULL # 分页游标，用于遍历所有记录（Notion 有分页机制）
  repeat {
    # 若没有 start_cursor，则为首次查询，发送空 body；否则使用游标
    body_json <- if (is.null(start_cursor)) "{}" else toJSON(list(start_cursor = start_cursor), auto_unbox = TRUE)
    
    # 向 Notion 发送 POST 请求，获取一页数据
    res <- POST(url, add_headers(
      Authorization = paste("Bearer", token),
      "Content-Type" = "application/json",
      "Notion-Version" = "2022-06-28"
    ), body = body_json, encode = "raw")
    
    # 若响应状态非 200，则终止并报错
    if (status_code(res) != 200) stop("❌ 请求失败：", content(res, "text", encoding = "UTF-8"))
    
    # 解析返回的 JSON 内容
    content_json <- content(res, "parsed", encoding = "UTF-8")
    
    # 遍历当前页的所有记录
    for (page in content_json$results) {
      props <- page$properties
      game_id <- props$游戏ID$number # 从 Notion 属性中提取游戏 ID
      
      # 👇 根据是否使用账户字段处理 key
      key <- tryCatch({
        if (use_account_field) {
          account <- props$所属账户$select$name
          if (is.null(account) || account == "") account <- "未知"
          paste0(game_id, "_", account)
        } else {
          as.character(game_id)
        }
      }, error = function(e) {
        if (use_account_field) paste0(game_id, "_未知") # 如果 Notion 中旧数据没有所属账户字段也不会报错
        else as.character(game_id)
      })
      
      if (!is.null(game_id) && !is.null(key)) {
        page_map[[key]] <- page$id
      }
    }
    
    if (!isTRUE(content_json$has_more)) break
    start_cursor <- content_json$next_cursor
  }
  
  return(page_map)
} 

# 准备上传上下文：获取 page_id 映射、合并到数据框、输出摘要
prepare_upload_context <- function(df, database_id, token,
				   merge_page_id = TRUE,
				   use_account_field = use_account_field_global,
				   log = log_utils()) {
  
  # 确认数据框中是否已经有 page_id 字段，若有且非空，直接跳过 fetch
  if ("page_id" %in% colnames(df) && all(!is.na(df$page_id))) {
    msg <- sprintf("📋 使用已有 page_id 字段，跳过 Notion 查询，待上传：%d", nrow(df))
    log$info(msg)
    log$log_to_file(msg, type = "INFO")
    return(list(df = df, game_pages = NULL, total = nrow(df)))
  }
  
  # 🔁 否则才从 Notion 拉取 page_id 映射
  game_pages <- fetch_existing_game_pages(database_id, token,
					  use_account_field = use_account_field)
  total <- nrow(df)
  
  msg <- sprintf("📋 Notion 已有记录：%d，待上传：%d", length(game_pages), total)
  log$info(msg)
  log$log_to_file(msg, type = "INFO")
  
  if (merge_page_id) {
    df$key <- if (use_account_field) {
      paste0(df$游戏ID, "_", df$所属账户)
    } else {
      as.character(df$游戏ID)
    }
    
    page_df <- tibble::tibble(
      key = names(game_pages),
      page_id = unname(unlist(game_pages))
    )
    df <- dplyr::left_join(df, page_df, by = "key")
  }
  df$key <- NULL # 清除临时字段，确保 key 列不影响后续上传
  return(list(df = df, game_pages = game_pages, total = total))
}

# 主流程函数：将 Steam 数据上传至 Notion 数据库
run_upload_loop <- function(df, database_id, token,
                            insert_fields = insert_fields_global,
                            update_fields = update_fields_global,
                            is_main_game = is_main_game_global,
                            skip_playtime_web = skip_playtime_web_global) {
  
  # 字段检查
  check_field_consistency(
    database_id = database_id,
    token = token,
    insert_fields = insert_fields,
    update_fields = update_fields
  )                       
  
  start_time <- Sys.time() # 开始计时
  
  # 准备上传上下文，包括 Notion 已有 page_id 映射合并
  ctx <- prepare_upload_context(df, database_id, token)
  df <- ctx$df # 已合并 page_id
  game_pages <- ctx$game_pages
  total <- ctx$total # 总上传记录数
  
  log <- log_utils()	# 初始化日志工具
  
  success_count <- 0 # 记录成功上传数
  fail_count <- 0 # 记录失败上传数
  
  # 创建cli进度条
  cli::cli_progress_bar(
    name = "upload_bar",
    format = "上传中：{cli::pb_bar} {cli::pb_percent}",
    total = total,
    clear = FALSE
  )
  
  for (i in seq_len(nrow(df))) {
    row <- as.list(df[i, ])
    skip <- FALSE  # 标记是否跳过上传
    
    # 跳过无效记录（如无游戏名或全 NA）
    if (is.null(row$游戏名称) || is.na(row$游戏名称) || row$游戏名称 == "" || all(is.na(row))) {
      log$warning(glue::glue("⏩ [{i}/{total}] 跳过无效记录（无游戏名）"))
      skip <- TRUE
    }
    
    # 跳过非游戏本体（如 DLC、原声音轨）
    type <- tolower(safe$safe_text(row$内容类型))
    is_main <- grepl("游戏本体", type)
    if (is_main_game && !is_main) {
      game_name <- safe$safe_text(row$游戏名称)
      game_id_str <- safe$safe_text(row$游戏ID)
      display_name <- if (nchar(game_name) > 30) paste0(substr(game_name, 1, 27), "…") else game_name
      
      log$info(glue::glue("⏩ [{i}/{total}] 跳过非游戏本体：「{display_name}」（{row$内容类型}）"))
      skip <- TRUE
    }
    
    result <- list(success = FALSE, action = "跳过")
    
    if (!skip) {
      # 统一提取游戏相关变量
      game_name <- safe$safe_text(row$游戏名称)
      game_id_str <- safe$safe_text(row$游戏ID)
      display_name <- if (nchar(game_name) > 30) paste0(substr(game_name, 1, 27), "…") else game_name
      
      
      # 特殊处理：如果为“免费开玩”游戏，自动设置价格为“免费”
      if (grepl("免费开玩", safe$safe_text(row$商店标签))) {
        row$原价 <- "免费"
        row$当前价格 <- "免费"
        row$当前折扣 <- NULL
      }
      
      # 自动判断游戏游玩状态
      game_status <- status$get_game_status(row)
      
      page_id <- if ("page_id" %in% colnames(df) && !is.na(row$page_id)) row$page_id else NULL
      
      # 构建 Notion 所需属性（根据是否为插入，决定是否传入 insert_fields）
      if (is.null(page_id)) {
        props <- notion$build_notion_props(row, status = game_status, insert_fields = insert_fields, update_fields = update_fields)
      } else {
        props <- notion$build_notion_props(row, status = game_status, update_fields = update_fields)
      }
      
      # 跳过网页来源的不可靠字段
      props <- filter_props_for_web_source(
        props = props,
        row = row,
        update_fields = update_fields,
        skip_playtime_web = skip_playtime_web,
        log = log,
        i = i,
        total = total
      )
      
      # 上传或更新记录
      result <- notion_safe_upload(
        page_id = page_id, 
        props = props,
        database_id = database_id,
        token = token,
        is_main_game = is_main_game,
        update_fields = update_fields
      )
      
      account_str <- if (use_account_field_global) paste0("[", safe$safe_text(row$所属账户), "]") else ""
      
      # 上传成功/失败日志记录与图标标记
      if (result$success) {
        success_count <- success_count + 1
        mark_uploaded_page(result$page_id, token)
        log$success(glue::glue("✅ [{i}/{total}] {row$内容类型}「{display_name}」（{game_id_str}）{account_str}：{result$action}"))
        log$log_to_file(glue::glue("✅ [{i}/{total}] {row$内容类型}「{game_name}」（ID: {game_id_str}）{account_str}上传成功：{result$action}"), type = "SUCCESS")
      } else {
        fail_count <- fail_count + 1
        log$error(glue::glue("❌ [{i}/{total}] {row$内容类型}「{display_name}」（{game_id_str}）{account_str}：{result$action}"))
        log$log_to_file(glue::glue("❌ [{i}/{total}] {row$内容类型}「{game_name}」（{game_id_str}）{account_str}上传失败：{result$action}"), type = "ERROR")
      }
    }
    
    # 统一更新cli进度条（放循环底部，确保完整性）
    cli::cli_progress_update(set = i)
  }
  
  cli::cli_progress_done() # 关闭进度条
  end_time <- Sys.time() # 结束计时
  
  # 上传完成总结提示
  log$success(glue::glue("\n上传完成：✅ 成功 {success_count} 条，❌ 失败 {fail_count} 条 🎉"))
  
  # 输出总耗时
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  if (duration < 60) { # 小于1分钟则单位为秒
    log$info(glue::glue("⏱️ 总耗时：{round(duration)} 秒"))
  } else { # 大于1分钟则单位为“分钟+秒”
    minutes <- floor(duration / 60)
    seconds <- round(duration %% 60)
    log$info(glue::glue("⏱️ 总耗时：{minutes} 分 {seconds} 秒"))
  }
  # 最终写入日志文件
  log$log_to_file(glue::glue("上传完成：成功 {success_count} 条，失败 {fail_count} 条"), type = "INFO")
  log$log_to_file(glue::glue("总耗时：{round(difftime(end_time, start_time, units = 'secs'), 2)} 秒"), type = "INFO")
}
