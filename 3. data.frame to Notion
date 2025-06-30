# === R/notion_upload.R ===
# Notion 上传模块（需 Notion API & Database ID）

notion_token <- "你的Notion Integration Token（API）"
database_id <- "你的32位Notion Database ID" 

# 包括：Notion已存在页面获取、判断更新/插入、初步判断游戏游玩状态等

#加载依赖
library(httr)  # 网络请求相关     
library(jsonlite)  # 数据处理  
library(cli)  # 命令行美化输出       
library(glue)  # 字符串拼接（提示消息、上传状态）

upload_steam_data_to_notion <- function(df = final_df, 
                                        database_id, 
                                        token,
                                        # 内容可根据需要更新需求更改
                                        update_fields = c("已解锁成就", 
                                                          "游玩时间",
                                                          "原价",
                                                          "当前价格",
                                                          "当前折扣"), 
                                        # 不上传DLC和原生音轨（要上传的话改成F）
                                        only_main_game = F) { 
  
  start_time <- Sys.time()  # ⏱️ 记录开始时间
  
  # 🛠️ 工具函数：将各种字段安全转换为 Notion 所需的格式
  # 🛠️ 例如 NULL → 空字符串，或处理成特定 JSON 结构
  safe_text <- function(x) if (is.null(x) || is.na(x)) "" else as.character(x)
  safe_number <- function(x) {
    if (is.null(x) || is.na(x) || x == "" || (is.character(x) && x %in% c("NA", "null"))) return(NULL)
    num <- suppressWarnings(as.numeric(x))
    if (is.na(num)) return(NULL)
    return(num)
  }
  safe_date <- function(x) {
    if (is.null(x) || is.na(x)) return(NULL)
    original_locale <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    try_formats <- c("%d %b, %Y", "%Y-%m-%d", "%Y/%m/%d", "%Y-%m-%d %H:%M:%S",
                     "%m/%d/%Y", "%Y.%m.%d", "%Y年%m月%d日")
    for (fmt in try_formats) {
      parsed <- tryCatch(as.Date(x, format = fmt), error = function(e) NA)
      if (!is.na(parsed)) {
        Sys.setlocale("LC_TIME", original_locale)
        return(format(parsed, "%Y-%m-%d"))
      }
    }
    Sys.setlocale("LC_TIME", original_locale)
    return(NULL)
  }
  
  # 封装 Notion 属性
  safe_text_prop <- function(x) if (is.null(x) || is.na(x)) NULL else list(rich_text = list(list(text = list(content = as.character(x)))))
  safe_title_prop <- function(x) if (is.null(x) || is.na(x)) NULL else list(title = list(list(text = list(content = as.character(x)))))
  safe_number_prop <- function(x) { val <- safe_number(x); if (is.null(val)) NULL else list(number = val) }
  safe_date_prop <- function(x) { date_val <- safe_date(x); if (is.null(date_val)) NULL else list(date = list(start = date_val)) }
  safe_status_prop <- function(x) { if (is.null(x) || is.na(x) || x == "") return(NULL); list(status = list(name = as.character(x))) }
  safe_select_prop <- function(x) {
    if (is.null(x) || is.na(x) || x == "") return(NULL)
    list(select = list(name = as.character(x)))
  }
  safe_multi_select_prop <- function(x) {
    if (is.null(x) || is.na(x) || x == "") return(NULL)
    tags <- unlist(strsplit(as.character(x), "[,，|/；;]"))
    tags <- trimws(tags)
    tags <- tags[tags != ""]
    if (length(tags) == 0) return(NULL)
    list(multi_select = lapply(tags, function(tag) list(name = tag)))
  }
  safe_file_prop <- function(url, name = "Steam Header") {
    if (is.null(url) || url == "" || is.na(url)) return(NULL)
    list(files = list(list(
      type = "external",
      name = name,
      external = list(url = url)
    )))
  }
  
  # 游戏状态推断函数（并不完全准确，还是需要自己在Notion中微调）
  get_game_status <- function(row) {
    type <- tolower(safe_text(row$内容类型))
    
    #只对游戏本体进行状态判断
    if (!grepl("游戏本体", type)) return(NULL) 
    
    hours <- safe_number(row$总时长小时)
    last_played <- safe_date(row$最后游玩)
    first_achieve <- safe_date(row$首个成就时间)
    achievements <- safe_number(row$已解锁成就)
    total_achievements <- safe_number(row$成就总数)
    
    if (is.null(hours)) hours <- 0
    if (is.null(achievements)) achievements <- 0
    if (is.null(total_achievements) || total_achievements == 0) total_achievements <- NA
    
    today <- Sys.Date()
    
    if (hours == 0 && achievements == 0 && is.null(first_achieve)) return("未开始")
    
    # 优先判断全成就
    if (!is.na(total_achievements) && achievements >= total_achievements) return("全成就")
    
    # 对于未全成就但已通关的游戏，可根据自己的成就情况更改比例
    if (!is.na(total_achievements) && total_achievements > 0 && (achievements / total_achievements) >= 0.4) return("已通关")
    
    # 尽量减少挂卡造成的虚假时长对游玩状态判断的影响
    if (achievements == 0 && is.null(first_achieve)) {
      if (!is.na(total_achievements) && hours <= 5 && total_achievements > 10) return("未开始")
      if (hours < 2) return("已弃坑")
    }
    
    if (!is.null(last_played) && !is.null(first_achieve)) {
      days_between <- as.numeric(as.Date(last_played) - as.Date(first_achieve))
      if (days_between >= 180 && hours < 5 && (achievements / total_achievements) < 0.2) return("已弃坑")
    } # 半年及以上没玩、游戏时长低于5小时、成就达成率小于20%视为已弃坑
    
    # 对一些没有成就的短篇游戏进行判断
    if (is.na(total_achievements) && hours >= 3 && (is.null(last_played) || as.numeric(today - as.Date(last_played)) > 30)) return("已通关")
    
    if (!is.null(last_played)) {
      days_since <- as.numeric(today - as.Date(last_played))
      if (days_since <= 30) return("游玩中")
      if (days_since <= 550) return("暂搁置") 
      return("已弃坑") # 超过一年半没再次游玩视为弃坑
    }
    if (hours > 0) return("已弃坑")
    return("未开始")
  }
  
  # 获取Notion中已存在的记录，用于判断是更新还是插入新页面
  fetch_existing_game_pages <- function(database_id, token) {
    url <- paste0("https://api.notion.com/v1/databases/", database_id, "/query")
    page_map <- list()
    start_cursor <- NULL
    repeat {
      body_json <- if (is.null(start_cursor)) "{}" else toJSON(list(start_cursor = start_cursor), auto_unbox = TRUE)
      res <- POST(url, add_headers(
        Authorization = paste("Bearer", token),
        "Content-Type" = "application/json",
        "Notion-Version" = "2022-06-28"
      ), body = body_json, encode = "raw")
      if (status_code(res) != 200) stop("❌ 请求失败：", content(res, "text", encoding = "UTF-8"))
      content_json <- content(res, "parsed", encoding = "UTF-8")
      for (page in content_json$results) {
        props <- page$properties
        game_id <- props$游戏ID$number
        page_id <- page$id
        if (!is.null(game_id)) page_map[[as.character(game_id)]] <- page_id
      }
      if (!isTRUE(content_json$has_more)) break
      start_cursor <- content_json$next_cursor
    }
    return(page_map)
  }
  
  game_pages <- fetch_existing_game_pages(database_id, token)
  total <- nrow(df)
  message(sprintf("📋 Notion 已有记录：%d，待上传：%d", length(game_pages), total))
  
  # 初始化进度条
  progress_id <- cli_progress_bar(
    name = "上传进度",
    total = total,
    format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent}",
    clear = FALSE
  )
  
  for (i in seq_len(total)) {
    row <- df[i, ]
    game_name <- safe_text(row$游戏名称)
    game_id_str <- safe_text(row$游戏ID)
    type <- tolower(safe_text(row$内容类型))
    is_main_game <- grepl("游戏本体", type)
    
    # 🆓 如果商店标签包含“免费开玩”，则价格设为“免费”，折扣设为NULL
    if (grepl("免费开玩", safe_text(row$商店标签))) {
      row$原价 <- "免费"
      row$当前价格 <- "免费"
      row$当前折扣 <- NULL  # 或 NA，也可以直接不上传
    }
    
    # 如果设置只上传游戏本体，则跳过 DLC/原声等
    if (only_main_game && !is_main_game) {
      cli_alert_info("⏩ [{i}/{total}] 跳过DLC和原声音轨：{game_name} ({game_id_str})")
      cli_progress_update(id = progress_id, set = i)
      next
    } 
    
    source <- tolower(trimws(safe_text(row$来源)))
    skip_playtime_web <- FALSE
    status <- if (is_main_game) get_game_status(row) else NULL
    status_str <- if (!is.null(status)) status else "N/A"
    
    # 生成完整上传字段列表，注意要匹配在Notion中填写的名称和属性
    full_props <- Filter(Negate(is.null), list(
      `游戏名称` = safe_title_prop(row$游戏名称),          	# 标题
      `游戏英文名` = safe_text_prop(row$游戏英文名),       		# 文本
      `游戏封面` = safe_file_prop(row$封面),               	# 文件和链接
      `游戏ID` = safe_number_prop(row$游戏ID),             	# 数字
      `游玩时间` = safe_text_prop(row$游玩时间),           	# 文本
      `总时长/h` = safe_number_prop(row$总时长小时),     		# 数字
      `成就总数` = safe_number_prop(row$成就总数),         	# 数字
      `已解锁成就` = safe_number_prop(row$已解锁成就),     		# 数字
      `开发商` = safe_text_prop(row$开发商),               	# 文本
      `发行商` = safe_text_prop(row$发行商),               	# 文本
      `原价` = safe_text_prop(row$原价),                   	# 文本
      `当前价格` = safe_text_prop(row$当前价格),           	# 文本
      `当前折扣` = safe_text_prop(row$当前折扣),              # 文本
      `游玩状态` = safe_status_prop(status),              	# 状态
      `商店标签` = safe_multi_select_prop(row$商店标签),   	# 多选
      `内容类型` = safe_select_prop(row$内容类型),         	# 选择
      `发售日期` = safe_date_prop(row$发售日期),           	# 日期
      `首个成就解锁于` = safe_date_prop(row$首个成就时间),   	# 日期
      `最后游玩日期` = safe_date_prop(row$最后游玩)         	# 日期
    ))
    
    props <- full_props
    
    if ("游玩时间" %in% update_fields && source == "网页" && is_main_game) {
      props$`游玩时间` <- NULL
      skip_playtime_web <- TRUE # 如果要更新游玩时间，则跳过网页抓取的游戏
    }
    
    props <- Filter(Negate(is.null), props)
    
    page_id <- game_pages[[game_id_str]]
    action_str <- ""
    upload_result <- "❌"
    
    tryCatch({
      # 判断是否已有 Notion 页面，决定是“更新”还是“插入”
      if (!is.null(page_id)) {
        # 更新页面（可指定更新内容）
        if (is_main_game) {
          if (!is.null(update_fields)) {
            props <- props[names(props) %in% update_fields]
          }
          res <- PATCH(paste0("https://api.notion.com/v1/pages/", page_id),
                       add_headers(Authorization = paste("Bearer", token),
                                   "Content-Type" = "application/json",
                                   "Notion-Version" = "2022-06-28"),
                       body = toJSON(list(properties = props), auto_unbox = TRUE, null = "null"))
          if (status_code(res) == 200) {
            action_str <- "🔁 更新"
            upload_result <- "✅"
          } else {
            action_str <- "⚠️ 更新失败"
            warning(sprintf("更新失败：%s (ID: %s)", game_name, game_id_str))
            warning(content(res, "text", encoding = "UTF-8"))
            page_id <- NULL
          }
        } else {
          action_str <- "⏩ 跳过非游戏本体"
          upload_result <- "✅"
        }
      } else { 
        # 插入新页面
        res <- POST("https://api.notion.com/v1/pages",
                    add_headers(Authorization = paste("Bearer", token),
                                "Content-Type" = "application/json",
                                "Notion-Version" = "2022-06-28"),
                    body = toJSON(list(parent = list(database_id = database_id), properties = props), auto_unbox = TRUE, null = "null"))
        if (status_code(res) == 200) {
          page_id <- fromJSON(content(res, "text", encoding = "UTF-8"))$id
          action_str <- "✅ 插入"
          upload_result <- "✅"
        } else {
          action_str <- "⚠️ 插入失败"
          warning(sprintf("插入失败：%s (ID: %s)", game_name, game_id_str))
          warning(content(res, "text", encoding = "UTF-8"))
          page_id <- NULL
        }
      }
      
    }, error = function(e) {
      cli_alert_danger("❌ 错误：{game_name} ({game_id_str}) - {e$message}")
    })
    
    # 每游戏上传后单独输出信息
    if (upload_result == "✅") {
      cli_alert_success("✔ [{i}/{total}]  {game_name} ({game_id_str}) ｜📌 {status_str}｜🔖 {action_str}")
    } else {
      cli_alert_danger("✖ [{i}/{total}]  {game_name} ({game_id_str}) ｜📌 {status_str}｜🔖 {action_str}")
    }
    
    # 上传成功后添加Steam图标 + “已上传”标记
    if (!is.null(page_id)) {
      PATCH(paste0("https://api.notion.com/v1/pages/", page_id),
            add_headers(Authorization = paste("Bearer", token),
                        "Content-Type" = "application/json",
                        "Notion-Version" = "2022-06-28"),
            body = toJSON(list(icon = list(type = "external", external = list(url = "https://upload.wikimedia.org/wikipedia/commons/8/83/Steam_icon_logo.svg"))), auto_unbox = TRUE))
      PATCH(paste0("https://api.notion.com/v1/pages/", page_id),
            add_headers(Authorization = paste("Bearer", token),
                        "Content-Type" = "application/json",
                        "Notion-Version" = "2022-06-28"),
            body = toJSON(list(properties = list(`已上传` = list(checkbox = TRUE))), auto_unbox = TRUE))
    }
    
    # 进度条推进
    cli_progress_update(id = progress_id, set = i)
    
    # ⏩ 跳过网页字段的提示
    if (skip_playtime_web && is_main_game && !is.null(page_id)) {
      cli_alert_info("⏩ [{i}/{total}] 跳过网页抓取游玩时间：{game_name} ({game_id_str})")
    }
    
    Sys.sleep(0.5)
  }
  
  # 所有游戏上传完成后，关闭进度条并输出用时
  cli_progress_done()
  end_time <- Sys.time()    # ⏱️ 记录结束时间
  time_used <- difftime(end_time, start_time, units = "secs")
  mins <- floor(as.numeric(time_used) / 60)
  secs <- round(as.numeric(time_used) %% 60)
  cli_alert_success(glue::glue("所有游戏上传完成 🎉 用时：{mins} 分 {secs} 秒"))
  
}

# ==== 🧪 上传测试 ====
# 以自己的游戏数量为准
df_test <- rbind(head(final_df,20), #前20行
                 final_df[380:400, ], #第380行-400行
                 tail(final_df, 20)) #最后20行

upload_steam_data_to_notion(
  df = df_test,
  database_id = database_id,
  token = notion_token
)

# ==== 🫣正式上传 ====
upload_steam_data_to_notion(
  df = final_df,
  database_id = database_id,
  token = notion_token
)

