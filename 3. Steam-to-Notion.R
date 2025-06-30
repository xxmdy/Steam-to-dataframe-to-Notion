# ==== Steam API和Steam ID配置 ====
API_key <- "你的32位Steam API"
steam_id <- "你的17位Steam ID"

# ==== Notion API和Database ID（如果只想手动导入，可不获取） ====
notion_token <- "你的Notion API"
database_id <- "你的32位Notion Database ID"

# 安装需要的R包
install.packages(c(
  "httr2", "httr", "jsonlite", "dplyr", "tibble", "lubridate",
  "openxlsx", "xml2", "rvest", "cli", "glue"
))

# ==== 导出Steam数据需要的包 ====
library(httr2) # 网络请求相关
library(httr) # 网络请求相关

library(rvest) # 网页和 XML 解析
library(xml2) # 网页和 XML 解析

library(dplyr) # 数据处理
library(tibble) # 数据处理
library(jsonlite) # 数据处理

library(lubridate) # 时间处理
library(openxlsx) # Excel 导出

# ==== 将导出数据上传至Notion所需要的包（如果只想手动导入，可不加载）====
library(httr)  # 网络请求相关     
library(jsonlite)  # 数据处理  
library(cli)  # 命令行美化输出       
library(glue)  # 字符串拼接（提示消息、上传状态）


# ==== 🪝 R 语言实现Steam 游戏数据抓取 ====

# ⚠️需要梯子：设置 HTTP 代理（适用于 Clash / Surge）
Sys.setenv(http_proxy = "http://127.0.0.1:7890", # Clash的默认设置，根据自己的代理地址改
           https_proxy = "http://127.0.0.1:7890") # Clash的默认设置，根据自己的代理地址改

# ==== 🏪 获取商店信息 ====
get_store_info <- function(appid) {
  
  # 辅助函数：根据地区与语言抓取数据
  fetch_store_data <- function(appid, cc, lang) {
    api_url <- paste0("https://store.steampowered.com/api/appdetails?appids=", appid,
                      "&cc=", cc, "&l=", lang)
    res <- tryCatch({
      request(api_url) |> req_perform() |> resp_body_json()
    }, error = function(e) {
      message(sprintf("❌ [%s] API 请求失败：%s", cc, e$message))
      return(NULL)
    })
    
    if (is.null(res)) return(NULL)
    
    entry <- res[[as.character(appid)]]
    
    if (!is.null(entry) && isTRUE(entry$success) && !is.null(entry$data)) {
      return(entry$data)
    } else {
      return(NULL)
    }
  }
  
  # 优先尝试中国区
  data <- fetch_store_data(appid, "cn", "zh-cn")
  
  if (is.null(data)) {
    message(sprintf("⚠️ AppID [%s] 在中国区不可用，尝试美国区", appid))
    data <- fetch_store_data(appid, "us", "en")
    region <- "US"
  } else {
    region <- "CN"
  }
  
  if (is.null(data)) {
    message(sprintf("❌ AppID [%s] 在所有区域都不可用", appid))
    return(list(NA, NA, NA, NA, NA, NA, NA, NA, appid, region, NA))
  }
  
  # 商店页面 HTML，尝试获取中文名与标签
  page_url <- paste0("https://store.steampowered.com/app/", appid, "/?l=schinese")
  headers <- c("Cookie" = "birthtime=568022401; lastagecheckage=1-January-1988")
  
  html <- tryCatch({
    read_html(httr::GET(page_url, httr::add_headers(.headers = headers)))
  }, error = function(e) NA)
  
  # 提取商店标签
  tags <- tryCatch({
    if (!inherits(html, "xml_missing") && !is.na(html)) {
      tag_nodes <- html_elements(html, ".glance_tags.popular_tags a.app_tag")
      tag_text <- html_text(tag_nodes, trim = TRUE)[1:min(5, length(tag_nodes))] # 只抓取前5个商店标签
      paste(tag_text, collapse = ", ")
    } else {
      ""
    }
  }, error = function(e) {
    message(sprintf("⚠️ [标签提取失败] AppID %s: %s", appid, e$message))
    ""
  })
  
  # 提取中文标题（如果有）
  cn_title <- tryCatch({
    if (!inherits(html, "xml_missing") && !is.na(html)) {
      html_title <- html_element(html, ".apphub_AppName")
      html_text(html_title, trim = TRUE)
    } else {
      NA_character_
    }
  }, error = function(e) NA_character_)
  
  # 提取其余字段
  dev       <- tryCatch(data$developers[[1]], error = function(e) NA) #开发商
  pub       <- tryCatch(data$publishers[[1]], error = function(e) NA) #发行商
  released  <- tryCatch(data$release_date$date, error = function(e) NA) #发售日期
  
  # 获取价格和币种
  price_initial <- tryCatch(data$price_overview$initial, error = function(e) NA) #原价
  price_final <- tryCatch(data$price_overview$final, error = function(e) NA) #当前价格
  currency <- tryCatch(data$price_overview$currency, error = function(e) NA) #币种
  
  # 自动根据币种格式化价格
  format_price <- function(price, currency) {
    if (is.null(price) || is.na(price)) return("未知")
    symbol <- switch(currency,
                     "CNY" = "￥", 
                     "USD" = "$", 
                     "EUR" = "€", 
                     "GBP" = "£", 
                     "")
    paste0(symbol, sprintf("%.2f", price / 100))
  }
  
  price_origin <- format_price(price_initial, currency) # 原价（带单位）
  price_now     <- format_price(price_final, currency) # 当前价格（带单位）
  
  # 折扣力度
  discount  <- tryCatch(data$price_overview$discount_percent, error = function(e) NA) 
  discount_txt <- if (!is.null(discount) && length(discount) > 0 && !is.na(discount)) paste0("-", discount, "%") else "无折扣"
  
  content   <- tryCatch(data$type, error = function(e) NA) # 数据来源
  
  message(sprintf("✅ [%s] 获取成功（区域：%s）：%s", appid, region, cn_title))
  
  return(list(
    dev, pub, released, price_now, 
    discount_txt, tags, content, cn_title, 
    appid, region, price_origin))
}


# ==== 🏆 获取成就信息 ====
get_achievements <- function(appid) {
  # 构建 API 请求 URL
  url <- paste0("https://api.steampowered.com/ISteamUserStats/GetPlayerAchievements/v1/?key=",
                API_key, "&steamid=", steam_id, "&appid=", appid)
  
  # 执行请求，失败返回 NULL
  res <- tryCatch({
    request(url) |> req_options(timeout = 20) |> req_perform() |> resp_body_json()
  }, error = function(e) return(NULL))
  
  # 判断成就结构，可能为 NULL 或空
  achs <- res$playerstats$achievements
  if (is.null(achs) || !is.list(achs)) return(NULL)
  
  # 将成就数据转换为 data.frame
  bind_rows(achs)
}

# ==== 🎮 获取拥有的游戏列表（支持 API Key 和 Steam ID） ====
get_owned_games <- function(API_key, steam_id) {
  
  url <- paste0("https://api.steampowered.com/IPlayerService/GetOwnedGames/v1/?key=",
                API_key, "&steamid=", steam_id, "&include_appinfo=1")
  
  res <- tryCatch({
    json_raw <- request(url) |>
      req_options(timeout = 30) |>
      req_retry(max_tries = 3) |>
      req_perform() |>
      resp_body_string()
    
    fromJSON(json_raw)
  }, error = function(e) {
    message("❌ get_owned_games() 错误：", e$message)
    return(NULL)
  })
  
  if (is.null(res$response$games)) return(tibble())
  
  games <- res$response$games
  df <- bind_rows(games) |> 
    select(appid, name, playtime_forever, rtime_last_played)
  
  return(df)
}

# ==== 🔍 使用 XML 页面方式抓取完整游戏列表（适配静态网页） ====
# 推荐使用官方 API 获取游玩时间和最后游玩时间（get_owned_games）
get_all_games_web <- function(user_id = steam_id) {
  
  base_url <- if (grepl("^\\d+$", user_id)) {
    paste0("https://steamcommunity.com/profiles/", user_id, "/games?xml=1")
  } else {
    paste0("https://steamcommunity.com/id/", user_id, "/games?xml=1")
  }
  
  page <- tryCatch(read_xml(base_url), error = function(e) {
    message("❌ 无法访问 Steam XML 游戏库：", e$message)
    return(NULL)
  })
  
  if (is.null(page)) return(data.frame())
  
  game_nodes <- xml_find_all(page, ".//games/game")
  if (length(game_nodes) == 0) {
    message("⚠️ XML 页面无游戏数据")
    return(data.frame())
  }
  
  # 逐个游戏提取字段
  df <- tibble(
    appid = as.integer(xml_text(xml_find_all(game_nodes, "appID"))), #获取游戏ID
    name_web = xml_text(xml_find_all(game_nodes, "name")), # 获取网页抓取的游戏名称
    hours_total_web = sapply(game_nodes, function(node) { 
      txt <- xml_text(xml_find_first(node, "hoursOnRecord")) # 获取网页抓取的游戏时间（小时）
      ifelse(nzchar(txt), as.numeric(gsub(",", "", txt)), NA_real_)
    })
  )
  
  message(sprintf("✅ 成功抓取 XML 游戏库：共 %d 个游戏", nrow(df)))
  return(df)
}

# ==== 🈴 合并网页与 API 获取的游戏列表 ====
merge_api_and_web_games <- function(api_games, web_games) {
  write.csv(api_games, "E:/api_games_debug.csv", row.names = FALSE)
  write.csv(web_games, "E:/web_games_debug.csv", row.names = FALSE)
  
  # 从网页抓取中只保留游戏ID、网页中的游戏名和网页时长（小时）
  web_games <- web_games |> select(appid, name_web, hours_total_web)
  
  combined <- full_join(api_games, web_games, by = "appid")
  
  # 如果 API 的playtime_forever（分钟）缺失，尝试使用网页时长 * 60 补足
  combined <- combined |> mutate(
    playtime_forever = ifelse(
      is.na(playtime_forever) & !is.na(hours_total_web),
      round(hours_total_web * 60),
      playtime_forever
    ),
    name = coalesce(name, name_web), # 优先使用 API 名称，否则网页名
    source = case_when(
      !is.na(playtime_forever) & !is.na(name_web) ~ "两者都有",
      !is.na(playtime_forever)                   ~ "API",
      !is.na(name_web)                           ~ "网页",
      TRUE                                       ~ "未知"
    )
  )
  
  # 移除网页字段，避免后续干扰
  combined <- combined |> select(appid, name, source, playtime_forever, rtime_last_played)
  
  return(combined)
}

# API抓取数据
api_games <- get_owned_games(API_key, steam_id)

# 网页抓取数据
web_games <- get_all_games_web()

# 合并数据
games <- merge_api_and_web_games(api_games, web_games)



# === 📦 主流程封装函数 ===
fetch_and_merge_all_games <- function(api_key = API_key, user_id = steam_id) {
  message("📦 正在获取 Steam 游戏数据...")
  api_games <- tryCatch({
    get_owned_games(api_key, user_id)
  }, error = function(e) {
    message("❌ API 获取失败：", e$message)
    data.frame()
  })
  
  web_games <- get_all_games_web(user_id)
  games <- merge_api_and_web_games(api_games, web_games)
  message(sprintf("🎮 合并完成：共 %d 个游戏", nrow(games)))
  return(games)
}

# ==== 📊 主处理逻辑 ====
games <- fetch_and_merge_all_games() # 获得API+网页抓取数据
games <- distinct(games, appid, .keep_all = TRUE) #去掉重复（若有）
result <- list()

# 如果担心出错的话可以先跑21行试试，用test替换下面的games
# 例如：games_test <- games[30:50, ] #选取第20行-50行的数据

for (i in seq_len(nrow(games))) { 
  game <- games[i, ]
  appid <- game$appid
  cat(sprintf("[%d/%d] 正在处理：%s\n", i, nrow(games), game$name))
  
  mins <- game$playtime_forever
  hours <- if (!is.na(mins)) round(mins / 60, 2) else NA_real_
  
  playtime_display <- if (is.na(mins) || mins == 0) {
    "0"
  } else if (mins < 60) {
    paste0(mins, " 分钟") # 若游玩时间不超过1小时，则时间单位为”分钟“
  } else {
    paste0(hours, " 小时") 
  }
  
  last_played <- if (!is.na(mins) && mins > 0 && !is.na(game$rtime_last_played)) {
    as_datetime(game$rtime_last_played)
  } else {
    NA
  }
  
  ach <- get_achievements(appid)
  if (is.null(ach)) {
    total_ach <- 0
    unlocked_ach <- 0
    first_ach_time <- "此版本无成就"
  } else {
    total_ach <- nrow(ach)
    unlocked_ach <- sum(ach$achieved == 1)
    times <- ach$unlocktime[ach$achieved == 1]
    first_ach_time <- if (length(times) > 0) as.character(as_datetime(min(times))) else NA
  }
  
  store <- get_store_info(appid)
  cover_url <- paste0("https://cdn.cloudflare.steamstatic.com/steam/apps/", appid, "/header.jpg")
  cn_name <- if (!is.null(store[[8]]) && !is.na(store[[8]])) store[[8]] else game$name
  en_name <- if (!is.null(game$name) && !is.na(game$name) && grepl("[A-Za-z]", game$name) && (is.null(store[[8]]) || is.na(store[[8]]) || store[[8]] != game$name)) game$name else NA_character_
  
  result[[i]] <- tibble(
    游戏名称 = cn_name,
    游戏英文名 = en_name,
    游戏ID = appid,
    游玩时间 = playtime_display,
    总时长小时 = hours,
    最后游玩 = last_played,
    成就总数 = total_ach,
    已解锁成就 = unlocked_ach,
    首个成就时间 = first_ach_time,
    开发商 = store[[1]],
    发行商 = store[[2]],
    发售日期 = store[[3]],
    原价 = store[[11]],
    当前价格 = store[[4]],
    当前折扣 = store[[5]],
    商店标签 = store[[6]],
    来源 = game$source,
    内容类型 = case_when(
      store[[7]] == "game"  ~ "游戏本体",
      store[[7]] == "dlc"   ~ "DLC",
      store[[7]] == "music" ~ "原声音轨",
      store[[7]] == "demo"  ~ "试玩版",
      TRUE                  ~ "其它"
    ),
    封面 = cover_url
  )
  Sys.sleep(1)
}

# 合并
# 这个final_df也是后面用来上传Notion所需要的数据
final_df <- bind_rows(result) 

# ==== 💾 导出 Excel ====
write.xlsx(final_df, file = "E:/Steam Data 2.xlsx", encoding = "UTF-8") #保存在E盘，可以根据自己需求改


# ==== 🆙 上传到Notion ====
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
  # 🛠 例如 NULL → 空字符串，或处理成特定 JSON 结构
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
      `当前折扣` = safe_text_prop(row$当前折扣),              	# 文本
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
