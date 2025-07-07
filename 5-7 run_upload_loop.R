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
