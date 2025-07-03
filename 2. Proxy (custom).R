# ⚠️需要梯子：设置代理（适用于 Clash / Surge）
# 不需要设置代理即可稳定连接，可无视这一步
Sys.setenv(http_proxy = "http://127.0.0.1:7890", # Clash的默认设置，根据自己的代理地址改
           https_proxy = "http://127.0.0.1:7890") # Clash的默认设置，根据自己的代理地址改

# 若连接失败，测试代理地址（成功可无视）
# 先关闭代理
Sys.unsetenv("http_proxy") 
Sys.unsetenv("https_proxy") 

# 查看本机真实IP
res <- GET("https://httpbin.org/ip")
jsonlite::fromJSON(content(res, "text", encoding = "UTF-8"))

# 测试1
# 验证代理是否生效
res <- GET("https://httpbin.org/ip", use_proxy("127.0.0.1", 7890)) # use_proxy仅支持HTTP
jsonlite::fromJSON(content(res, "text", encoding = "UTF-8")) # 若成功，返回非本机出口IP

# 测试2
# 构建请求 URL
url <- paste0(
  "https://api.steampowered.com/IPlayerService/GetOwnedGames/v1/",
  "?key=", API_key,
  "&steamid=", steam_id,
  "&include_appinfo=1",
  "&include_played_free_games=1"
)

cat("🔍 正在测试连接 Steam API...\n")

# ⏱️ 发送请求
res <- tryCatch({
  GET(url, use_proxy("127.0.0.1", 7890))  # ⚠️ 若未开代理可删除 use_proxy
}, error = function(e) e)

# 结果判断
if (inherits(res, "response") && status_code(res) == 200) {
  json <- fromJSON(content(res, "text", encoding = "UTF-8"))
  game_count <- json$response$game_count
  cat("✅ 成功连接 Steam API，游戏总数：", game_count, "\n")
  
  # 输出前几个游戏名称和游玩时长
  games <- json$response$games
  if (!is.null(games)) {
    head_df <- head(games[, c("appid", "name", "playtime_forever")])
    print(head_df)
  } else {
    cat("⚠️ 没有返回游戏列表，可能该账号设置了隐私。\n")
  }
  
} else if (inherits(res, "response")) {
  cat("❌ 请求失败，状态码：", status_code(res), "\n")
  cat("响应内容：\n", content(res, "text", encoding = "UTF-8"), "\n")
} else {
  cat("❌ 无法连接 Steam API：", res$message, "\n")
}
