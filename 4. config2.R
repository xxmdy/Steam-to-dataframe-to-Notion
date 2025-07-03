# 配置信息
notion_token <- "你的Notion API，保留双引号" # 以防前面忘记赋值（若已输入，可无视）
database_id <- "你的32位Notion Database ID，保留双引号" # 以防前面忘记赋值（若已输入，可无视）

update_fields_global <- NULL # NULL为更新所有字段
														 # 若要更新指定字段，请将NULL修改为c("要更新的字段1", "要更新的字段2", ...)
														 # 举个例子：	c("已解锁成就", "最后游玩日期", "游玩时间")
is_main_game_global <- TRUE   	# TRUE为只上传游戏本体，不上传DLC、试玩版和原生音轨
skip_playtime_web_global <- TRUE # 跳过网页来源的不靠谱数据
use_account_field_global <- FALSE  # 是否启用“所属账户”字段作为上传识别key的一部分
																	 # 若要将多个账号信息传到一个Notion库，
																	 # 请将这个参数改为TRUE
