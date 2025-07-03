# Notion 属性构造函数
notion_utils <- function(safe) {
  build_notion_props <- function(row, status = NULL) {
    Filter(Negate(is.null), list(  # 自动去除 NULL，避免上传错误
      `游戏名称` = safe$safe_title_prop(row$游戏名称), # 标题类型
      `游戏英文名` = safe$safe_text_prop(row$游戏英文名), # 文本类型
      `游戏封面` = safe$safe_file_prop(row$封面), # 文件和链接类型
      `游戏ID` = safe$safe_number_prop(row$游戏ID), # 数字类型
      `游玩时间` = safe$safe_text_prop(row$游玩时间), # 文本类型
      `总时长/h` = safe$safe_number_prop(row$总时长小时), # 数字类型
      `成就总数` = safe$safe_number_prop(row$成就总数), # 数字类型
      `已解锁成就` = safe$safe_number_prop(row$已解锁成就), # 数字类型
      `开发商` = safe$safe_text_prop(row$开发商), # 文本类型
      `发行商` = safe$safe_text_prop(row$发行商), # 文本类型
      `原价` = safe$safe_text_prop(row$原价), # 文本类型
      `当前价格` = safe$safe_text_prop(row$当前价格), # 文本类型
      `当前折扣` = safe$safe_text_prop(row$当前折扣), # 文本类型
      `游玩状态` = safe$safe_status_prop(status), # 状态类型
      `商店标签` = safe$safe_multi_select_prop(row$商店标签), # 多选类型
      `内容类型` = safe$safe_select_prop(row$内容类型), # 选择类型
      `发售日期` = safe$safe_date_prop(row$发售日期), # 日期类型
      `首个成就解锁于` = safe$safe_date_prop(row$首个成就时间), # 日期类型
      `最后游玩日期` = safe$safe_date_prop(row$最后游玩), # 日期类型
      `所属账户` = safe$safe_select_prop(row$所属账户) # 选择类型
    ))
  }
  
  return(list(build_notion_props = build_notion_props))
}

# 工具模块初始化
safe <- safe_utils() # 各类字段转换工具
status <- status_utils(safe) # 状态判断器
notion <- notion_utils(safe) # Notion 属性构造器
