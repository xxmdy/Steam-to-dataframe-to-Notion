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
