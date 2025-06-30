# Steam-to-dataframe-to-Notion
## 借助了ChatGPT的帮助
自动抓取你的 Steam 游戏库信息，并上传至 Notion 数据库，包括游戏时长、成就、价格、商店标签、封面图等字段。

## 特性 Features
- 支持 Steam API + 网页抓取，自动合并数据
- 可抓取成就、时长、最后游玩日期等信息
- 不会重复插入已存在的游戏，只更新它
- 支持多字段上传到 Notion（如封面图、商店标签等）
- 支持根据按需更新字段功能（已解锁成就、游玩时间等）
- 可选择不上传DLC、Demo和原声音轨
- 针对网页抓取的无法获得游戏时间的游戏，会跳过游玩时间的更新
- 上传成功后可自动添加Steam图标
- 可初步判断游戏状态（游玩中、已弃坑等）
- 可导出 Excel / 上传 Notion / CLI 进度提示

# 上传状态展示：
## 若Notion中无该游戏（根据appid判断），则插入新页面
- ![image (3)](https://github.com/user-attachments/assets/8eebddab-5b39-491f-acc5-5392c03e19b5)
## 若该游戏已存在，则更新update_fields()中的内容
- ![image](https://github.com/user-attachments/assets/595b9173-ec6d-4d3b-a594-8ff5aa950501)
## 若该数据来源仅为网页，则不更新其游玩时间
- ![image (4)](https://github.com/user-attachments/assets/65b548b0-fef3-4b86-a5fa-d6f86ec0ada5)
## 支持仅上传游戏本体（不上传DLC、原声音轨等）
- ![image (1)](https://github.com/user-attachments/assets/5e14a902-faee-4084-a529-f1914ebfdbdf)

# 上传后的Notion示例：
![image](https://github.com/user-attachments/assets/7d2f431f-6cf9-4fbc-9b8d-ab67d67bf4b2)

## Notion中的成就进度函数
if(prop("已解锁成就")/prop("成就总数") == 1, "■■■■■■■■■■ 🏆", if(empty(prop("成就总数")), "此版本无成就", ((substring("■■■■■■■■■■", 0, floor(prop("已解锁成就")/prop("成就总数") *10))  + substring("☐☐☐☐☐☐☐☐☐☐", floor(prop("已解锁成就")/prop("成就总数") *10))+ " ")+ format(round(prop("已解锁成就")/prop("成就总数") *100))) + "%"))

