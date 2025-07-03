# ==== 🧪 上传测试（多用户） ====

# 假设现在已导出两个账户的数据，生成了final_df1和final_df2
# 为数据框添加“所属账户”列
final_df1$所属账户 <- "user1"
fina2_df1$所属账户 <- "user2"
df_all <- dplyr::bind_rows(final_df1, fina2_df1)

# 可根据自己的数据量和数据类型修改
df_all_test <- rbind(head(df_all , 20), #前20行
                  df_all [380:400, ], #第380行-400行
                  tail(df_all , 20)) #最后20行
                  
run_upload_loop(
  df = df_all_test,
  database_id = database_id,
  token = notion_token,
  update_fields = update_fields_global,
  is_main_game = is_main_game_global,
  skip_playtime_web = skip_playtime_web_global
)               

# ==== ❗正式上传❗ ====
final_df1$所属账户 <- "user1"
fina2_df1$所属账户 <- "user2"
df_all <- dplyr::bind_rows(final_df1, fina2_df1)

run_upload_loop(
  df = df_all,
  database_id = database_id,
  token = notion_token,
  update_fields = update_fields_global,
  is_main_game = is_main_game_global,
  skip_playtime_web = skip_playtime_web_global
)
