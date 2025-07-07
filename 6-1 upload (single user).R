# ==== 🧪 上传测试（单个用户） ====
df_test <- rbind(head(final_df,20), #前20行
                 final_df[380:400, ], #第380行-400行
                 tail(final_df, 20)) #最后20行

run_upload_loop(
  df = df_test,
  database_id = database_id,
  token = notion_token,
  insert_fields = insert_fields_global,
  update_fields = update_fields_global,
  is_main_game = is_main_game_global,
  skip_playtime_web = skip_playtime_web_global
)

# ==== 🫣正式上传（单个用户） ====
run_upload_loop(
  df = final_df,
  database_id = database_id,
  token = notion_token,
  insert_fields = insert_fields_global,
  update_fields = update_fields_global,
  is_main_game = is_main_game_global,
  skip_playtime_web = skip_playtime_web_global
)
