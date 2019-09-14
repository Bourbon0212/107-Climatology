# 107-Climatology
Final project: Seasonal Variation of Pollutants in Taiwan

* [Final Poster](https://gntuedutw-my.sharepoint.com/:b:/g/personal/b06208001_g_ntu_edu_tw/EZ96eIaHnrdLhs5svn0PYe0BfVhRTyEdWRmRBQuyCa_LQQ?e=0hn00P)

## Description
* `mean_map.R`: 最後報告中呈現的圖表， `stat_summary_2D` 將每個網格中的測站資料平均後上色。  
  + Based on `ggplot`
* `heat_map.R`: 圖表類型與所選資料錯誤，比較偏向"測站"熱度圖，而非"污染物"熱度圖，最終不採用。
  + Based on `ggplot`
* `interp_map`: 測站點位過於稀疏，全臺灣插值後繪製結果難以解釋，最終不採用。
  + Based on `base`
