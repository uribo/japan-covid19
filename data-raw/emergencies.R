#############################
# 緊急事態宣言及びまん延防止等重点措置の発令の記録
# 2021-09-18時点
#############################
library(tidyverse)
library(lubridate)

# 緊急事態宣言 ------------------------------------------------------------------
df1_202004fin <- tibble(
  area = c("千葉県", "神奈川県", "東京都", "埼玉県",
           "大阪府", "兵庫県",
           "福岡県",
           "北海道",
           "京都府",
           "青森県", "岩手県", "宮城県", "秋田県", "山形県", 
           "福島県", "茨城県", "栃木県", "群馬県", "新潟県", 
           "富山県", "石川県", "福井県", "山梨県", "長野県", 
           "岐阜県", "静岡県", "愛知県", "三重県", "滋賀県", 
           "奈良県", "和歌山県", "鳥取県", "島根県", "岡山県", 
           "広島県", "山口県", "徳島県", "香川県", "愛媛県", 
           "高知県", "佐賀県", "長崎県", "熊本県", "大分県", 
           "宮崎県", "鹿児島県", "沖縄県"),
  type = "prefecture",
  date_from = c(rep(ymd("2020-04-07"), 7),
                rep(ymd("2020-04-16"), 40)),
  date_to = c(rep(ymd("2020-05-25"), 4),
              rep(ymd("2020-05-21"), 2),
              ymd("2020-05-14"),
              ymd("2020-05-25"),
              ymd("2020-05-21"),
              rep(ymd("2020-05-14"), 38)))
df2_202101fin <- tibble(
  area = c("千葉県", "神奈川県", "埼玉県", "東京都",
           "栃木県", 
           "愛知県", "岐阜県", "京都府", "大阪府", "兵庫県", "福岡県"),
  type = "prefecture",
  date_from = c(rep(ymd("2021-01-08"), 4),
                ymd("2021-01-14"),
                rep(ymd("2021-01-14"), 6)),
  date_to = c(rep(ymd("2021-03-21"), 4),
              ymd("2021-02-07"),
              rep(ymd("2021-02-28"), 6)))
df3_202104fin <- tibble(
  area = c("東京都", "大阪府", "京都府", "兵庫県",
           "愛知県", "福岡県",
           "北海道", "岡山県", "広島県",
           "沖縄県"),
  type = "prefecture",
  date_from = c(rep(ymd("2021-04-25"), 4),
                rep(ymd("2021-05-12"), 2),
                rep(ymd("2021-05-16"), 3),
                ymd("2021-05-23")),
  date_to = c(rep(ymd("2021-06-20"), 9),
              ymd("2021-09-30"))
)
df4_202107current <- tibble(
  area = c("東京都", 
           "埼玉県", "千葉県", "神奈川県", "大阪府", 
           "茨城県", "栃木県", "群馬県", "静岡県", "京都府", "兵庫県", "福岡県",
           "宮城県", "岡山県", 
           "北海道", "岐阜県", "愛知県", "三重県", "滋賀県", "広島県"),
  type = "prefecture",
  date_from = c(ymd("2021-07-12"),
                rep(ymd("2021-08-02"), 4),
                rep(ymd("2021-08-20"), 7),
                rep(ymd("2021-08-27"), 8)),
  date_to = c(rep(ymd("2021-09-30"), 12),
              rep(ymd("2021-09-12"), 2),
              rep(ymd("2021-09-30"), 6))
)

df_kinkyu_all <-
  bind_rows(df1_202004fin,
            df2_202101fin,
            df3_202104fin,
            df4_202107current)

df_kinkyu_all %>% 
  write_csv(here::here("data-raw/緊急事態宣言の発令.csv"))


# まん延防止等重点措置 --------------------------------------------------------------
df_manbou_all <- tribble(
  ~prefecture, ~area, ~date_from, ~date_to,
  "北海道", "札幌市", ymd("2021-05-09"), ymd("2021-05-15"),
  "北海道", "札幌市", ymd("2021-06-21"), ymd("2021-07-11"),
  "北海道", "札幌市、江別市、千歳市、恵庭市、北広島市、石狩市、石狩郡当別町、石狩郡新篠津村、小樽市、旭川市", ymd("2021-08-14"), ymd("2021-08-26"),
  "宮城県", "仙台市", ymd("2021-04-05"), ymd("2021-05-11"),
  "宮城県", "仙台市", ymd("2021-08-20"), ymd("2021-08-26"),
  "宮城県", "仙台市", ymd("2021-09-13"), ymd("2021-09-30"),
  "福島県", "いわき市", ymd("2021-08-08"), ymd("2021-09-30"),
  "福島県", "郡山市", ymd("2021-08-23"), ymd("2021-09-30"),
  "福島県", "福島市", ymd("2021-08-26"), ymd("2021-09-30"),
  "茨城県", "水戸市、土浦市、古河市、石岡市、結城市、龍ケ崎市、下妻市、常総市、常陸太田市、北茨城市、笠間市、取手市、牛久市、つくば市、ひたちなか市、鹿嶋市、潮来市、守谷市、常陸大宮市、那珂市、筑西市、坂東市、稲敷市、かすみがうら市、桜川市、神栖市、行方市、鉾田市、つくばみらい市、小美玉市、東茨城郡茨城町、那珂郡東海村、稲敷郡美浦村、稲敷郡阿見町、結城郡八千代町、猿島郡五霞町、猿島郡境町、北相馬郡利根町", ymd("2021-08-08"), ymd("2021-08-19"), # 38cities
  "茨城県", "日立市、大洗町、城里町、大子町、河内町", ymd("2021-08-15"), ymd("2021-08-19"),
  "栃木県", "宇都宮市、足利市、栃木市、佐野市、鹿沼市、日光市、小山市、真岡市、大田原市、矢板市、那須塩原市、さくら市、那須烏山市、下野市、河内郡上三川町、芳賀郡益子町、芳賀郡市貝町、芳賀郡芳賀町、下都賀郡壬生町、下都賀郡野木町、塩谷郡塩谷町、塩谷郡高根沢町、那須郡那須町", ymd("2021-08-08"), ymd("2021-08-19"),
  "栃木県", "茂木市", ymd("2021-08-16"), ymd("2021-08-19"),
  "群馬県", "前橋市、高崎市、伊勢崎市、太田市、沼田市、渋川市、藤岡市、富岡市、安中市、佐波郡玉村町", ymd("2021-05-16"), ymd("2021-06-13"),
  "群馬県", "前橋市、高崎市、桐生市、伊勢崎市、太田市、沼田市、館林市、渋川市、藤岡市、富岡市、安中市、みどり市、北群馬郡榛東村、北群馬郡吉岡町、佐波郡玉村町、邑楽郡板倉町、邑楽郡明和町、邑楽郡千代田町、邑楽郡大泉町、邑楽郡邑楽町", ymd("2021-08-08"), ymd("2021-08-19"),
  "埼玉県", "さいたま市、川口市", ymd("2021-04-20"), ymd("2021-08-01"),
  "埼玉県", "川越市、所沢市、草加市、越谷市、蕨市、戸田市、朝霞市、志木市、和光市、新座市、富士見市、ふじみ野市、入間郡三芳町", ymd("2021-04-28"), ymd("2021-06-20"),
  "埼玉県", "川越市、所沢市、草加市、越谷市、蕨市、戸田市、朝霞市、志木市、和光市、新座市、富士見市、ふじみ野市、入間郡三芳町、春日部市、  八潮市、三郷市、鶴ヶ島市、北足立郡伊奈町", ymd("2021-07-20"), ymd("2021-08-01"),
  "千葉県", "市川市、浦安市、船橋市、松戸市", ymd("2021-04-20"), ymd("2021-08-01"),
  "千葉県", "柏市", ymd("2021-04-20"), ymd("2021-06-20"),
  "千葉県", "千葉市、習志野市", ymd("2021-04-28"), ymd("2021-08-01"),
  "千葉県", "野田市、流山市、八千代市、鎌ケ谷市、我孫子市", ymd("2021-04-28"), ymd("2021-06-20"),
  "千葉県", "市原市", ymd("2021-06-21"), ymd("2021-08-01"),
  "千葉県", "木更津市、君津市、富津市、袖ケ浦市", ymd("2021-06-21"), ymd("2021-07-11"),
  "千葉県", "成田市", ymd("2021-07-02"), ymd("2021-08-01"),
  "千葉県", "柏市", ymd("2021-07-12"), ymd("2021-08-01"),
  "千葉県", "八千代市、鎌ケ谷市", ymd("2021-07-19"), ymd("2021-08-01"),
  "東京都", "東京23区、武蔵野市、調布市、府中市、立川市、八王子市、町田市", ymd("2021-04-12"), ymd("2021-04-24"),
  "東京都", "東京23区、八王子市、立川市、武蔵野市、三鷹市、青梅市、府中市、昭島市、調布市、町田市、小金井市、小平市、日野市、東村山市、国分寺市、国立市、福生市、狛江市、東大和市、清瀬市、東久留米市、武蔵村山市、多摩市、稲城市、羽村市、あきる野市、西東京市、西多摩郡瑞穂町、西多摩郡日の出町", ymd("2021-06-21"), ymd("2021-07-11"),
  "神奈川県", "横浜市、川崎市、相模原市、厚木市", ymd("2021-04-20"), ymd("2021-07-21"), # 
  "神奈川県", "厚木市", ymd("2021-04-28"), ymd("2021-07-21"), #
  "神奈川県", "座間市", ymd("2021-04-28"), ymd("2021-07-11"),
  "神奈川県", "大和市、海老名市、綾瀬市、鎌倉市", ymd("2021-04-28"), ymd("2021-06-20"),
  "神奈川県", "藤沢市、横須賀市、茅ヶ崎市、伊勢原市、逗子市、三浦市、高座郡寒川町、三浦郡葉山町", ymd("2021-05-12"), ymd("2021-06-20"),
  "神奈川県", "小田原市", ymd("2021-06-01"), ymd("2021-07-11"),
  "神奈川県", "平塚市、秦野市", ymd("2021-06-01"), ymd("2021-06-20"),
  "神奈川県", "横浜市、川崎市、相模原市、横須賀市、小田原市、茅ヶ崎市、平塚市、鎌倉市、藤沢市、逗子市、三浦市、秦野市、厚木市、大和市、伊勢原市、海老名市、座間市、南足柄市、綾瀬市、三浦郡葉山町、高座郡寒川町、中郡大磯町、中郡二宮町、足柄上郡中井町、足柄上郡大井町、足柄上郡松田町、足柄上郡山北町、足柄上郡開成町、足柄下郡箱根町、足柄下郡真鶴町、足柄下郡湯河原町、愛甲郡愛川町", ymd("2021-07-22"), ymd("2021-08-01"),
  "富山県", "富山市", ymd("2021-08-20"), ymd("2021-09-12"),
  "石川県", "金沢市", ymd("2021-05-16"), ymd("2021-06-13"),
  "石川県", "金沢市", ymd("2021-08-02"), ymd("2021-09-30"),
  "山梨県", "甲府市、富士吉田市、都留市、山梨市、大月市、韮崎市、南アルプス市、北杜市、甲斐市、笛吹市、上野原市、甲州市、中央市、西八代郡市川三郷町、南巨摩郡富士川町、中巨摩郡昭和町、南都留郡山中湖村、南都留郡富士河口湖町", ymd("2021-08-20"), ymd("2021-09-12"),
  "岐阜県", "岐阜市、大垣市、多治見市、関市、中津川市、羽島市、美濃加茂市、土岐市、各務原市、可児市、瑞穂市、本巣市、羽島郡岐南町、羽島郡笠松町、養老郡養老町、本巣郡北方町", ymd("2021-05-09"), ymd("2021-06-20"),
  "岐阜県", "高山市、瑞浪市、恵那市、山県市、下呂市、可児郡御嵩町", ymd("2021-05-16"), ymd("2021-06-20"),
  "岐阜県", "岐阜市、羽島市、各務原市、山県市、瑞穂市、本巣市、羽島郡岐南町、羽島郡笠松町、本巣郡北方町、大垣市、美濃加茂市、可児市、可児郡御嵩町、多治見市、中津川市", ymd("2021-08-20"), ymd("2021-08-26"),
  "静岡県", "静岡市、浜松市、沼津市、熱海市、三島市、富士宮市、伊東市、富士市、御殿場市、下田市、裾野市、伊豆市、伊豆の国市、賀茂郡東伊豆町、賀茂郡河津町、賀茂郡南伊豆町、賀茂郡松崎町、賀茂郡西伊豆町、田方郡函南町、駿東郡清水町、駿東郡長泉町、駿東郡小山町", ymd("2021-08-08"), ymd("2021-08-19"),
  "静岡県", "磐田市、焼津市、藤枝市", ymd("2021-08-15"), ymd("2021-08-19"),
  "静岡県", "袋井市、掛川市、御前崎市、菊川市、牧之原市、島田市、湖西市、周智郡森町、榛原郡吉田町", ymd("2021-08-18"), ymd("2021-08-19"),
  "愛知県", "名古屋市", ymd("2021-04-20"), ymd("2021-05-11"),
  "愛知県", "名古屋市、豊橋市、小牧市", ymd("2021-06-21"), ymd("2021-07-11"),
  "愛知県", "岡崎市、半田市、春日井市、津島市、刈谷市、犬山市、高浜市、清須市、西春日井郡豊山町、丹羽郡大口町、海部郡大治町", ymd("2021-06-21"), ymd("2021-07-02"),
  "愛知県", "蒲郡市", ymd("2021-07-03"), ymd("2021-07-11"),
  "愛知県", "名古屋市、春日井市、江南市、大府市、尾張旭市、日進市、清須市、あま市、長久手市、愛知郡東郷町、海部郡大治町", ymd("2021-08-08"), ymd("2021-08-26"),
  "愛知県", "海部郡飛島村", ymd("2021-08-08"), ymd("2021-08-20"),
  "愛知県", "岡崎市、一宮市、瀬戸市、半田市、津島市、刈谷市、豊田市、蒲郡市、犬山市、常滑市、小牧市、稲沢市、東海市、知立市、高浜市、岩倉市、豊明市、愛西市、北名古屋市、西春日井郡豊山町、丹羽郡大口町、丹羽郡扶桑町、海部郡蟹江町、知多郡阿久比町、知多郡南知多町、知多郡美浜町、知多郡武豊町、北設楽郡東栄町", ymd("2021-08-21"), ymd("2021-08-26"),
  "三重県", "桑名市、いなべ市", ymd("2021-05-09"), ymd("2021-06-13"),
  "三重県", "四日市市", ymd("2021-05-09"), ymd("2021-06-20"),
  "三重県", "鈴鹿市、亀山市、名張市、伊賀市、桑名郡木曽岬町、員弁郡東員町、三重郡菰野町、三重郡朝日町、三重郡川越町", ymd("2021-05-09"), ymd("2021-06-13"),
  "三重県", "桑名市、いなべ市、桑名郡木曽岬町、員弁郡東員町、四日市市、三重郡菰野町、三重郡朝日町、三重郡川越町、鈴鹿市、亀山市、津市、松阪市、多気郡多気町、多気郡明和町、多気郡大台町、名張市、伊賀市", ymd("2021-08-20"), ymd("2021-08-26"),
  "滋賀県", "大津市、彦根市、長浜市、近江八幡市、草津市、守山市、栗東市、甲賀市、野洲市、湖南市、高島市、東近江市、米原市", ymd("2021-08-08"), ymd("2021-08-26"),
  "京都府", "京都市", ymd("2021-04-12"), ymd("2021-04-24"),
  "京都府", "京都市", ymd("2021-06-21"), ymd("2021-07-11"),
  "京都府", "京都市", ymd("2021-08-02"), ymd("2021-08-19"),
  "京都府", "宇治市、城陽市、向日市、長岡京市、八幡市、京田辺市、木津川市", ymd("2021-08-17"), ymd("2021-08-19"),
  "大阪府", "大阪市", ymd("2021-04-05"), ymd("2021-04-24"),
  "大阪府", "岸和田市、豊中市、池田市、吹田市、泉大津市、高槻市、貝塚市、守口市、枚方市、茨木市、八尾市、泉佐野市、富田林市、寝屋川市、河内長野市、松原市、大東市、和泉市、箕面市、柏原市、羽曳野市、門真市、摂津市、高石市、藤井寺市、東大阪市、泉南市、四條畷市、交野市、大阪狭山市、阪南市", ymd("2021-06-21"), ymd("2021-08-01"),
  "兵庫県", "尼崎市、西宮市、芦屋市、神戸市", ymd("2021-04-05"), ymd("2021-04-24"),
  "兵庫県", "伊丹市、宝塚市、川西市、三田市、明石市、川辺郡猪名川町", ymd("2021-04-22"), ymd("2021-04-24"),
  "兵庫県", "神戸市、尼崎市、西宮市、芦屋市、伊丹市、宝塚市、川西市、三田市、川辺郡猪名川町、明石市、加古川市、高砂市、加古郡稲美町、加古郡播磨町、姫路市", ymd("2021-06-21"), ymd("2021-07-11"),
  "兵庫県", "神戸市、尼崎市、西宮市、芦屋市、伊丹市、宝塚市、川西市、三田市、川辺郡猪名川町、明石市、加古川市、高砂市、加古郡稲美町、加古郡播磨町、姫路市", ymd("2021-08-02"), ymd("2021-08-19"),
  "兵庫県", "西脇市、三木市、小野市、加西市、加東市、多可郡多可町、神崎郡神河町、神崎郡市川町、神崎郡福崎町、相生市、たつの市、赤穂市、宍粟市、揖保郡太子町、赤穂郡上郡町、佐用郡佐用町、丹波篠山市、丹波市、洲本市、南あわじ市、淡路市", ymd("2021-08-16"), ymd("2021-08-19"),
  "岡山県", "岡山市、倉敷市", ymd("2021-08-20"), ymd("2021-08-26"),
  "岡山県", "岡山市、倉敷市、津山市、玉野市、笠岡市、井原市、総社市、備前市、赤磐市、真庭市、浅口市、都窪郡早島町、小田郡矢掛町、勝田郡勝央町、勝田郡奈義町、久米郡久米南町、久米郡美咲町", ymd("2021-09-13"), ymd("2021-09-30"),
  "広島県", "広島市、三原市、廿日市市、呉市、尾道市、福山市、府中市、竹原市、東広島市、安芸郡府中町、安芸郡海田町、安芸郡坂町", ymd("2021-08-20"), ymd("2021-08-26"),
  "香川県", "高松市", ymd("2021-08-20"), ymd("2021-09-30"),
  "愛媛県", "松山市", ymd("2021-04-25"), ymd("2021-05-22"),
  "愛媛県", "松山市", ymd("2021-08-20"), ymd("2021-09-12"),
  "高知県", "高知市", ymd("2021-08-27"), ymd("2021-09-12"),
  "福岡県", "福岡市、北九州市、久留米市", ymd("2021-06-21"), ymd("2021-07-11"),
  "福岡県", "福岡市、北九州市、久留米市、筑紫野市、春日市、大野城市、宗像市、太宰府市、古賀市、福津市、朝倉市、糸島市、那珂川市、糟屋郡宇美町、糟屋郡篠栗町、糟屋郡志免町、糟屋郡須恵町、糟屋郡新宮町、糟屋郡久山町、糟屋郡粕屋町、朝倉郡筑前町、朝倉郡東峰村", ymd("2021-08-02"), ymd("2021-08-19"),
  "熊本県", "熊本市", ymd("2021-05-16"), ymd("2021-06-13"),
  "熊本県", "熊本市", ymd("2021-08-08"), ymd("2021-09-30"),
  "佐賀県", "旧唐津市", ymd("2021-08-27"), ymd("2021-09-12"), ### 
  "長崎県", "長崎市、佐世保市", ymd("2021-08-27"), ymd("2021-09-12"),
  "宮崎県", "宮崎市", ymd("2021-08-27"), ymd("2021-09-30"),
  "宮崎県", "日向市、東臼杵郡門川町", ymd("2021-08-27"), ymd("2021-09-12"),
  "鹿児島県", "鹿児島市", ymd("2021-08-20"), ymd("2021-09-30"),
  "鹿児島県", "霧島市、姶良市", ymd("2021-08-20"), ymd("2021-09-12"),
  "沖縄県", "那覇市、名護市、うるま市、沖縄市、宜野湾市、南城市、浦添市、豊見城市、糸満市", ymd("2021-04-12"), ymd("2021-05-22"),
  "沖縄県", "島尻郡南風原町、島尻郡八重瀬町、島尻郡与那原町、中頭郡西原町、中頭郡北谷町", ymd("2021-05-01"), ymd("2021-05-22"),
  "沖縄県", "石垣市", ymd("2021-05-12"), ymd("2021-05-22")
) %>% 
  separate_rows(area, sep = "、") %>% 
  mutate(type = if_else(area == "旧唐津市",
                        "other",
                        "city")) %>% 
  relocate(type, .after = area)

df_manbou_all %>% 
  write_csv(here::here("data-raw/まん延防止等重点措置の発令.csv"))
