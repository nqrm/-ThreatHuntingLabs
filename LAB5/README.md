## Цель работы

### 1. Закрепить практические навыки использования языка программирования R для обработки данных

### 2. Закрепить знания основных функций обработки данных экосистемы tidyverse языка R

### 3. Закрепить навыки исследования метаданных DNS трафика

## Общая ситуация

### Вы исследуете подозрительную сетевую активность во внутренней сети Доброй Организации. Вам в руки попали метаданные о DNS трафике в исследуемой сети. Исследуйте файлы, восстановите данные, подготовьте их к анализу и дайте обоснованные ответы на поставленные вопросы исследования.

## Задания

### Подготовка данных

    library("tidyverse")

#### 1.Импортируйте данные DNS

    headers_csv <- read_csv("header.csv",show_col_types = FALSE)
    headers <- unlist(headers_csv[1])

    dns <- read_tsv("dns.log", show_col_types = FALSE, col_names = headers)
    df <- as.data.frame(dns)

#### 2.Добавьте пропущенные данные о структуре данных (назначении столбцов)

##### Сделано вручную

#### 3.Преобразуйте данные в столбцах в нужный формат

##### Сделано вручную

### Анализ данных

#### 4.Сколько участников информационного обмена в сети Доброй Организации?

    origs <- df %>% select("orig_h") %>% distinct(.keep_all = TRUE)
    resps <- df %>% select("resp_h") %>% distinct(.keep_all = TRUE)
    members <- bind_rows(origs,resps) %>% transmute(orig_h) %>% unique() %>% na.omit()
    count_members <- members %>% nrow()
    count_members

    ## [1] 253

#### 5.Какое соотношение участников обмена внутри сети и участников обращений к внешним ресурсам

    v_resps <- unlist(resps)
    v_resps <- as.vector(v_resps,'character')
    inside <- str_extract(v_resps,regex("(192\\.168\\..+|10\\..+|100\\.(6[4-9]|[7-9][0-9]|1[0-1][0-9]|12[0-7])\\..+)|172\\.(1[6-9]|2[0-9]|3[0-1])\\..+")) %>% na.omit() %>% length()
    outside <- length(v_resps) - inside
    inside / outside

    ## [1] 25.17021

#### 6.Найдите топ-10 участников сети, проявляющих наибольшую сетевую активность.

    df %>% select(orig_h) %>% group_by(orig_h) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(10)

    ## # A tibble: 10 × 2
    ##    orig_h          count
    ##    <chr>           <int>
    ##  1 10.10.117.210   75943
    ##  2 192.168.202.93  26522
    ##  3 192.168.202.103 18121
    ##  4 192.168.202.76  16978
    ##  5 192.168.202.97  16176
    ##  6 192.168.202.141 14967
    ##  7 10.10.117.209   14222
    ##  8 192.168.202.110 13372
    ##  9 192.168.203.63  12148
    ## 10 192.168.202.106 10784

#### 7.Найдите топ-10 доменов, к которым обращаются пользователи сети и соответственное количество обращений

    domains <- df %>% select(query) %>% group_by(query) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(10)
    domains

    ## # A tibble: 10 × 2
    ##    query                                                                   count
    ##    <chr>                                                                   <int>
    ##  1 "teredo.ipv6.microsoft.com"                                             39273
    ##  2 "tools.google.com"                                                      14057
    ##  3 "www.apple.com"                                                         13390
    ##  4 "time.apple.com"                                                        13109
    ##  5 "safebrowsing.clients.google.com"                                       11658
    ##  6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x… 10401
    ##  7 "WPAD"                                                                   9134
    ##  8 "44.206.168.192.in-addr.arpa"                                            7248
    ##  9 "HPE8AA67"                                                               6929
    ## 10 "ISATAP"                                                                 6569

#### 8.Опеределите базовые статистические характеристики (функция summary()) интервала времени между последовательным обращениями к топ-10 доменам.

    fun <- function(x){
      abs(diff(x))
    }

    queries <- domains %>% select(query)
    v_queries <- unlist(queries)
    v_queries <- as.vector(v_queries,'character')
    temp <- df %>% filter(query %in% v_queries) %>% select(ts) %>% arrange(ts)
    temp <- as.double(unlist(temp))
    temp <- as.POSIXct(temp, origin="1970-01-01")
    summary(temp)

    ##                       Min.                    1st Qu. 
    ## "2012-03-16 16:30:05.5099" "2012-03-16 21:01:10.2000" 
    ##                     Median                       Mean 
    ## "2012-03-16 22:29:04.9949" "2012-03-17 02:11:13.0510" 
    ##                    3rd Qu.                       Max. 
    ## "2012-03-17 01:17:58.5899" "2012-03-18 00:59:39.0799"

#### 9. Часто вредоносное программное обеспечение использует DNS канал в качестве канала управления, периодически отправляя запросы на подконтрольный злоумышленникам DNS сервер. По периодическим запросам на один и тот же домен можно выявить скрытый DNS канал. Есть ли такие IP адреса в исследуемом датасете?

### Анализ данных

#### 10.Определите местоположение (страну, город) и организацию-провайдера для топ-10 доменов. Для этого можно использовать сторонние сервисы, например <https://v4.ifconfig.co/>.

    library(curl)
    library(jsonlite)

    fun_org <- function(ip) {
      url <- toString(paste0("ifconfig.co/json?ip=",ip))
      req <- curl_fetch_memory(url)
      json <- parse_headers(req$content)
      document <- fromJSON(txt=json)
      return (document$asn_org)
    }

    fun_city <- function(ip) {
      url <- toString(paste0("ifconfig.co/city?ip=",ip))
      req <- curl_fetch_memory(url)
      content <- parse_headers(req$content)
      return (content$city)
    }

    fun_country <- function(ip) {
      url <- toString(paste0("ifconfig.co/country?ip=",ip))
      req <- curl_fetch_memory(url)
      content <- parse_headers(req$content)
      return (content$country)
    }

    # Ошибка: trailing garbage
    #fin_df <- df %>% filter(query %in% domains$query) %>% select(query,resp_h)
    #fin_df %>% mutate(asn_org =lapply(resp_h,fun_org) )
    #fin_df %>% mutate(city = lapply(resp_h,fun_city))
    #fin_df %>% mutate(country = lapply(resp_h,fun_country))
