guess_unit <- function(x) {
  stringr::str_extract(x, "\\(.+?\\)") %>%
    stringr::str_remove_all("\\(|\\)")
}

validate_date <- function(year, month, day) {

  check_positive <-
    sapply(list(year = year, month = month, day = day), function(x) { x > 0})

  if (sum(check_positive) != 3L) {
    rlang::inform(paste0(
      "Input arguments should be positive\n",
      "Check: ",
      paste(names(which(check_positive == FALSE)), collapse = ", ")))
    return(FALSE)
  }

  x <-
    lubridate::make_date(year, month, day)

  if (is.na(x)) {
    rlang::inform(paste0(
      "Input arguments should be calender format\n",
      "Check: ",
      paste(names(which(c(
        "month" =
          ifelse(is.na(match(13, 1:12)), FALSE, TRUE),
        "day" =
          ifelse(is.na(match(1, 1:31)), FALSE, TRUE)
      ) == FALSE)), collapse = ", ")))
    return(FALSE)
  } else {
    return(TRUE)
  }
}



# 数値が高くなるほど危険度が増す情報の配色
# 降水量、風速、積雪
# jma_pal <-
#   rev(purrr::pmap_chr(
#     list(r = c(180, 255, 255, 250, 255, 255, 185,   0,  0,    0),
#          g = c(0,    40, 153, 245, 255, 255, 235, 150,  65,  32),
#          b = c(104,   0,   0,   0, 150, 240, 255, 255, 255, 128)),
#     ~ rgb(..1, ..2, ..3, max = 255)
#   ))

# x <- seq(0, 1, length = 600)
# scales::show_col(scales::seq_gradient_pal(c(jma_pal))(x),
#                  labels = FALSE, borders = "#a87963")

jma_pal <- function(pallet = c("absolute", "relative"), .attribute = FALSE) {

  res <-
    list(
      absolute = list(
        colors = purrr::pmap_chr(
          list(r = c(180, 255, 255, 250,   0, 33,  160, 242),
               g = c(0,    40, 153, 245,  65, 140, 210, 242),
               b = c(104,   0,   0,   0, 255, 240, 255, 255)),
          ~ rgb(..1, ..2, ..3, max = 255)
        ),
        precipitation = list(
          labels = c("Over 80", "50~80", "30~50", "20~30", "10~20", "5~10", "1~5", "0~1"),
          breaks = c(80, 50, 30, 20, 10, 5, 1, 0)
        ),
        snow = list(
          labels = c("Over 200", "150~200", "100~150", "50~100",
                     "20~50", "5~20", "Under 5"),
          breaks = c(200, 150, 100, 50, 20, 5, 4)
        ),
        wind = list(
          labels = c("Over 25", "20~25", "15~20", "10~15", "5~10", "0~5"),
          breaks = c(25, 20, 15, 10, 5, 0)
        )
      ),
      relative = list(
        colors = purrr::pmap_chr(
          list(
            r = c(180, 255, 255, 250, 255, 255, 185,   0,   0,   0),
            g = c(  0,  40, 153, 245, 255, 255, 235, 150,  65,  32),
            b = c(104,   0,   0,   0, 150, 240, 255, 255, 255, 128)
          ),
          ~ rgb(..1, ..2, ..3, max = 255)
        ),
        amedas = list(
          labels =     c("35~", "30~35", "25~30", "20~25",
                         "15~20", "10~15", "5~10", "0~5",
                         "-5~0", "~-5"),
          breaks = c(35, 30, 25, 20, 15, 10, 5, 0, -5, -6)
        ),
        forecast = list(
          labels =     c("35~", "30~35", "25~30", "20~24",
                         "15~19", "10~14", "5~9", "0~4",
                         "-5~-1", "~-5"),
          breaks = c(35, 30, 25, 20, 15, 10, 5, 0, -5, -6)
        )
      )
    )

  if (.attribute == FALSE) {
    res[[rlang::quo_name(pallet)]][[1]]
  } else {
    res
  }
}

# 警報・注意報に及びこれに類する情報の配色
pal1 <-
  purrr::pmap_chr(
    list(
      r = c(0, 200, 255, 255, 250, 200, 242),
      g = c(0,   0,  40, 170, 245, 200, 242),
      b = c(0, 255,   0,   0,   0, 203, 255)
    ),
    ~ rgb(..1, ..2, ..3, max = 255)
  )

# 数値が高くなるほど危険度が増す情報の配色（震度）
pal2 <-
  purrr::pmap_chr(
    list(
      r = c(180, 165, 255, 255, 255, 250,   0,   0, 242),
      g = c(0,     0,  40, 153, 230, 230,  65, 170, 242),
      b = c(104,  33,   0,   0,   0, 150, 255, 255, 255),
      value = c("7", "6+", "6-", "5+", "5-", "4", "3", "2", "1")
    ),
    ~ rgb(..1, ..2, ..3, max = 255)
  )

# 数値が高くなるほど危険度が増す情報の配色（紫外線情報）
pal3 <-
  purrr::pmap_chr(
    list(
      r = c(204, 204, 181, 165, 255, 250, 255, 255, 250, 250, 255, 153, 217, 255),
      g = c(  0,   0,   0,   0,  20,  90, 140, 200, 245, 250, 255, 203, 217, 255),
      b = c(204,  160,  91, 33,   0,   0,   0,   0,   0, 150, 190, 255, 255, 255),
      value = c("13+", "12", "11", "10", "9", "8", "7", "6", "5", "4", "3", "2", "1", "0")
    ),
    ~ rgb(..1, ..2, ..3, max = 255)
  )

# 中間レベルの値は標準的で、値が大きくまたは小さくなるほど注意を促したい情報
pal4 <-
  purrr::pmap_chr(
    list(
      r = c(180, 255, 255, 250, 255, 255, 185,   0,   0,   0),
      g = c(  0,  40, 153, 245, 255, 255, 235, 150,  65,  32),
      b = c(104,   0,   0,   0, 150, 240, 255, 255, 255, 128),
      value = c("35~", "30~35", "25~30", "20~25",
                "15~20", "10~15", "5~10", "0~5",
                "-5~0", "~-5")
    ),
    ~ rgb(..1, ..2, ..3, max = 255)
  )

# 中間レベルの値は標準的で、値が大きくまたは小さくなるほど注意を促したい情報
# (季節情報)
pal5_temp <-
  purrr::pmap_chr(
    list(
      r = c(145, 255, 255, 255, 255, 255, 200,  70,   0,  33,   0),
      g = c(  0,  26, 153, 240, 240, 255, 255, 255, 126,  33,   0),
      b = c( 83,  26,   0,   0, 180, 240, 255, 255, 255, 255, 112)
    ),
    ~ rgb(..1, ..2, ..3, max = 255)
  )
# scales::show_col(scales::seq_gradient_pal(c(pal5_temp))(x),
#                  labels = FALSE,
#                  borders = "#a87963")
