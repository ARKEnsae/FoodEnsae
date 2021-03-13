library(highcharter)
my_own_theme <- hc_theme(
  chart = list(
    backgroundColor = NULL#,
    # style = list(
    #   # fontFamily = "Gloria Hallelujah"
    #   )
  ),
  # title = list(
  #   # style = list(
  #   #   # fontFamily = "Gloria Hallelujah"
  #   # )
  # ),
  # subtitle = list(
  #   # style = list(
  #   #   # fontFamily = "Gloria Hallelujah"
  #   # )
  # ),
  caption = list(
    style = list(
      fontSize = "10px"
      )
  )
)
saveRDS(my_own_theme, "content/homepage/data/hc_theme.RDS")
