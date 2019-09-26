# install.packages('rvest')
# install.packages('ggplot2')
# install.packages("plotly")

library(rvest)
library(stringr)
library(ggplot2)
library(plotly)



autotrader_url_to_df <- function(url, model) {
  
  page <- read_html(url)
  
  # Chop out "featured" nodes: they just create incomplete duplicates
  xml_remove(page %>% html_nodes('.featured-left'))
  
  
  # <div class="kms">
  #   <b>Mileage</b> 33,075 km
  # </div>
  
  km_nodes <- page %>% html_nodes('.kms') %>% html_text()
  kms <- km_nodes[stringr::str_detect(km_nodes, "Mileage")]
  kms <- as.numeric(str_replace_all(str_extract(kms, '\\d+,\\d+'), ',', ''))
  
  
  # <span itemprop="itemOffered">
  #   2016 Kia Soul EV Electric NAV REAR CAM HTD SEATS
  # </span>
  
  descrs <- trimws(page %>% html_nodes('[itemprop="itemOffered"]') %>%
                     html_text())
  
  years <- as.numeric(str_extract(descrs, '^\\d+'))
  
  
  
  # <span class="price-amount">$21,995</span>
  
  prices <- as.numeric(
    gsub('[$,]', '', page %>% html_nodes('.price-amount') %>%
           html_text())
  )
  
  
  df <- data.frame(
    model=as.factor(rep(model, length(descrs))),
    year=as.factor(years),
    km=kms,
    price=prices,
    descr=descrs
  )
  
  df["descr"] <- as.character(df[["descr"]])
  
  # TODO:
  # Look for this to get an id then make sure it is unique to
  # avoid duplicates with priority listings
  
  # <div id="61375194_price" class="price" itemprop="price">
  
  return (df)
  
}



kia_soul_url <- 'https://www.autotrader.ca/cars/kia/on/?rcp=50&rcs=0&srt=4&yRng=2016%2C&pRng=8000%2C35000&prx=-2&prv=Ontario&loc=n2t0a2&hprc=False&wcp=False&eng=Electric%20Motor&inMarket=advancedSearch'
df_soul <- autotrader_url_to_df(kia_soul_url, "Kia Soul EV")


nissan_leaf_url <- 'https://www.autotrader.ca/cars/nissan/on/?rcp=50&rcs=0&srt=4&yRng=2016%2C&pRng=8000%2C35000&prx=-2&prv=Ontario&loc=n2t0a2&hprc=False&wcp=False&eng=Electric%20Motor&inMarket=advancedSearch'
df_leaf <- autotrader_url_to_df(nissan_leaf_url, "Nissan Leaf")

chevy_bolt_url <- 'https://www.autotrader.ca/cars/chevrolet/on/?rcp=50&rcs=0&srt=4&yRng=2016%2C&pRng=8000%2C35000&prx=-2&prv=Ontario&loc=n2t0a2&hprc=False&wcp=False&eng=Electric%20Motor&inMarket=advancedSearch'
df_bolt <- autotrader_url_to_df(chevy_bolt_url, "Chevy Bolt")

df <- rbind(df_soul, df_leaf, df_bolt)


g <- ggplot(df,
            aes(y      = price,
                x      = km,
                col    = year,
                shape  = model,
                label  = descr
            )
) +
  labs(color="Model", shape="Year") +
  geom_point() +
  scale_y_continuous(name="Price", labels = scales::comma) +
  scale_x_continuous(name="Km", labels = scales::comma) +
  geom_smooth(aes(linetype=model), method="lm", colour="gray", fullrange=T, size=0.5) 


ggplotly(g)



