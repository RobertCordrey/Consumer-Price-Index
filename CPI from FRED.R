
library(dplyr)
library(lubridate)
library(ggplot2)
library(fredr)
library(scales)

# Get the current month and year
month <- format(Sys.Date(), "%B")
year <- format(Sys.Date(), "%Y")

# Set FRED API key
fredr_set_key("******")

# Retrieve median sale price of home sold, data from FRED
medianCPI <- fredr(
    series_id = 'MEDCPIM158SFRBCLE',
    observation_start = as.Date('1971-01-01'),
    observation_end = as.Date(Sys.Date())
)

min_date <- format(min(medianCPI$date), '%B %Y')

medianCPI1 <- medianCPI %>%
    na.omit() %>%
    select(1, 3) %>%
    rename(Date = 1, Value = 2)

# class(medianCPI1$Date)

yearlymedianCPI <- medianCPI1 %>%
    group_by(year = year(Date)) %>%
    summarize(medianCPI = round(median(Value), 2))

# Define a custom label function to add a % sign
percent_label <- function(x) {
    paste0(x, "%")
}

# shows the median sale price for each year
ggplot(yearlymedianCPI, aes(year, medianCPI)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(min(yearlymedianCPI$year),
                                    max(yearlymedianCPI$year),
                                    by = 4)) +
    scale_y_continuous(limits = c(min(yearlymedianCPI$medianCPI, na.rm = TRUE),
                                  max(yearlymedianCPI$medianCPI, na.rm = TRUE)),
                       labels = percent_label) +
    scale_color_brewer(palette="Paired") +
    theme_minimal() +
    ggtitle(paste0("Consumer Price Index from ",
                   min(yearlymedianSales1$year),
                   " to ",
                   max(yearlymedianSales1$year))) +
    labs(caption = "Blue Hen Analytics - Data from Federal Reserve Economic Database") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.caption=element_text(hjust = 1)) +
    xlab(" ") +
    ylab("Median Price")

# class(yearlymedianSales1$year)

# shows monthly flucations for each year
ggplot(medianCPI1,
       aes(x = year(Date),
           y = Value,
           group = year(Date))) +
    geom_boxplot() +
    scale_x_continuous(breaks = seq(min(year(medianCPI1$Date)),
                                    max(year(medianCPI1$Date)),
                                    by = 4)) +
    scale_y_continuous(limits = c(min(medianCPI1$Value, na.rm = TRUE),
                                  max(medianCPI1$Value, na.rm = TRUE)),
                       labels = percent_label) +
    scale_color_brewer(palette="Paired")+
    theme_minimal() +
    ggtitle(paste0("Yearly Variation Rate from ",
                   min(lubridate::year(medianCPI1$Date)),
                   " to ",
                   max(lubridate::year(medianCPI1$Date)))) +
    labs(subtitle = "",
         caption = "Blue Hen Analytics - Data from Federal Reserve Economic Database") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle=element_text(hjust = 0.5),
          plot.caption=element_text(hjust = 1)) +
    xlab("") +
    ylab("Monthly CPI")

