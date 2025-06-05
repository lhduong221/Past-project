## LIBRARIES ----
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(ggthemes)


## DATA ----
getwd()
data <- read.csv("Data/hotel_booking.csv")

summary(data)
data <- data[!is.na(data$children),]               # remove children NAs
data$agent[is.na(data$agent)] <- "No agent"        # make agent zero values into "No agent" string
data$company[is.na(data$company)] <- "No company"  # make company zero values into "No company" string
data <- data[data$market_segment != "Undefined",]  # remove "Undefined" from market_segment
data <- data[data$country != "",]                  # remove blank observations from country
data <- data[data$adr != 5400,]                    # remove a significant outlier 5400 from adr

# Distnct individuals in dataset
n_distinct(data$email) #115,425 unqiue emails
n_distinct(data$name) #81,234 unique names

# Theme selection
theme_set(
  #theme_minimal()
  #theme_classic()
  theme_tufte(base_family = "sans")
)

# Cancellation per hotel
ggplot(data, aes(x = hotel, fill = factor(is_canceled))) +
  geom_bar(position = "dodge") +
  labs(x = "Hotel", y = "Count", fill = "Canceled?") +
  scale_x_discrete(labels = c("City", "Resort")) #+
theme_tufte()

# divide the data set into two, based on hotel, and use one due to limited analysis criteria
data_city <- data[data$hotel == "City Hotel", ]  # will use this, since more obs and bigger proportion of cancellations
data_resort <- data[data$hotel == "Resort Hotel", ]


## EXPLORING THE VARIABLES ----
# stay_nights --> no, does not vary for cancelation
data_city$stay_nights <- data_city$stays_in_weekend_nights + data_city$stays_in_week_nights
ggplot(data_city, aes(x = factor(is_canceled), stay_nights)) +
  geom_boxplot(color = "black")

# country --> Good, but needs relativity
top_cancelling_countries <- data_city |>
  filter(is_canceled == 1) |>
  count(country, name = "cancellations") |>
  arrange(desc(cancellations)) |>
  slice_head(n = 10)

ggplot(top_cancelling_countries, aes(x = reorder(country, -cancellations), y = cancellations)) +
  geom_bar(stat = "identity")

# deposit_type --> maybe, but poor information about data meaning
ggplot(data_city, aes(x = deposit_type, fill = factor(is_canceled))) +
  geom_bar(color = "black")

# market_segment --> good
ggplot(data_city, aes(x = market_segment, fill = factor(is_canceled))) +
  geom_bar(color = "black")

# lead_time --> good, but needs to be combined with something (alone too simple)
ggplot(data_city, aes(x = factor(is_canceled), y = lead_time)) +
  geom_boxplot()

ggplot(data_city, aes(x = factor(market_segment), y = lead_time, fill = factor(is_canceled))) +
  geom_boxplot()

# distribution_channel --> nothing special, market segment is better
ggplot(data_city, aes(x = distribution_channel, fill = factor(is_canceled))) +
  geom_bar(color = "black")

# customer_type --> same as distribution channel
ggplot(data_city, aes(x = customer_type, fill = factor(is_canceled))) +
  geom_bar(color = "black")

# agent --> Good, but needs relativity (zero is no agent)
top_cancelling_agents <- data_city |>
  filter(is_canceled == 1) |>
  count(agent, name = "cancellations") |>
  arrange(desc(cancellations)) |>
  slice_head(n = 10)

ggplot(top_cancelling_agents, aes(x = reorder(agent, -cancellations), y = cancellations)) +
  geom_bar(stat = "identity")

# name --> Interesting, but needs relativity (p.s. relatively cancellations do no variate much)
top_cancelling_name <- data_city |>
  filter(is_canceled == 1) |>
  count(name, name = "cancellations") |>
  arrange(desc(cancellations)) |>
  slice_head(n = 20)

ggplot(top_cancelling_name, aes(x = reorder(name, -cancellations), y = cancellations)) +
  geom_bar(stat = "identity")

# adr --> interesting, cancellations do not vart due to monetary reasons
ggplot(data_city[data_city$adr < 1000, ], aes(x = factor(is_canceled), y = adr)) +  # limit due to single extreme outlier 5400 (likely miss typing)
  geom_boxplot()  # additionally single negative value - 6.38 (refund?)


## MONTH + ADR ----
# STEP 1: Summarize data for cancellations, total bookings, average ADR, and cancellation percentage per month
monthly_data <- data_city |>
  mutate(arrival_date_month = factor(arrival_date_month,                                 # organizing the months in the correct order
                                     levels = c("January", "February", "March",
                                                "April", "May", "June", 
                                                "July", "August", "September",
                                                "October", "November", "December"))) |>
  group_by(arrival_date_month) |>
  summarize(                                                                             # counting total bookings for each month
    total_bookings = n(),
    canceled_bookings = sum(is_canceled == 1),                                           # counting cancelled booking for each month
    avg_adr = mean(adr, na.rm = TRUE),                                                   # calculate average ADR for each month
    cancellation_percentage = (canceled_bookings / total_bookings) * 100                 # calculate cancellation percentage for each month
  )

# PLOT: Bar & Line for Month & ADR
ggplot(monthly_data, aes(x = arrival_date_month)) +
  geom_bar(aes(y = total_bookings, fill = "Total Bookings"),                                          # Total Booking bars
           stat = "identity",
           position = "dodge",
           width = 0.9) +
  geom_bar(aes(y = canceled_bookings, fill = "Canceled Bookings"),                                    # Cancellations bars
           stat = "identity",
           position = "dodge",
           width = 0.7) +
  geom_text(aes(y = canceled_bookings, label = paste0(round(cancellation_percentage, 1), "%")),       # Cancellation percentage as text on top of the bars
            vjust = -0.5, 
            color = "gray20", 
            size = 3.5) +
  geom_line(aes(y = avg_adr * max(total_bookings) / max(avg_adr), group = 1, color = "Average ADR"),  # Average adr line
            size = 1.2,
            alpha = 0.6) +
  geom_point(aes(y = avg_adr * max(total_bookings) / max(avg_adr), color = "Average ADR"),            # Average adr points
             size = 2) +
  scale_y_continuous(name = "Number of Bookings",
                     sec.axis = sec_axis(~ . * max(monthly_data$avg_adr) / max(monthly_data$total_bookings), 
                                         name = "Average ADR")) +
  scale_fill_manual(
    name = "",
    values = c("Total Bookings" = "steelblue", "Canceled Bookings" = "salmon")
  ) +
  scale_color_manual(
    name = "",
    values = c("Average ADR" = "red")
  ) +
  labs(
    title = "Seasonal changes in Reservations and Revenue",
    subtitle = "Monthly Bookings, Cancellations, and Average ADR for City Hotel",
    x = "Month",
    y = "Number of Bookings"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14), 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 10),
    legend.position = "top",
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14)
  )


## COUNTRY + AGENT ----
# STEP 1: Identify the top 10 countries by total cancellations
top_countries <- data_city |>
  filter(is_canceled == 1) |>
  group_by(country) |>
  summarize(total_cancellations = n()) |>
  arrange(desc(total_cancellations)) |>
  slice_max(order_by = total_cancellations, n = 10)  # number of countries to select

# STEP 2: Identify the top 5 agents by total cancellations
top_agents <- data_city |>
  filter(is_canceled == 1) |>
  group_by(agent) |>
  summarize(total_cancellations = n()) |>
  arrange(desc(total_cancellations)) |>
  slice_max(order_by = total_cancellations, n = 10)  # number of agents to select

# STEP 3: Filter data for top countries and top agents, then summarize cancellations by year
top_countries_agents_years <- data_city|>
  filter(is_canceled == 1, country %in% top_countries$country, agent %in% top_agents$agent) |>
  group_by(country, agent, arrival_date_year) |>
  summarize(total_cancellations = n()) |>
  ungroup()

# PLOT: Heatmap for Countries & Agents
ggplot(top_countries_agents_years, aes(x = country, y = as.factor(agent), fill = total_cancellations)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(
    title = "Heatmap of Agents vs. Countries",
    subtitle = "Faceted by Year and colored by number of Cancellations",
    x = "Country",
    y = "Agent",
    fill = "Total Cancellations"
  ) +
  facet_wrap(~ arrival_date_year) +  # Facet by year
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14) ,
    legend.position = "top",
    panel.border = element_rect(color = "gray20", fill = NA, size = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 11, face = "bold")
  )


## LEAD_TIME + MARKET_SEGMENT----
# STEP 1: Summarize data for all market segments by cancellations
top_market_segments <- data_city |>
  filter(is_canceled == 1) |>
  group_by(market_segment) |>
  summarize(
    Cancellations = n(),
    Avg_lead_time = mean(lead_time)  # used for the line later
  ) |>
  arrange(desc(Cancellations))

# STEP 2: Calculate total bookings for each market segment
total_bookings <- data_city |>
  group_by(market_segment) |>
  summarize(Total_Bookings = n())

# STEP 3: Merge total bookings with all market segments
top_market_segments <- top_market_segments |>
  left_join(total_bookings, by = "market_segment")

# STEP 4: Calculate cancellation percentage for each market segment
top_market_segments <- top_market_segments |>
  mutate(Cancellation_Percentage = Cancellations / Total_Bookings * 100)

# PLOT: Bar & Line for Market Segments & Lead Times
ggplot(top_market_segments, aes(x = reorder(market_segment, -Cancellations))) +
  geom_bar(aes(y = Total_Bookings, fill = "Total Bookings"),                                                          # Total Bookings bars
           stat = "identity",
           position = "dodge",
           width = 0.9) +
  geom_bar(aes(y = Cancellations, fill = "Cancellations"),                                                             # Cancellations bars
           stat = "identity",
           position = "dodge",
           width = 0.7) +
  geom_text(aes(y = Cancellations, label = paste0(round(Cancellation_Percentage, 1), "%")),                            # Cancellation percentage as text on top of the bars
            position = position_dodge(width = 0.7), 
            vjust = -0.5,
            color = "gray20",
            size = 3.5, 
            fontface = "bold") +
  geom_line(aes(y = Avg_lead_time * max(Cancellations) / max(Avg_lead_time), group = 1, color = "Average Lead Time"),  # Average lead time line with legend label
            size = 1.2, 
            alpha = 0.6) +
  geom_point(aes(y = Avg_lead_time * max(Cancellations) / max(Avg_lead_time), color = "Average Lead Time"),            # Average lead time points with legend label
             size = 2) +
  scale_y_continuous(
    name = "Number of Bookings",
    sec.axis = sec_axis(~ . * max(top_market_segments$Avg_lead_time) / max(top_market_segments$Cancellations),
                        name = "Average Lead Time (days)")
  ) +
  scale_fill_manual(
    name = "",
    values = c("Cancellations" = "salmon", "Total Bookings" = "steelblue")
  ) +
  scale_color_manual(
    name = "",
    values = c("Average Lead Time" = "red")
  ) +
  labs(
    title = "Reservation distribution by Market Segments",
    subtitle = "Market Segment Bookings, Booking Cancellations, and Average Lead Time for City Hotel",
    x = "Market Segment",
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold"), # Increase title font size
    plot.subtitle = element_text(size = 14) ,
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),
    legend.position = "top"
  )
