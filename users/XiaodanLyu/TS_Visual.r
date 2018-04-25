library(plotly)
library(lubridate)

train_items <- train %>% left_join(items, by = c("pid", "size")) #%>%
  # filter(date != ymd("2017-11-24"))

items %>% filter(pid %in% c(22144, 15845, 18453, 15413, 18415), grepl("^L|M", size)) -> expop_items

TS_daily <- train_items %>%
  anti_join(expop_items, by = c("pid", "size")) %>%
  group_by(date, group = paste(ctgroup, category)) %>%
  summarise(sumunits = sum(units),
            nitems = n(),
            medunits = median(units),
            avgunits = mean(units),
            IQunits = quantile(units, c(0.25, 0.75)) %>% diff,
            q75units = quantile(units, 0.75),
            upperfence = quantile(units, 0.75) + 3*IQunits) %>% ungroup

startup <- expand.grid(date = seq(ymd("2017-10-01"), ymd("2018-01-31"), 1),
                       group = with(items, paste(ctgroup, category)) %>% unique)
TS_daily_all <- startup %>% full_join(TS_daily, by = c("date", "group")) %>%
  mutate_at(vars(sumunits:q75units), funs(ifelse(is.na(.), 0, .)))

lag <- 7
TS_d_lag <- TS_daily_all %>% mutate(date = date + lag)

TS <- rbind(data.frame(TS_daily_all, source = "raw"),
            data.frame(TS_d_lag, source = paste0("lag", lag)))

plot_ly(data = TS,
        x = ~date, y = ~sumunits,
        color = ~paste(source, group), text = ~weekdays(date),
        type = "scatter", mode = "markers+lines")

TS_NP <- TS_daily %>% 
  inner_join(TS_d_lag, by = c("date", "group"), suffix = c("", ".lag")) %>%
  mutate(sumunits_np = sumunits - sumunits.lag)

plot_ly(data = TS_NP,
        x = ~date, y = ~sumunits_np,
        color = ~group, text = ~weekdays(date),
        type = "scatter", mode = "markers+lines") 

train_items %>% mutate(group = paste(ctgroup, category)) %>%
  left_join(TS_daily, by = c("date", "group")) %>%
  filter(units > 10*avgunits, units > 0.10*sumunits) %>%
  mutate(p = units/sumunits) %>% arrange(desc(p)) %>%
  select(date, pid, size, units, avgunits, nitems, sumunits, p, everything()) -> items.outlier

train_items %>% filter(pid == 12985) %>% 
  plot_ly(x = ~date, y = ~units, color = ~size, type = "scatter", mode = "markers+lines")
