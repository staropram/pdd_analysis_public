library(tmap)
library(tmaptools)
library(ggplot2)
library(RColorBrewer)
library(colorspace)

create_trends_plot <- function() {
   graphics.off()
   dev.new()

   # Melt the data to long format
   reoffendingTrends_long <- reshape2::melt(reoffendingTrends, id.vars = "PoliceForce", variable.name = "Year", value.name = "ReoffendingRate")

   # Convert Year to numeric (assuming it's in character format)
   reoffendingTrends_long$YearNum <- as.numeric(as.character(reoffendingTrends_long$Year))-2000

   # Plot
   gplot <- ggplot(reoffendingTrends_long, aes(x = YearNum, y = ReoffendingRate, color = PoliceForce)) +
     geom_line() +
     labs(title = "Reoffending Proportion Trends",
        x = "Year",
        y = "Reoffending proportion (%)",
        color = "Police Force") +
     scale_x_continuous(breaks = seq(min(reoffendingTrends_long$YearNum), max(reoffendingTrends_long$YearNum), by = 1)) +
     theme_minimal()
  print(gplot)
}
create_trends_plot()
