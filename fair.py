"""

library(dplyr)
library(mc2d)
library(ggplot2)
library(ggthemes)

loss_event_frequency_min <- 3
loss_event_frequency_max <- 12
loss_event_frequency_likely <- 5
loss_magnitude_min <- 125000
loss_magnitude_max <- 3500000
loss_magnitude_likely <- 750000
confidence <- 4 # default in PERT
number_of_runs <- 10000
set.seed(88881111)
LEF <- rpert(number_of_runs, loss_event_frequency_min, loss_event_frequency_likely, loss_event_frequency_max, shape = confidence)
LM <- rpert(number_of_runs, loss_magnitude_min, loss_magnitude_likely, loss_magnitude_max, shape = confidence)
annual_loss_exposure <- LEF * LM
crude_ALE <- annual_loss_exposure
ALE <- sapply(LEF, function(e) sum(rpert(e, loss_magnitude_min, loss_magnitude_likely, loss_magnitude_max, shape = confidence)))
max_loss <- max(ALE)
min_loss <- min(ALE)
ale_frame <- data.frame(ALE)
most <- max(ALE)
gg <- ggplot(ale_frame, aes(x = ALE))
gg <- gg + geom_histogram(aes(y = ..density..),
color="black",
fill = "white",
binwidth = 5000)
gg <- gg + geom_density(fill = "steelblue", alpha = 1/3)
gg <- gg + theme_bw()
gg
ale_frame <- mutate(ale_frame, prob = 1 - percent_rank(ALE))
ale_frame <- ale_frame[order(ALE),]
g2 <- ggplot(ale_frame, mapping = aes(x = ALE, y = prob))
g2 <- g2 + geom_path() + scale_y_continuous(labels = percent)
g2 <- g2 + scale_x_continuous(labels = dollar)
g2 <- g2 + annotate("text", y = 0.1, x = max(ALE),
label = dollar(max(ALE)), vjust = -1)
g2 <- g2 + annotate("text", y = 0.10, x = 0, label = percent(0.1), vjust = -1)
g2 <- g2 + annotate("text", y = 0, x = quantile(ALE, c(0.90)),
label = dollar(quantile(ALE, c(0.90))), hjust = 0.5)
g2 <- g2 + geom_segment(aes(x = 0, y = 0.1, xend = quantile(ALE, c(0.90)), yend = 0.1), lty = "dotted")
g2 + theme_few()
"""

# from pert import PERT
# import seaborn as sns
# import matplotlib.pyplot as plt

# pert = PERT(1,2,3).rvs(4)
# print(pert)
# exit(1)

# # pert = PERT(10, 190, 200)
# # sns.kdeplot(pert.rvs(10000))
# # plt.show()


# loss_event_frequency_min = 3
# loss_event_frequency_max = 12
# loss_event_frequency_likely = 5
# loss_magnitude_min = 125000
# loss_magnitude_max = 3500000
# loss_magnitude_likely = 750000
# confidence = 4 # default in PERT
# number_of_runs = 10000


# LEF = PERT(loss_event_frequency_min, loss_event_frequency_likely, loss_event_frequency_max, lamb=confidence).rvs(number_of_runs)
# LM = PERT(loss_magnitude_min, loss_magnitude_likely, loss_magnitude_max, lamb=confidence).rvs(number_of_runs)


# annual_loss_exposure = LEF * LM
# crude_ALE = annual_loss_exposure

# ALE = [sum(PERT(loss_magnitude_min, loss_magnitude_likely, loss_magnitude_max, lamb=confidence).rvs(e)) for e in LEF]
# # ALE = sapply(LEF, function(e) sum(rpert(e, loss_magnitude_min, loss_magnitude_likely, loss_magnitude_max, shape = confidence)))





# from src.model.model import FairModel
# from src.model.meta_model import FairMetaModel
# from src.report.simple_report import FairSimpleReport

from Fair import FairModel
from Fair import FairMetaModel
from Fair import FairSimpleReport

# Create using LEF (PERT), PL, (PERT), and SL (constant)
model1 = FairModel(name="Model 1", n_simulations=10000)
model1.input_data('Loss Event Frequency', low=20, mode=100, high=900)
model1.input_data('Primary Loss', low=3000000, mode=3500000, high=5000000)
model1.input_data('Secondary Loss', constant=3_500000)
model1.calculate_all()

# Create another model using LEF (Normal) and LM (PERT)
model2 = FairModel(name="Model 2", n_simulations=10000)
model2.input_data('Loss Event Frequency', mean=.3, stdev=.1)
model2.input_data('Loss Magnitude', low=2000000000, mode=3000000000, high=5000000000)
model2.calculate_all()

model3 = FairModel(name='Model 3', n_simulations=10000)
model3.input_data('Loss Event Frequency', low=3, mode=5, high=12)
model3.input_data('Loss Magnitude', low=125000, mode=750000, high=3500000)
model3.calculate_all()



# Create metamodel by combining 1 and 2
mm = FairMetaModel(name='Meta Model', models=[model1, model2])
mm.calculate_all()

# Create report comparing 2 vs metamodel.
fsr = FairSimpleReport([model3,mm])
fsr.to_html('output.html')


