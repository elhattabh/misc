
library(ggplot2)

# data=read.csv('/Users/helhattab/Downloads/results-20200108-174129.csv', header = TRUE, stringsAsFactors = FALSE)
# 
# 
# ggplot(data, aes(x = period, y = pct_reg, fill = persona, label = pct_reg)) +
#   geom_bar(stat = "identity") +
#   geom_text(size = 3, position = position_stack(vjust = 0.5))+
#   ggtitle(label = "                                          Registration Distribution by Persona")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
#   xlab("Month")+
#   ylab("Percent Of Registered Users")
# 
# 
# ggplot(data, aes(x = period, y = pct_init_funding_bypersona, fill = persona, label = pct_init_funding_bypersona)) +
#   geom_bar(stat = "identity") +
#   geom_text(size = 3, position = position_stack(vjust = 0.5))+
#   ggtitle(label = "                                          Funded Client Distribution by Persona")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
#   xlab("Month")+
#   ylab("Percent Of Funded Clients")




personatiers=read.csv('/Users/helhattab/Downloads/persona_tierchoice.csv', header = TRUE, stringsAsFactors = FALSE)

personatiers_anabelle<-dplyr::filter(personatiers, persona=='Anabelle (25+, Under 50k)')

ggplot(personatiers_anabelle, aes(x = group_given, y = pct, fill = q_likelytier, label = pct)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle(label = "                                         Anabelle Likely Tier Distribution by Variant")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  xlab("Price Variant")+
  ylab("Percent of Responders")


personatiers_anabelle<-dplyr::filter(personatiers, persona=='Elle (25+,50K+)')

ggplot(personatiers_anabelle, aes(x = group_given, y = pct, fill = q_likelytier, label = pct)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle(label = "                                         Elle Likely Tier Distribution by Variant")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  xlab("Price Variant")+
  ylab("Percent of Responders")

#by just pricing
overalltiers=read.csv('/Users/helhattab/Downloads/results-20200109-173620.csv', header = TRUE, stringsAsFactors = FALSE)

ggplot(overalltiers, aes(x = group_given, y = pct, fill = q_likelytier, label = pct)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle(label = "                                         Overall Likely Tier Distribution by Variant")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  xlab("Price Variant")+
  ylab("Percent of Responders")

# persona interest
interestpersona=read.csv('/Users/helhattab/Downloads/results-20200109-174104.csv', header = TRUE, stringsAsFactors = FALSE)

ggplot(interestpersona, aes(x = persona, y = pct, fill = interest_level_net, label = pct)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle(label = "                                          Interest by Persona")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  xlab("Persona")+
  ylab("Percent of Responders")

# likelihood to subscribe by pricing
subscribebyprice=read.csv('/Users/helhattab/Downloads/results-20200109-174403.csv', header = TRUE, stringsAsFactors = FALSE)

ggplot(subscribebyprice, aes(x = group_given, y = pct, fill = likelihood_to_subscribe_net, label = pct)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle(label = "                                          Likelihood to Subscribe by Price Variant")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  xlab("Price Variant")+
  ylab("Percent of Responders")

#likelihood to subscribe by persona 
subscribebypersona=read.csv('/Users/helhattab/Downloads/results-20200109-174925.csv', header = TRUE, stringsAsFactors = FALSE)

ggplot(subscribebypersona, aes(x = persona, y = pct, fill = likelihood_to_subscribe_net, label = pct)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle(label = "                                          Likelihood to Subscribe by Persona")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  xlab("Persona")+
  ylab("Percent of Responders")

#nopricequestion, this question is working. here is proof:
noprice_byprice=read.csv('/Users/helhattab/Downloads/results-20200109-175209.csv', header = TRUE, stringsAsFactors = FALSE)

ggplot(noprice_byprice, aes(x = group_given, y = pct, fill = qtier_noprice, label = pct)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle(label = "                                      Tier Choice, Price Agnostic, by Variant")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  xlab("Price Variant")+
  ylab("Percent of Responders")


maxprice_bylikelytier=read.csv('/Users/helhattab/Downloads/results-20200109-204203.csv', header = TRUE, stringsAsFactors = FALSE)

ggplot(maxprice_bylikelytier, aes(x=q_likelytier, y=highest_price_raw, fill=group_given, label=highest_price_raw)) +
  geom_bar(stat='identity', position='dodge')+
  geom_text(size = 3, position = position_stack(vjust = 0.5))
  
ggplot(maxprice_bylikelytier, aes(x=group_given  , y=highest_price_raw, fill=q_likelytier, label=highest_price_raw)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle(label = "                                      Average Highest Price by Tier by Price Variant")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  xlab("Price Variant")+
  ylab("Average Highest Price")

