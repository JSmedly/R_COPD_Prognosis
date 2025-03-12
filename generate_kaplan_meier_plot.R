library(ggsurvfit)
library(survival)
library(svglite)

load("data/survival/surv_data.Rda")
# Kaplan-Meier curve
sf <- survfit2(Surv(surv_time/12, status) ~ strata(sex),
         data = surv.data)
sf %>%
  ggsurvfit() +
  add_confidence_interval() +
  add_risktable(risktable_stats = "{n.risk} ({cum.event})",
                stats_label = list(n.risk = "Number at Risk",
                                   cum.event = "Events")) +
  add_risktable_strata_symbol() +
  add_legend_title(c("Strata")) +
  lims(y=c(0,1)) +
  labs(x="Survival Time (years)") +
  scale_color_manual(values = c("#F8766D", "#00BFC4"),
                     labels = c("Female", "Male")) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"),
                    labels = c("Female", "Male"))
ggsave("results/survival/kaplan_meier.svg",
       dpi=300, height=5, width=7, unit="in")


# Simultaneously generate survival stats
survival_summary <- as.data.frame(summary(sf)$table) %>%
  mutate(event.pct = events/records) %>%
  select(records, events, event.pct, median)
rownames(survival_summary) <- c("Female", "Male")

ft <- flextable(survival_summary %>% rownames_to_column("Sex")) %>% 
  colformat_double(j="median", digits=2) %>%
  # colformat_int(j="events") %>%
  mk_par(j="events", 
         value = as_paragraph(fmt_n_percent(events, event.pct, digit=1))) %>%
  delete_columns("event.pct") %>%
  set_header_labels(records="n",
                    events="Deaths",
                    median="Median Survival Time (years)") %>%
  autofit() %>%
  theme_vanilla() %>%
  align(align = "center", part="header")
save_as_docx(ft, 
             path="results/survival/survival_stats.docx")