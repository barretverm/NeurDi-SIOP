library(ggplot2)

plot_ksearch <- function(sk, family = "Times New Roman", label_size = 3.6) {
  ggplot(as.data.frame(sk$results),
         aes(x = as.numeric(semcoh), y = as.numeric(exclus))) +
    geom_text(aes(label = K),
              show.legend = FALSE,
              check_overlap = FALSE,
              size = label_size,
              family = family) +
    labs(x = "Semantic coherence", y = "Exclusivity") +
    theme_classic() +
    theme(text = element_text(size = 12, family = family))
}

# usage:
# plot_excl_coh(sk2)
