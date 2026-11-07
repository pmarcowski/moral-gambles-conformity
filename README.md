# Social conformity in moral and monetary gambles

This repository contains the prepared datasets and Quarto analysis notebooks for the paper ["People conform to social norms when gambling with lives or money"](https://doi.org/10.1038/s41598-023-27462-1) by Jiang, Marcowski, Ryazanov, and Winkielman (2023), published in *Scientific Reports*.

Across two longitudinal experiments (*N* = 291 participants: *N* = 138 in Experiment 1, moral decisions about animal lives; *N* = 153 in Experiment 2, monetary decisions), we adapted the Asian Disease Paradigm to examine how observing risk-averse or risk-seeking group norms shapes personal risk preferences under gain and loss frames. Participants completed a baseline gamble-acceptance phase (`Baseline`), observed and predicted group choices with feedback (`Learning`), and reassessed their own choices immediately (`OwnD0`) and after a three-day delay (`OwnD3`, alongside predictions of another group's choices, `Others`). Mixed-effects logistic regression models (`glmmTMB`) show that participants shift their risk preferences toward observed social norms in both moral and monetary domains, that these conformity effects persist three days later, and that the relative strength of risk-averse versus risk-seeking norms depends on the interaction of framing (`Gains` vs. `Losses`) and domain (`Lives` vs. `Money`).

## Layout

| Path | Contents |
|---|---|
| `analysis/index.qmd` | Website overview: study summary, overview figure across stages and domains, and notebook links. |
| `analysis/day0.qmd` | Day 0 analysis notebook: baseline risk preferences, immediate conformity, and cross-domain comparisons. |
| `analysis/day3.qmd` | Day 3 follow-up notebook: three-day persistence of conformity and four-phase comparisons. |
| `analysis/_quarto.yml` | Quarto website configuration; renders to `analysis/_site/`. |
| `analysis/mgc.scss` | Website typography and table styling. |
| `data/` | Prepared trial-level datasets (`experiment1.Rds` and `experiment2.Rds`). |
| `R/prepare_data.R` | Data preparation script documenting the exclusion criteria and variable construction for `data/experiment{1,2}.Rds`. |
| `output/` | Cached `glmmTMB` model fits (`models_day0.Rds`, `models_day3.Rds`), exported figures (`fig2.pdf`–`fig6.pdf`), and summary tables (`key_results_day0.csv`, `key_results_day3.csv`) generated when rendering `analysis/`. |

## Reproduce

Requirements: R (4.5 or later) and Quarto (1.10 or later). Install the required R packages once:

```r
install.packages(c(
  "tidyverse", "glmmTMB", "emmeans", "easystats",
  "patchwork", "ggsignif", "cowplot", "broom", "here", "withr",
  "knitr", "rmarkdown"
))
```

Render the Quarto analysis notebooks and overview website from the prepared datasets in `data/` to `analysis/_site/` (also fits and caches `output/models_day0.Rds` and `output/models_day3.Rds` and exports `output/fig2.pdf`–`output/fig6.pdf`):

```bash
quarto render analysis
```

## Citation

If you use this code or data in your research, please cite:

> Jiang, Y., Marcowski, P., Ryazanov, A., & Winkielman, P. (2023). People conform to social norms when gambling with lives or money. *Scientific Reports*, *13*, 853. https://doi.org/10.1038/s41598-023-27462-1

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
