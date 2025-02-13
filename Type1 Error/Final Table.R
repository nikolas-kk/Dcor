library(kableExtra)
library(dplyr)
final_table <- t(as.data.frame(
  cbind(Beta_Res, Cauchy_Res, Gamma_Res
        , SkewNormal_Res, von_results)
))
row.names(final_table) <- ifelse(row.names(final_table) == "Permutations", "P", "A")
final_table_latex <- kable(
  final_table,
  format = 'latex',
  booktabs = TRUE,
  row.names = TRUE
) %>%
  kable_styling(full_width = TRUE) %>%
  pack_rows('Beta - Mixture2Normal', 1, 2, indent = FALSE) %>%
  pack_rows('Beta - Mixture3Normal', 3, 4, indent = FALSE) %>%
  pack_rows('Beta - Mixture2Skew.t', 5, 6, indent = FALSE) %>%
  pack_rows('Cauchy - Mixture2Normal', 7, 8, indent = FALSE) %>%
  pack_rows('Cauchy - Mixture3Normal', 9, 10, indent = FALSE) %>%
  pack_rows('Cauchy - Mixture2Skew.t', 11, 12, indent = FALSE) %>%
  pack_rows('Gamma - Mixture2Normal', 13, 14, indent = FALSE) %>%
  pack_rows('Gamma - Mixture3Normal', 15, 16, indent = FALSE) %>%
  pack_rows('Gamma - Mixture2Skew.t', 17, 18, indent = FALSE) %>%
  pack_rows('Skewed Normal - Mixture2Normal', 19, 20, indent = FALSE) %>%
  pack_rows('Skewed Normal - Mixture3Normal', 21, 22, indent = FALSE) %>%
  pack_rows('Skewed Normal - Mixture2Skew.t', 23, 24, indent = FALSE) %>%
  pack_rows('Von Mises - Mixture2Normal', 25, 26, indent = FALSE) %>%
  pack_rows('Von Mises - Mixture3Normal', 27, 28, indent = FALSE) %>%
  pack_rows('Von Mises - Mixture2Skew.t', 29, 30, indent = FALSE)

#--------------Τελικός Κώδικας Latex--------------------------------
final_table_latex_code <-#Με πακέτο tabu
"\begin{table}[h!] 
\centering
\begin{tabu} to \linewidth {>{\raggedright}X>{\raggedleft}X>{\raggedleft}X>{\raggedleft}X>{\raggedleft}X>{\raggedleft}X>{\raggedleft}X>{\raggedleft}X>{\raggedleft}X}
\toprule
\toprule
  \textbf{N}& \textbf{50} & \textbf{100} & \textbf{200} & \textbf{500} & \textbf{1000} & \textbf{2000} & \textbf{5000} & \textbf{10000}\\
\midrule
\midrule
\multicolumn{9}{l}{\textbf{Beta - Mixture2Normal}}\\
P & 0.049 & 0.060 & 0.045 & 0.039 & 0.051 & 0.054 & 0.053 & 0.045\\
A & 0.053 & 0.060 & 0.043 & 0.036 & 0.047 & 0.049 & 0.047 & 0.040\\
\midrule
\multicolumn{9}{l}{\textbf{Beta - Mixture3Normal}}\\
P & 0.055 & 0.046 & 0.050 & 0.060 & 0.049 & 0.063 & 0.051 & 0.047\\
A & 0.053 & 0.047 & 0.050 & 0.060 & 0.041 & 0.054 & 0.051 & 0.044\\

\midrule
\multicolumn{9}{l}{\textbf{Beta - Mixture2Skew.t}}\\
P & 0.055 & 0.050 & 0.045 & 0.064 & 0.040 & 0.051 & 0.055 & 0.048\\
A & 0.051 & 0.050 & 0.047 & 0.058 & 0.031 & 0.046 & 0.048 & 0.039\\

\midrule
\multicolumn{9}{l}{\textbf{Cauchy - Mixture2Normal}}\\
P & 0.050 & 0.056 & 0.045 & 0.044 & 0.046 & 0.047 & 0.047 & 0.046\\
A & 0.049 & 0.053 & 0.037 & 0.036 & 0.036 & 0.042 & 0.039 & 0.037\\

\midrule
\multicolumn{9}{l}{\textbf{Cauchy - Mixture3Normal}}\\
P & 0.060 & 0.052 & 0.054 & 0.052 & 0.059 & 0.051 & 0.044 & 0.060\\
A & 0.057 & 0.043 & 0.047 & 0.045 & 0.047 & 0.040 & 0.039 & 0.051\\

\midrule
\multicolumn{9}{l}{\textbf{Cauchy - Mixture2Skew.t}}\\
P & 0.060 & 0.064 & 0.055 & 0.043 & 0.045 & 0.051 & 0.054 & 0.052\\
A & 0.046 & 0.044 & 0.031 & 0.024 & 0.026 & 0.021 & 0.031 & 0.024\\

\midrule
\multicolumn{9}{l}{\textbf{Gamma - Mixture2Normal}}\\
P & 0.045 & 0.051 & 0.052 & 0.041 & 0.050 & 0.039 & 0.047 & 0.042\\
A & 0.049 & 0.046 & 0.050 & 0.047 & 0.049 & 0.034 & 0.048 & 0.041\\

\midrule
\multicolumn{9}{l}{\textbf{Gamma - Mixture3Normal}}\\
P & 0.048 & 0.053 & 0.051 & 0.042 & 0.051 & 0.052 & 0.050 & 0.044\\
A & 0.046 & 0.051 & 0.047 & 0.042 & 0.049 & 0.048 & 0.047 & 0.043\\
\midrule
\multicolumn{9}{l}{\textbf{Gamma - Mixture2Skew.t}}\\
P & 0.033 & 0.048 & 0.055 & 0.048 & 0.052 & 0.046 & 0.040 & 0.057\\
A & 0.031 & 0.044 & 0.044 & 0.041 & 0.038 & 0.043 & 0.033 & 0.050\\
\midrule
\multicolumn{9}{l}{\textbf{Skewed Normal - Mixture2Normal}}\\
P & 0.052 & 0.053 & 0.040 & 0.053 & 0.042 & 0.055 & 0.050 & 0.046\\
A & 0.052 & 0.053 & 0.036 & 0.052 & 0.038 & 0.050 & 0.047 & 0.044\\
\midrule
\multicolumn{9}{l}{\textbf{Skewed Normal - Mixture3Normal}}\\
P & 0.047 & 0.049 & 0.049 & 0.052 & 0.044 & 0.051 & 0.046 & 0.045\\
A & 0.045 & 0.045 & 0.048 & 0.048 & 0.045 & 0.050 & 0.040 & 0.045\\
\midrule
\multicolumn{9}{l}{\textbf{Skewed Normal - Mixture2Skew.t}}\\
P & 0.050 & 0.043 & 0.058 & 0.044 & 0.055 & 0.056 & 0.056 & 0.056\\
A & 0.047 & 0.039 & 0.050 & 0.040 & 0.044 & 0.054 & 0.048 & 0.050\\
\midrule
\multicolumn{9}{l}{\textbf{Von Mises - Mixture2Normal}}\\
P & 0.049 & 0.039 & 0.046 & 0.040 & 0.061 & 0.050 & 0.055 & 0.052\\
A & 0.054 & 0.035 & 0.042 & 0.036 & 0.057 & 0.040 & 0.047 & 0.046\\
\midrule
\multicolumn{9}{l}{\textbf{Von Mises - Mixture3Normal}}\\
P & 0.049 & 0.056 & 0.042 & 0.051 & 0.049 & 0.048 & 0.044 & 0.053\\
A & 0.047 & 0.050 & 0.041 & 0.049 & 0.051 & 0.043 & 0.041 & 0.045\\
\midrule
\multicolumn{9}{l}{\textbf{Von Mises - Mixture2Skew.t}}\\
P & 0.055 & 0.045 & 0.053 & 0.048 & 0.046 & 0.049 & 0.047 & 0.044\\
A & 0.052 & 0.038 & 0.044 & 0.042 & 0.037 & 0.044 & 0.041 & 0.034\\
\bottomrule
\bottomrule
\end{tabu}
\end{table}"
