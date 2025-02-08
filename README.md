Dcor

Το σενάρια είναι για n=c(50, 100, 200, 500, 1000, 2000, 5000, 10000).

x έρχονται από a) beta, b) skew normal, c) Cauchy, d) gamma, and e )von Mises distribution.

y έρχονται από mixtures of a ) 2 normals, b) mixtures of 3 normals, and c) mixtures of 2 skew t distribution.

Άρα έχουμε 5 * 3 = 15 σενάρια να δοκιμάσουμε για διάφορα μεγέθη δείγματος (n).



Κατεβάστε το πακέτο dcov.

Εκεί έχει την εντολή dcov που υπολογίζει το p-value με permutation  

dcor.test(x, y, R = 1000, type = "U")



The asymptotic p-value is computed via

stat <- n * dcor(x, y, type = "U")^2 + 1

p-value <- pchisq(stat, 1, lower.tail = FALSE).



R <- 1000

For every combination of distributions (15 scenarios)

for ( i in 1:R ) {

  generate n values x

  generate n values y

  compute the p-value via permutation and via the asymptotic approach, say p1 and p2.

 }



In the end compute the proportion of times the permutation based p-value was less than 0.05 and the proportion of times the asymptotic p-value was less than 0.05. If both approaches are correct, both numbers, ideally, should be close to 0.05. If not, still good, we have a finding.

In the end we have a table with 15 columns (15 scenarios) and 8 rows (sample sizes), with proportions for each approach.


