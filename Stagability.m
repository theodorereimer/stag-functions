(* ::Package:: *)

BeginPackage["Stagability`"]

binomialMass::usage = "binomialMass[n, p, {min, max}] returns the cumulative density of a binomial distribution from min to max. If a number is given instead of a List it will return the chance that that event occurs."
binomialMean::usage = "binomialMean[n, p] finds the mean of a binomial distribution"
binomialVariance::usage = "binomialVariance[n, p] finds the variance of a binomial distribution"

firstSuccessMass::usage = "firstSuccessMass[p, {min, max}] gives the cumulative mass of a first success distribution from min to max. If an integer is given instead of a range, it will use the pmf for that value."
firstSuccessMean::usage = "firstSuccessMean[p] gives the expected value for a FS distribution with probability of success p."
firstSuccessVariance::usage = "firstSuccessVariance[p] gives the variance of a FS distribution with probability of success p."

geometricMass::usage = "geometricMass[p, {min,max}] gives the cumulative mass of a geometric distribution from min to max. If an integer is given instead of a range, it will use the pmf for that value."
geometricDistributionMean::usage = "geometricDistributionMean[p]"
geometricVariance::usage = "geometricVariance[p]"

rSuccessMass::usage = "rSuccessMass[r, p, {min, max}] gives the cumulative mass of a rth success distribution from min to max. If an integer is given instead of a range, it will use the pmf for that value."
rSuccessMean::usage = "rSuccessMean[r, p]"
rSuccessVariance::usage = "rSuccessVariance[r, p]"

negativeBinomialMass::usage = "negativeBinomialMass[r, p, {min, max}] gives the cumulative mass of a negative binomial distribution from min to max. If an integer is given instead of a range, it will use the pmf for that value."
negativeBinomialMean::usage = "negativeBinomialMean[r, p]"
negativeBinomialVariance::usage = "negativeBinomialVariance[r, p]"

discreteUniformMass::usage = "discreteUniformMass[N, {min, max}] gives the cumulative mass of a discrete uniform distribution from min to max. If an integer is given instead of a range, it will use the pmf for that value."
discreteUniformMean::usage = "discreteUniformMean[N]"
discreteUniformVariance::usage = "discreteUniformVariance[N]"

poissonMass::usage = "poissonMass[\[Lambda], {min, max}] gives the cumulative mass of a Poisson distribution from min to max. If an integer is given instead of a range, it will use the pmf for that value."
poissonMean::usage = "poissonMean[\[Lambda]]"
poissonVariance::usage = "poissonVariance[\[Lambda]]"

hypergeometricMass::usage = "hypergeometricMass[w, b, n, {min, max}] gives the cumulative mass of a hypergeometric distribution from min to max. If an integer is given instead of a range, it will use the pmf for that value. \!\(\*
StyleBox[\"w\",\nFontSlant->\"Italic\"]\) is the number of one type of object, \!\(\*
StyleBox[\"b\",\nFontSlant->\"Italic\"]\) is the number of other objects, \!\(\*
StyleBox[\"n\",\nFontSlant->\"Italic\"]\) is the total number of things being chosen. \!\(\*
StyleBox[\"k\",\nFontSlant->\"Italic\"]\) is the number of things which are of type \!\(\*
StyleBox[\"w\",\nFontSlant->\"Italic\"]\)."
hypergeometricMean::usage = "hypergeometricMean[w, b, n]"
hypergeometricVariance::usage = "hypergeometricVariance[w, b, n]"

continuousUniformDensity::usage = "continuousUniformDensity[a, b, {min, max}]"
continuousUniformMean::usage = "continuousUniformMean[a, b]"
continuousUniformVariance::usage = "continuousUniformVariance[a, b]"

exponentialDensity::usage = "exponentialDensity[\[Lambda], {min, max}]"
exponentialMean::usage = "exponentialMean[\[Lambda]]"
exponentialVariance::usage = "exponentialVariance[\[Lambda]]"

gammaDensity::usage = "gammaDensity[a, \[Lambda], {min, max}] gives the probability for \!\(\*
StyleBox[\"a\",\nFontSlant->\"Italic\"]\) exponential distributions with parameter \[Lambda]"
gammaMean::usage = "gammaMean[a, \[Lambda]]"
gammaVariance::usage = "gammaVariance[a, \[Lambda]]"

(*
multinomialMass::usage = ""
multinomialMean::usage = ""
multinomialVariance::usage = ""
*)

deMontmort::usage = "Finds the probability for the matching hats problem with \!\(\*
StyleBox[\"n\",\nFontSlant->\"Italic\"]\) hats."

Begin["`Private`"]

binomialMass[n_,p_,k_Integer]:=Binomial[n,k](p)^k (1-p)^(n-k)
binomialMass[n_,p_,{min_,max_}]:=Sum[Binomial[n,i](p)^i (1-p)^(n-i),{i,min,max}]
binomialMean[n_,p_]:=n*p
binomialVariance[n_,p_]:=n*p*(1-p)

firstSuccessMass[p_, x_Integer]:=p (1-p)^(x-1)
firstSuccessMass[p_, {min_, max_}]:=Sum[p (1-p)^(i-1),{i,min,max}]
firstSuccessMean[p_]:=1/p
firstSuccessVariance[p_]:=(1-p)/(p^2)

geometricMass[p_, x_Integer]:=p (1-p)^x
geometricMass[p_, {min_, max_}]:=Sum[p (1-p)^i,{i,min,max}]
geometricDistributionMean[p_]:=(1-p)/p
geometricVariance[p_]:=(1-p)/(p^2)

rSuccessMass[r_,p_,x_Integer]:=Binomial[x-1,r-1] p^r (1-p)^(x-r)
rSuccessMass[r_, p_, {min_, max_}]:=Sum[Binomial[x-1,r-1] p^r (1-p)^(x-r),{x,min,max}]
rSuccessMean[r_, p_]:=r/p
rSuccessVariance[r_,p_]:=r (1-p)/(p^2)

negativeBinomialMass[r_, p_, x_Integer]:=Binomial[r+x-1,r-1] p^r (1-p)^x
negativeBinomialMass[r_, p_, {min_, max_}]:=Sum[Binomial[r+x-1,r-1] p^r (1-p)^x,{x,min,max}]
negativeBinomialMean[r_, p_]:=r (1-p)/p
negativeBinomialVariance[r_, p_]:=r (1-p)/(p^2)

discreteUniformMass[N_, x_Integer]:=1/N
discreteUniformMass[N_, {min_, max_}]:=(max-min+1)/N
discreteUniformMean[N_]:=(N+1)/2
discreteUniformVariance[N_]:=((N^2)-1)/12

poissonMass[\[Lambda]_, x_Integer]:=(\[Lambda]^x)/(Exp[\[Lambda]](x!))
poissonMass[\[Lambda]_, {min_, max_}]:=Sum[(\[Lambda]^x)/(Exp[\[Lambda]](x!)),{x,min,max}]
poissonMean[\[Lambda]_]:=\[Lambda]
poissonVariance[\[Lambda]_]:=\[Lambda]

hypergeometricMass[w_, b_, n_, k_Integer]:=Binomial[w,k]Binomial[b,n-k]/Binomial[w+b,n]
hypergeometricMass[w_, b_, n_, {min_, max_}]:=Sum[Binomial[w,k]Binomial[b,n-k]/Binomial[w+b,n],{k,min,max}]
hypergeometricMean[w_, b_, n_]:=n w/(w+b)
hypergeometricVariance[w_, b_, n_]:=n w b (w+b-n)/(((w+b)^2)(w+b-1))

continuousUniformDensity[a_,b_,{min_,max_}]:=(min-max)/(b-a)
continuousUniformMean[a_,b_]:=(b+a)/2
continuousUniformVariance[a_,b_]:=(b-a)^2/2

exponentialDensity[\[Lambda]_,{min_, max_}]:=Integrate[\[Lambda] Exp[-\[Lambda] x],{x,min,max}]
exponentialMean[\[Lambda]_]:=1/\[Lambda]
exponentialVariance[\[Lambda]_]:=1/\[Lambda]^2

gammaDensity[a_, \[Lambda]_, {min_, max_}]:=Integrate[\[Lambda]^a/(a-1)! x^(a-1) Exp[-\[Lambda] x],{x,min,max}]
gammaMean[a_, \[Lambda]_]:=a/\[Lambda]
gammaVariance[a_,\[Lambda]_]:=a/(\[Lambda]^2)

deMontmort[n_]:=Sum[((-1)^(i+1))/(i!),{i,1,n}]

End[]
EndPackage[]
