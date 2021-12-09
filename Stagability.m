(* ::Package:: *)

BeginPackage["Stagability`"]

BinomialMass::usage = "BinomialCDF[n, p, {min, max}] returns the cumulative density of a binomial distribution from min to max. If a number is given instead of a List it will return the chance that that event occurs."
BinomialMean::usage = "BinomialMean[n, p] finds the mean of a binomial distribution"
BinomialVariance::usage = "BinomialVariance[n, p] finds the variance of a binomial distribution"

FirstSuccessMass::usage = "FirstSuccessMass[p, {min, max}] gives the cumulative mass of a first success distribution from min to max. If an integer is given instead of a range, it will use the pmf for that value."
FirstSuccessMean::usage = "FirstSuccessMean[p] gives the expected value for a FS distribution with probability of success p."
FirstSuccessVariance::usage = "FirstSuccessVariance[p] gives the variance of a FS distribution with probability of success p."

GeometricMass::usage = "GeometricMass[p, {min,max}] gives the cumulative mass of a geometric distribution from min to max. If an integer is given instead of a range, it will use the pmf for that value."
GeometricDistributionMean::usage = "GeometricDistributionMean[p]"
GeometricVariance::usage = "GeometricVariance[p]"

rSuccessMass::usage = "rSuccessMass[r, p, {min, max}] gives the cumulative mass of a rth success distribution from min to max. If an integer is given instead of a range, it will use the pmf for that value."
rSuccessMean::usage = "rSuccessMean[r, p]"
rSuccessVariance::usage = "rSuccessVariance[r, p]"

NegativeBinomialMass::usage = "NegativeBinomialMass[r, p, {min, max}] gives the cumulative mass of a negative binomial distribution from min to max. If an integer is given instead of a range, it will use the pmf for that value."
NegativeBinomialMean::usage = "NegativeBinomialMean[r, p]"
NegativeBinomialVariance::usage = "NegativeBinomialVariance[r, p]"

DiscreteUniformMass::usage = "DiscreteUniformMass[N, {min, max}] gives the cumulative mass of a discrete uniform distribution from min to max. If an integer is given instead of a range, it will use the pmf for that value."
DiscreteUniformMean::usage = "DiscreteUniformMean[N]"
DiscreteUniformVariance::usage = "DiscreteUniformVariance[N]"

PoissonMass::usage = "PoissonMass[\[Lambda], {min, max}] gives the cumulative mass of a Poisson distribution from min to max. If an integer is given instead of a range, it will use the pmf for that value."
PoissonMean::usage = "PoissonMean[\[Lambda]]"
PoissonVariance::usage = "PoissonVariance[\[Lambda]]"

HypergeometricMass::usage = "HypergeometricMass[w, b, n, {min, max}] gives the cumulative mass of a hypergeometric distribution from min to max. If an integer is given instead of a range, it will use the pmf for that value."
HypergeometricMean::usage = "HypergeometricMean[w, b, n]"
HypergeometricVariance::usage = "HypergeometricVariance[w, b, n]"

ContinuousUniformDensity::usage = "ContinuousUniformDensity[a, b, {min, max}]"
ContinuousUniformMean::usage = "ContinuousUniformMean[a, b]"
ContinuousUniformVariance::usage = "ContinuousUniformVariance[a, b]"

ExponentialDensity::usage = "ExponentialDensity[\[Lambda], {min, max}]"
ExponentialMean::usage = "ExponentialMean[\[Lambda]]"
ExponentialVariance::usage = "ExponentialVariance[\[Lambda]]"

GammaDensity::usage = "GammaDensity[a, \[Lambda], {min, max}] gives the probability for \!\(\*
StyleBox[\"a\",\nFontSlant->\"Italic\"]\) exponential distributions with parameter \[Lambda]"
GammaMean::usage = "GammaMean[a, \[Lambda]]"
GammaVariance::usage = "GammaVariance[a, \[Lambda]]"

(*
MultinomialMass::usage = ""
MultinomialMean::usage = ""
MultinomialVariance::usage = ""
*)

deMontmort::usage = "Finds the probability for the matching hats problem with \!\(\*
StyleBox[\"n\",\nFontSlant->\"Italic\"]\) hats."

Begin["`Private`"]

BinomialMass[n_,p_,k_Integer]:=Binomial[n,k](p)^k (1-p)^(n-k)
BinomialMass[n_,p_,{min_,max_}]:=Sum[Binomial[n,i](p)^i (1-p)^(n-i),{i,min,max}]
BinomialMean[n_,p_]:=n*p
BinomialVariance[n_,p_]:=n*p*(1-p)

FirstSuccessMass[p_, x_Integer]:=p (1-p)^(x-1)
FirstSuccessMass[p_, {min_, max_}]:=Sum[p (1-p)^(i-1),{i,min,max}]
FirstSuccessMean[p_]:=1/p
FirstSuccessVariance[p_]:=(1-p)/(p^2)

GeometricMass[p_, x_Integer]:=p (1-p)^x
GeometricMass[p_, {min_, max_}]:=Sum[p (1-p)^i,{i,min,max}]
GeometricDistributionMean[p_]:=(1-p)/p
GeometricVariance[p_]:=(1-p)/(p^2)

rSuccessMass[r_,p_,x_Integer]:=Binomial[x-1,r-1] p^r (1-p)^(x-r)
rSuccessMass[r_, p_, {min_, max_}]:=Sum[Binomial[x-1,r-1] p^r (1-p)^(x-r),{x,min,max}]
rSuccessMean[r_, p_]:=r/p
rSuccessVariance[r_,p_]:=r (1-p)/(p^2)

NegativeBinomialMass[r_, p_, x_Integer]:=Binomial[r+x-1,r-1] p^r (1-p)^x
NegativeBinomialMass[r_, p_, {min_, max_}]:=Sum[Binomial[r+x-1,r-1] p^r (1-p)^x,{x,min,max}]
NegativeBinomialMean[r_, p_]:=r (1-p)/p
NegativeBinomialVariance[r_, p_]:=r (1-p)/(p^2)

DiscreteUniformMass[N_, x_Integer]:=1/N
DiscreteUniformMass[N_, {min_, max_}]:=(max-min+1)/N
DiscreteUniformMean[N_]:=(N+1)/2
DiscreteUniformVariance[N_]:=((N^2)-1)/12

PoissonMass[\[Lambda]_, x_Integer]:=(\[Lambda]^x)/(Exp[\[Lambda]](x!))
PoissonMass[\[Lambda]_, {min_, max_}]:=Sum[(\[Lambda]^x)/(Exp[\[Lambda]](x!)),{x,min,max}]
PoissonMean[\[Lambda]_]:=\[Lambda]
PoissonVariance[\[Lambda]_]:=\[Lambda]

HypergeometricMass[w_, b_, n_, k_Integer]:=Binomial[w,k]Binomial[b,n-k]/Binomial[w+b,n]
HypergeometricMass[w_, b_, n_, {min_, max_}]:=Sum[Binomial[w,k]Binomial[b,n-k]/Binomial[w+b,n],{k,min,max}]
HypergeometricMean[w_, b_, n_]:=n w/(w+b)
HypergeometricVariance[w_, b_, n_]:=n w b (w+b-n)/(((w+b)^2)(w+b-1))

ContinuousUniformDensity[a_,b_,{min_,max_}]:=(min-max)/(b-a)
ContinuousUniformMean[a_,b_]:=(b+a)/2
ContinuousUniformVariance[a_,b_]:=(b-a)^2/2

ExponentialDensity[\[Lambda]_,{min_, max_}]:=Integrate[\[Lambda] Exp[-\[Lambda] x],{x,min,max}]
ExponentialMean[\[Lambda]_]:=1/\[Lambda]
ExponentialVariance[\[Lambda]_]:=1/\[Lambda]^2

GammaDensity[a_, \[Lambda]_, {min_, max_}]:=Integrate[\[Lambda]^a/(a-1)! x^(a-1) Exp[-\[Lambda] x],{x,min,max}]
GammaMean[a_, \[Lambda]_]:=a/\[Lambda]
GammaVariance[a_,\[Lambda]_]:=a/(\[Lambda]^2)

deMontmort[n_]:=Sum[((-1)^(i+1))/(i!),{i,1,n}]

End[]
EndPackage[]
