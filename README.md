# stag-functions
A collection of Mathematica packages and files I've created to help me do and see different things.  
Go to [this website](https://support.wolfram.com/5648?src=mathematica) to find out how to install and use packages.  

Currently the packages are in a very early state and the documentation is fairly limited. Fixing that is near the top of my priority list.
# Functions
## Stagability
Stagability has the pmf/pdf and cdf/cmf combined as one function for each of the distributions used in my Probability course, as well as functions for the mean and variance of those distributions. Discrete distributions are named `DistributionMass`, continuous ones named `DistributionDensity`. All of the mean and variance functions are named `DistributionMean` and `DistributionVariance`, except for the Geometric distribution, which has `GeometricDistributionMean[p]` since the Geometric Mean is already something.

- Discrete: Binomial, FirstSuccess, Geometric, rSuccess, NegativeBinomial, DiscreteUniform, Poisson, Hypergoemetric
- Continuous: ContinuousUniform, Exponential, Gamma

Each function has the distributions parameters as we used them in class, followed by the range that you want the probability of. For the discrete distributions, you can enter just an Integer for the range and it will give the probability of that point, or a List of two Integers and it will sum up every probability in that range. This avoids needing an extra function or two, since it lets you do a point `a`, the cumulative distribution as `{0,a}` or `{1,a}`, or any range as `{a,b}`.

The package also includes a function for deMontmort's matching hats problem for _n_ hats.
