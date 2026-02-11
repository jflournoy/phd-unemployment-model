# Ordered Categorical Parameterization - Geometry Issues

**Date**: 2026-02-11
**Issue**: Ordered categorical model fails to converge despite independent hierarchical model working

## The Paradox

You're absolutely right to be suspicious! This IS strange:
- **Independent hierarchical parameters**: Converges fine
- **Add ordering constraint** (`ordered[N_edu]`): Model completely fails (75% max treedepth, Rhat > 2)

## The Mathematical Issue

### Ordered Categorical Parameterization

```stan
real sigma;              // Can be POSITIVE or NEGATIVE
ordered[N_edu] theta;    // Constrained: θ₁ ≤ θ₂ ≤ ... ≤ θ₇
parameter[i] = mu + sigma * theta[i]
```

### The Problem: Sign of Sigma Creates Phase Transition

1. **When sigma > 0** (positive):
   - parameter[1] < parameter[2] < ... < parameter[7] (increasing)
   - Less education → lower parameter value

2. **When sigma < 0** (negative):
   - parameter[1] > parameter[2] > ... > parameter[7] (decreasing)
   - Less education → higher parameter value

3. **When sigma ≈ 0**:
   - No trend, parameters collapse to mu
   - Ordering becomes irrelevant

### Geometric Issues

1. **Phase Transition at sigma = 0**:
   - The posterior has to navigate across sigma = 0
   - The interpretation completely flips at this boundary
   - Creates difficult curvature

2. **Funnel Geometry** (classic hierarchical model issue):
   - When sigma → 0, the theta parameters become increasingly constrained
   - Creates funnel-shaped posterior that's hard to sample
   - Ordered constraint makes this WORSE

3. **Boundary Effects**:
   - Ordered constraint creates hard boundaries: θᵢ ≤ θᵢ₊₁
   - If data weakly supports ordering, chains get stuck near boundaries
   - Creates label-switching-like behavior

4. **Multiple Modalities**:
   - Positive sigma (increasing trend) vs negative sigma (decreasing trend)
   - If data is ambiguous, posterior can be bimodal
   - Chains get trapped in different modes (explains high Rhat)

## Why Independent Hierarchical Works

```stan
vector[N_edu] raw;       // Unconstrained
parameter[i] = mu + sigma * raw[i]
```

- No phase transition (sigma just controls scale, not direction)
- No ordering constraint to create boundaries
- Standard funnel geometry that adapt_delta can handle

## The Irony

**The ordering constraint that was supposed to help (enforce monotonicity) actually hurts!**

- It restricts the parameter space
- Creates geometric pathologies
- Makes the model HARDER to fit, not easier

## Evidence from Failed Fit

```
75% transitions hit max treedepth
Rhat values: 2.43, 2.04, 2.10, 1.58 (chains in different regions)
ESS: 4.85, 5.34, 6.77 (essentially no effective samples)
```

This suggests:
- Chains exploring different modes (positive vs negative sigma?)
- Can't navigate the geometry even with 14 tree depth
- Fundamentally difficult posterior landscape

## Solutions

### Option 1: Monotonic I-Spline (RECOMMENDED)
```stan
vector<lower=0>[K_ispline] beta;  // Positive coefficients
parameter[i] = mu + dot_product(I_spline_edu[i], beta)
```

**Advantages**:
- Enforces monotonicity through POSITIVE coefficients (no sign ambiguity)
- No ordered constraint (smoother geometry)
- Flexible (can be nearly flat if data doesn't support trend)

### Option 2: Use Increments (Reparameterization)
```stan
real theta_raw[1];                      // First element
vector<lower=0>[N_edu-1] delta_raw;     // Positive increments
theta[1] = theta_raw[1];
for (i in 2:N_edu) {
  theta[i] = theta[i-1] + delta_raw[i-1];
}
```

**Advantages**:
- Avoids ordered constraint in parameter declaration
- Clearer geometric interpretation
- But still has issues with sigma sign ambiguity

### Option 3: Force Positive Sigma
```stan
real<lower=0> sigma;  // FORCE positive
ordered[N_edu] theta;
```

**Problems**:
- Forces trend to go one direction only
- Not what we want (should let data decide)
- Less flexible than I-spline

## Recommendation

**Use the Monotonic I-Spline approach** - it has the best geometric properties:
1. No sign ambiguity (always monotonic in one direction via positive coefficients)
2. No ordered constraint (smoother parameter space)
3. Flexible enough to be flat if data doesn't support trend
4. Prior on positive coefficients is well-behaved (exponential)

The ordered categorical parameterization looked elegant mathematically but creates severe geometric issues in practice.
