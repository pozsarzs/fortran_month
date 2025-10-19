# Square Root Calculation with Newton–Raphson Method

The **Newton–Raphson method** is an iterative technique for finding successively better approximations to the roots of a real-valued function.

To compute the **square root** of a positive number $S$, we solve the equation:

$$
f(x) = x^2 - S = 0
$$

The Newton–Raphson iteration formula is:

$$
x_{n+1} = x_n - \frac{f(x_n)}{f'(x_n)}
$$

Substituting $f(x) = x^2 - S$ and $f'(x) = 2x$ gives:

$$
x_{n+1} = \frac{1}{2}\left(x_n + \frac{S}{x_n}\right)
$$

### Explanation
- Start with an initial guess $x_0$ (for example, $x_0 = S/2$).
- Repeat the formula until the value stabilizes (the difference between iterations becomes very small).
- The process converges quickly to $\sqrt{S}$ for any positive $S$.

### Example
To find $\sqrt{25}$:

1. Choose $x_0 = 12.5$  
2. Compute:
   - $x_1 = \frac{1}{2}(12.5 + 25/12.5) = 7.25$
   - $x_2 = \frac{1}{2}(7.25 + 25/7.25) \approx 5.349$
   - $x_3 = \frac{1}{2}(5.349 + 25/5.349) \approx 5.011$
3. After a few iterations, $x_n \to 5.0$

Thus, $\sqrt{25} \approx 5.0$ with rapid convergence.
