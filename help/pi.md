# Calculating value of Pi with some methods

## 1. Wallis product

The Wallis product is the infinite product representation of Pi:

$$
\frac{\pi}{2} = \prod_{n=1}^{\infty} \frac{(2n)^2}{(2n-1)(2n+1)}
$$

in other format:

$$
\frac{\pi}{2} = \frac{2 \cdot 2}{1 \cdot 3} \cdot \frac{4 \cdot 4}{3 \cdot 5} \cdot \frac{6 \cdot 6}{5 \cdot 7} \cdot \frac{8 \cdot 8}{7 \cdot 9} \cdot \ldots
$$

It was published in 1656 by John Wallis.

## 2. Leibniz’s method

Leibniz discovered that π can be expressed through a simple alternating infinite series. It comes from evaluating the arctangent function at 1:

$$
\arctan(1) = \frac{\pi}{4} = 1 - \frac{1}{3} + \frac{1}{5} - \frac{1}{7} + \frac{1}{9} - \ldots
$$

Rearranging this gives:

$$
\pi = 4 \left( 1 - \frac{1}{3} + \frac{1}{5} - \frac{1}{7} + \cdots \right).
$$

The series alternates signs and uses the reciprocals of odd integers. Although extremely simple to implement, it converges very slowly: even millions of terms only give a few correct digits of π. Despite this, it is historically important and often used to demonstrate infinite series and numerical summation.
The sequence was discovered by James Gregory (1671), but Leibniz rediscovered it in 1673 and published it in the journal Acta Eruditorum in 1682.

## 3. (...)
