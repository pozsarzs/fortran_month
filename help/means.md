# Statistical Measures Summary

This section describes the main statistical measures often used to analyze a dataset.

## 1. Arithmetic Mean
The **arithmetic mean** (average) is the sum of all values divided by the number of values:

$$
\bar{x} = \frac{1}{n}\sum_{i=1}^{n} x_i
$$

## 2. Geometric Mean
The **geometric mean** is the $n$th root of the product of all positive values:

$$
G = \sqrt[n]{\prod_{i=1}^{n} x_i}
$$

## 3. Harmonic Mean
The **harmonic mean** is the reciprocal of the average of the reciprocals:
$$
H = \frac{n}{\sum_{i=1}^{n} \frac{1}{x_i}}
$$

## 4. Quadratic Mean (Root Mean Square)
The **quadratic mean** or **RMS** measures the square root of the mean of squares:

$$
Q = \sqrt{\frac{1}{n}\sum_{i=1}^{n} x_i^2}
$$

## 5. Logarithmic Mean
The **logarithmic mean** between two positive numbers $a$ and $b$ ($a \ne b$) is:

$$
L = \frac{b - a}{\ln(b) - \ln(a)}
$$

## 6. Median
The **median** is the middle value when all numbers are sorted in order.  
If there is an even number of values, it is the average of the two middle values.

## 7. Modus (Mode)
The **mode** is the most frequently occurring value in the dataset.  
If all values occur only once, there is no mode.

## 8. Standard Deviation
The **standard deviation** measures the dispersion of the data around the mean:

$$
\sigma = \sqrt{\frac{1}{n}\sum_{i=1}^{n}(x_i - \bar{x})^2}
$$
