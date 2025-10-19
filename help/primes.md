#Mathematical Description of Prime Number Search

The program determines all prime numbers less than or equal to a given integer **N**.  

It starts from the first known primes, **2** and **3**, and examines each subsequent odd number **P ≥ 5**.  
For each **P**, the algorithm tests whether there exists an odd divisor **Q** (with **3 ≤ Q ≤ √P**) such that:

$$
P \bmod Q = 0
$$

If such a **Q** exists, **P** is composite and is skipped.  
If no divisor is found for any **Q ≤ √P**, then **P** is prime and is output.  

Formally, for each odd integer **P ≤ N**, the program checks:

$$
\forall Q \in \{3,5,7,\dots,\lfloor \sqrt{P} \rfloor\}: P \bmod Q \neq 0 \implies P \text{ is prime.}
$$

This method is a straightforward trial division algorithm optimized to test only odd divisors.
