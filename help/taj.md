# The Hungarian TAJ number

This is a 9-digit personal social security identifier used in Hungary. It uniquely identifies a person in the national health insurance system.

## Mathematical validation

A TAJ number can be checked using a **checksum rule** applied to the first 8 digits:

1. Multiply the **odd-position digits** (1st, 3rd, 5th, 7th) by **3**.
2. Multiply the **even-position digits** (2nd, 4th, 6th, 8th) by **7**.
3. Add all these products together.
4. Take the result **modulo 10** (i.e., the last digit of the sum).
5. This digit must match the **9th digit** of the TAJ number (the checksum).

If it matches, the TAJ number is mathematically valid.
