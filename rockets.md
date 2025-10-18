```{"variant":"standard","title":"Rocket motion equations (English translation)","id":"59214"}
The motion of the rocket is described by the **rocket equation** (Tsiolkovsky equation):

\[
v(t) = v_e \ln\frac{m_0}{m(t)} - g t
\]

where  

- \( v_e \) – exhaust gas velocity (m/s),  
- \( m_0 = m + f \) – initial mass (rocket + fuel),  
- \( m(t) = m_0 - d \cdot t \) – instantaneous mass (while fuel remains),  
- \( g \approx 9.81 \, \text{m/s}^2 \) – gravitational acceleration,  
- \( v(t) \) – rocket velocity (positive upward).

The **acceleration** \( a(t) \) is given by:

\[
a(t) = \frac{dv}{dt} = \frac{d \cdot v_e}{m(t)} - g
\]

The **altitude** \( h \) is calculated numerically, since there is no simple analytical solution:

\[
h(t + \Delta t) = h(t) + v(t) \cdot \Delta t
\]

Here \(\Delta t = 1\,\text{s}\), since the data are computed for each second.
```
