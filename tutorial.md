# This is the theoretical background of the app.

Welcome to the PK simulation app! The aim of this tutorial is to supplement you with more theoretical explanation to explain how this app works.

## Computation of the model for the coated reservoir sustained release formulation

The dosage form that is modeled in this case is a coated reservoir system. All diffusion problems must be approached from the Fick's second law, initially:

$$
\frac{\delta C}{\delta t} = D\frac{\delta^2 C}{\delta x^2} \quad (1)
$$

This is a second order partial differential equation, and quite hard to solve. Fortunately, we can make the assumption that $\frac {\delta C}{\delta t} ≈ 0$. This means that the right hand side of Fick's law is also equal to zero, which means that $\frac {\delta C}{\delta x} = k$ and thus that $C(x) = kx+b$. Essentially, the whole situation reduces to the image below, which can be easily modeled through Fick's first law of diffusion:

$$
\frac{dM_{GI}}{A dt} = -D\frac{C_{{GI},\ film}-C_{formulation,\ film}}{\Delta x} \quad (2)
$$

To remove the "film" subscript, we need to include a term that takes into account the partition of the API into the film and the aqueous medium. This can be done through the equilibrium constant $K = \frac{Concentration\ in\ film}{Concentration\ outside\ of\ film}$. Moreover, to solve the differential equation, $dM_{GI}$ is transformed into $C_{GI}$ by using the volume in the GI $V_{GI}$. Moreover, A is moved to the other side, $C_{formulation}$ is the equilibrium concentration $C_{sat}$ and $\Delta x$ is just the thickness of the film $(h)$. Based on this, we have:

$$
\frac{dC_{GI}}{dt} = -\frac{DKA}{hV_{GI}}(C_{GI}-C_{sat}) \quad (3)
$$

This will be the contribution from the dosage form to the total GI tract concentration. A very similar expression can be used to determine the drug disappearing from the GI tract due to absorption:

$$
\frac{dC_{GI}}{dt} = \frac{P_{eff}A}{V_{GI}}(C_{plasma}- C_{GI}) \quad (4)
$$

Now, in order to avoid a system of differential equations (which would result in tremendously complicated math), let's assume that $C_{plasma} << C_{GI}$, and hence the $C_{plasma}$ in the parentheses above can be neglected. Based on this, we get a relatively simple expression for $\frac{dC_{GI}}{dt}$:

$$
\frac{dC_{GI}}{dt} = \frac{DKA}{hV_{GI}}(C_{sat}-C_{GI}) - \frac{P_{eff}A}{V_{GI}}(C_{GI}) \quad (5).
$$

This differential equation is solvable through separation of variables. Renaming $\frac{DKA}{hV_{GI}} k_s$ and $\frac{P_{eff}A}{V_{GI}} k_a$, we can write:

$$
\frac{dC_{GI}}{k_sC_{sat}-k_sC_{GI}-k_aC_{GI}} = dt \quad (6).
$$

This can be solved by performing a u-substitution and setting the whole denominator equal to u. Solving the resulting equation with boundary conditions $C_{GI} = 0 \ if \  t = 0$ and $C_{GI} = C(t)_{GI} \ if \  t = t$ gives:

$$
C_{GI}(t) = \frac{k_SC_{sat}-k_sC_{sat}e^{-(k_a+k_s)t}}{k_s+k_a} \quad (7).
$$

The problem with the approach so far is that it does not consider the dosage form emptying. The release will keep increasing as long as the concentration in the GI tract is below the saturation concentration. Furthermore, the dosage form might exit the GI tract before release is complete. The first issue is solved by determining the time at which the dosage form is empty based on the equations above. The second issue is solved by comparing the time required to empty the dosage form with a certain GI transit time (that can be picked by you). As soon as either time is reached (whichever is smaller), the release from the formulation is set to zero, and thus:

$$
\frac{dC_{GI}}{dt} = - \frac{P_{eff}A}{V_{GI}}(C_{GI}) \quad (8),
$$

which is just first order elimination. The boundary conditions for this differential are a little more complex, since at $t = 0$, the concentration is now $C_{GI}(transit \  time)\  or\  C_{GI}(empty \  dosage \  form \  time)$ The calculation of the time required to empty the dosage form is a little more complex. Essentially, equation 7 is plugged into equation 2 and the resulting equation is solved using similar methods as before. One obtains the following expression for the mass of drug left, where D is the initial dose in the formulation, and $k'_s = k_sV_{GI}$:

$$
M(t) = D - t k_s' C_{sat} + t \frac{k_s' k_s C_{sat}}{k_a + k_s} + \frac{k_s' k_s C_{sat} e^{-(k_a+k_s)t}}{(k_a+k_s)^2} + \frac{k_s' k_s C_{sat}}{(k_a+k_s)^2} \quad (9)
$$

The time required to empty the dosage form can then be obtained by setting $M(t) = 0$ and determining $t$. This can not be done easily analytically, and is thus solved numerically.

Finally, the plasma concentration is obtained by the equation:

$$
\frac{dC_{plasma}}{dt} = \frac{P_{eff}A}{V_d} C_{GI}-\frac{Cl}{Vd}C_{plasma} \quad (10)
$$

where Cl is the clearance. Again, the assumption is made here that the plasma concentration is much smaller than the concentration in the GI tract. If $C_{GI}$ in equation 10 is replaced by the expression from equation 7, the differential equation can again be solved using separation of variables and u-substitution. The resulting final equation is:

$$
C_{plasma}(t) = \frac{P_{eff} A} {Cl} \frac{k_SC_{sat}-k_sC_{sat}e^{-(k_a+k_s)t}}{k_s+k_a}-\frac{P_{eff} A} {Cl} \frac{k_SC_{sat}-k_sC_{sat}e^{-(k_a+k_s)t}}{k_s+k_a}e^{-\frac{Cl}{V_d}t} \quad (11).
$$
