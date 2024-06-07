Data regression was started for all five grain size classes with the following settings:
1. <size of tracer in m, e.g. length of b-axis>
2. <name of file containing time series of shear stress, including file extension, see file '2-1-shear-stress-CEST.txt'>
3. <name of file containing time spans and transport lengths, see e.g. '2-2-32p1mm.txt'>
4. '1.5' (exponent 1.5 is used in formula)
5. 'decel' (check for deceleration)
6. 'a' (automatic mode)
7. '0' (0 is used as the start value of critical Shields stress in regression) 
8. 'b' (regression starts in inner loops with the value for critical Shields stress that resulted from the last loop)
9. '0.0000001' (start value for a*b is set to 0.0000001)
10. '0.1' (log10 search increment for a*b is set to 0.1)

For grain size 0.1404 m the regression was not successfull with the above settings, but by changing the following:
5. 'accel'