graVel: a code set for deriving the unsteady virtual grain velocity 
from bedload tracer studies 

Mario Klösch & Helmut Habersack

Institute of Hydraulic Engineering and River Research, Department of 
Water, Atmosphere and Environment, University of Natural Resources and 
Life Sciences, Vienna, Am Brigittenauer Sporn 3, 1200 Vienna, Austria

Countless laboratory experiments generated a vast body of knowledge on
the transport of bedload particles. The controlled conditions in the 
laboratories made it possible to link transport variables such as the 
virtual velocity of the bedload directly to flow variables. New 
technologies have greatly expanded the use of bedload tracers in the 
field as well, which hold great potential for expanding knowledge of 
field phenomena. However, due to the uncontrolled conditions in the 
field, it is difficult to link even to basic theories available from 
laboratory studies. 
To overcome this, Klösch and Habersack (2018) introduced "graVel", 
now named "graVel0" (or "graVel-zero") to differentiate from the more 
recent development, a code that performs a regression of bedload tracer
data to derive an unsteady virtual velocity of bedload particles. By 
directly linking the virtual velocity to the flow variable, this was an 
important step in bridging gaps between laboratory and field studies. 
However, the slowdown of the tracers due to effects from burial and 
advection has not yet been taken into account.
Together with a publication by Klösch et al. (2024a), we present 
"graVel±", a code that additionally accounts for tracer slowdown. The 
inclusion of slowdown enables the distinction between bedload velocity 
and decelerating tracer velocity and is a further step towards bridging
gaps to more fundamental knowledge gained in laboratories. 
At the moment, the graVel repository contains ‘graVel±’ and data from 
Klösch et al. (submitted) in the format required for use as input data 
for graVel±. This enables the reconstruction of the results of Klösch 
et al. (2024a). We plan to also provide ‘graVel0’ in this github 
repository, which was originally provided as supporting information to 
Klösch and Habersack (2018).  

References:

Klösch, M., & Habersack, H. (2018). Deriving formulas for an unsteady 
virtual velocity of bedload tracers. Earth Surface Processes and 
Landforms, 43, 1529–1541. https://doi.org/10.1002/esp.4326

Klösch, M., Pessenlehner, S., Gmeiner, Ph., & Habersack, H. (2024). 
Tracer velocity vs. bedload velocity: Derivation of the unsteady 
virtual bedload velocity from decelerating tracers. Water Resources 
Research. https://doi.org/10.1029/2023WR034823

Klösch, M., Pessenlehner, S., Gmeiner, Ph., & Habersack, H. (submitted). 
Data set from a bedload tracer field study in an alpine section of the
Drava River in 2017 and 2018 [Dataset]. PANGAEA.   

Citation of this github repository: 

Klösch, M., & Habersack, H. (2024). graVel: a code set for deriving the
unsteady virtual grain velocity from bedload tracer studies 
(Version 1.0) [Software]. 
