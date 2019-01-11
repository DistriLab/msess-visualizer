# handle-transmit-overflow
If too many transmissions happen, the GUI will run out of vertical space to 
display the transmits.

Since we already have:
- A history of previous transmits as a list, `bTransmits`, and
- A scroll position that indexes the list, `bScrollPos`,
then we can view a small part of `bTransmits`, and move that view when 
`bScrollPos` changes.

## Overview
- Calculate number of transmits that the screen can show, 
  `numTransmitsOnScreenMax`.
- If the length of `bTransmits` exceeds `numTransmitsOnScreenMax`, then define 
  `transmitsOnScreen` as only the last `numTransmitsOnScreenMax` elements of 
  `bTransmits`.
- Remap the y-axis values of `transmitsOnScreen` such that the first 
  transmission is displayed at the top of the GUI. This ensures that all 
  transmits inside `transmitsOnScreen` are shown in the GUI.
