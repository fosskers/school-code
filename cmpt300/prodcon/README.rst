The Foundry
===========
The Foundry is a Producer-Consumer Dining Philosopher's Simulator.
Three Generators produce Metals, which Operators collect and meld into Alloys.

On Metals
---------
For reference:

* Tin + Copper  = Bronze
* Tin + Lead    = Solder
* Copper + Lead = Molybdochalkos

Compilation
-----------
Just run `make` in the top directory. The Foundry uses nCurses, which is
installed on the CSIL Linux computers, so it might be a good idea to compile
it there. No guarantees on compilation success can be made of other systems.

Simulation Setup
----------------
Press any key to advance from the Welcome screen.

You will be asked how many Tools and Operators you want in the simulation.
The minimum number of Tools is 2, and of Operators is 1. You will be kicked
out of The Foundry if you give invalid values.

The Simulation
--------------
All relevant data is shown around the edges of the screen.

Generators (on left) are highlighted green when they are actively producing.

Tools (on top) are highlighted yellow when they are "in use".

Operators (on right) are highlighted red when they successfully meld an Alloy.

*Tools Yielded* refers to the number of times an Operator had one tool, went
for another, realized there were none, and gave his up to avoid deadlock.

*Gave Up* refers to the number of times an Operator went to meld a certain Alloy,
realized the output queue constraints wouldn't allow it, and gave up.

Key Bindings
------------
* Press up to speed up the simulation.
* Press down to slow down the simulation.
* Press p to pause.
* Press q to quit the simulation.


