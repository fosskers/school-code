Mell - The Moody Shell
======================

Mell is a simple shell that treats you differently depending on her mood.

Features
--------
- **Mood Indicator**: Mell will tell you how she's feeling. Performing
  commands successfully will make her happy. The opposite will reduce her
  opinion of you.
- **Exit Status**: The exit status of the previous command will affect the
  colouring of the time.

  - Green  => Success!
  - Red    => Failure...
  - Yellow => Some other error. Bad flags?

Compilation
-----------
Just run `make`. It is compiled with debugging information.

Usage
-----
- `exit`: Quit the shell.
- `cd`: Move to another directory. `cd ..` moves to the parent directory.

Libraries
---------
Because strings are a nightmare in C, we employ the `bstring` library
to handle some things for us.
