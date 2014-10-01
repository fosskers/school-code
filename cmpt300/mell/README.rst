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

Mell's Moods
------------
- Mell doesn't like it when you don't treat her nice.

  -  :D => Great => Mell thinks you're great!
  -  :) => Happy => Mell likes you. She'll tell you what directory you're in.
  -  :| => Unimpressed => Well, now Mell doesn't feel like telling you your cwd.
  -  :( => Mad => Might lies to you about the time.
  - >:( => Livid => Mell won't listen to your commands sometimes.

Libraries
---------
Because strings are a nightmare in C, we employ the `bstring` library
to handle some things for us.
