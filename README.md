# shenzhen-solitaire-solver

A Haskell program that is meant to act as a solver for the SHENZHEN I/O version of 
Solitaire. 

Goals include:
- identifying the Shenzhen I/O window, and focusing on it
- taking an in-memory screenshot of its current game state
- converting that screenshot to a representation of the game state
- a simple AI that can determine the most likely best possible move to take next
- taking the expected moves so as to solve the game of solitaire

While certainly a roundabout way to get SHENZHEN I/O's "BECOME IMMORTAL" achievement, hopefully
this remains within the spirit of the game, if not the letter. 😛

Required:
- xdotool and an X11 compatible windowing system for windowing control and mouse emulation
- a 1920x1080 resolution assumed (could be updated to work with more resolutions)
- GTK compatibility for the screenshots
- the Tesseract command line executable for OCR/text recognition (with adequate tessdatas)

