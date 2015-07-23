# pongctrl

Simple Pong implementation for testing autonomous pong controllers.

## Installation

Clone this github repository and run
	
	$ lein uberjar

to create the target jar file.

## Usage

The controller tester expects as its only argument a controller
executable.
The controller should read from stdin a single line containing the
dimensions of the image, i.e. width and height separated by a space
character.
After this line, the controller tester outputs the image of the Pong
board in ASCII format, so that black pixels are represented by a "0"
and white pixels by a "1".
The image is output as `image height` lines, each containing `image
width` characters.
After printing the image, the tester expects to read a line consisting
of a single number that corresponds to the new `y` coordinate of the
controller bat.

To test a controller, run

    $ java -jar pongctrl-0.1.0-standalone.jar <controller-executable>


Alternatively, run with `leiningen` using

	$ lein run -- ./controller.py

or
	$ lein run -- "<path to python executable> controller.py"

## Configuration

There are several configuration options in
`resources/pongctrl.properties`.
Feel free to experiment with those.

## Examples

A very simple controller implementation written in python is included.

### Bugs

Several, working on them.

## License

Copyright © 2014 Nikolaos Vlassopoulos

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
