In Search of Light
===================

Yet another roguelike game written in pure Common Lisp.

Game is still under development and will be under it for a while (forever?)
because I'm not focused on making it complete. It's more like an experimental
project rather than an actual playable game.


Copying
-------

Copyright (C) Mark Fedurin, 2011-2014.

ISoL in this context is an abbreviation of "In Search of Light" which is the
full name of the game.  Both names hold the same meaning and can be used equally.

ISoL is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

ISoL is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
ISoL.  If not, see <http://www.gnu.org/licenses/>.


Running
--------

Load project with ASDF by executing standard
```lisp
(asdf:load-system :isol)
```
and then run MAIN in ISOL package.
```lisp
(isol:main)
```


Controls
---------

```
 y k u
h     l  — movement
 b j n
```

```
;  — pick object
```

```
i  — display inventory menu
```

```
q  — close any menu
```

```
q, ^C  — quit
```
