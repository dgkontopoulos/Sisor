## NAME

<b>S</b>isor: <b>I</b>ntelligent <b>S</b>pace <b>Or</b>ganization

## DESCRIPTION

A Common Lisp graphical application for efficient spaces organization and inventory sorting.

<b>Sisor</b> was initially developed for the <a href="http://lispinsummerprojects.org/" style="text-decoration:none">2013 Lisp in Summer Projects programming contest</a>.

The application's <a href="http://en.wikipedia.org/wiki/Recursive_acronym#Computer-related_examples" style="text-decoration:none">recursive acronym</a> and logo have been inspired from <b><i><a href="http://en.wikipedia.org/wiki/Sisor" style="text-decoration:none">Sisor rabdophorus</a></i></b>.

## DEMO/TUTORIAL

<center><iframe width="640" height="480" src="http://www.youtube.com/embed/uIqMjxHD-6Q?rel=0&vq=hd720" frameborder="0" allowfullscreen></iframe></center>

## INSTALLATION

###For GNU/Linux and BSD systems:
Download the application, unzip it and run the <b>install.sh</b> script with superuser privileges from Sisor's directory, e.g.:
<code>sudo bash ./install.sh</code>

Respectively, to uninstall Sisor:
<code>sudo bash ./uninstall.sh</code>

Make sure that ImageMagick and SQLite3 are already installed.

## DEPENDENCIES

\-<a href="http://www.sbcl.org/" style="text-decoration:none">SBCL</a>, >= 1.0.58

\-<a href="http://weitz.de/cl-fad/" style="text-decoration:none">cl-fad</a>, >= 0.7.2 (Common Lisp library)

\-<a href="http://www.cliki.net/cl-gtk2" style="text-decoration:none">cl-gtk2</a>, >= 0.1.1 (Common Lisp library)

\-<a href="http://weitz.de/cl-ppcre/" style="text-decoration:none">cl-ppcre</a>, >= 2.0.4 (Common Lisp library)

\-<a href="http://common-lisp.net/project/cl-sqlite/" style="text-decoration:none">cl-sqlite</a>, >= 0.2 (Common Lisp library)

\-<a href="http://www.imagemagick.org/" style="text-decoration:none">ImageMagick</a>, >= 6.7.7-10

\-libgtk2.0-0

\-<a href="https://www.sqlite.org/" style="text-decoration:none">SQLite3</a>, 3.7.17

## KNOWN BUGS

<b>i) <a href="https://github.com/dgkontopoulos/Sisor/issues/1" style="text-decoration:none">Launching the application on GNU/Linux from a menu (such as Unity's dash) causes a terminal to appear along with it.</a></b> The terminal shows the standard error of the application and is mostly an unneeded nuisance. Setting "<i>Terminal=false</i>" to the .desktop file prevents the application from launching from the menu.

## TO-DOs

<b>i)</b> Allow an item to move from one space's inventory to another's.

<b>ii)</b> Implement a method to search all the spaces for specific items.

<b>iii)</b> Create a successful working PPA for easier installation of Sisor on Ubuntu.

<b>iv)</b> Internationalization.

## AUTHOR

Dimitrios - Georgios Kontopoulos <<dgkontopoulos@member.fsf.org>>

## LICENSE

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
<a href="https://www.gnu.org/copyleft/gpl.html" style="text-decoration:none">GNU General Public License</a> for more details.

