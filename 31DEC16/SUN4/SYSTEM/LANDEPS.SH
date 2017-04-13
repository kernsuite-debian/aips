#!/bin/sh
#-----------------------------------------------------------------------
#! Format Encapsulated PostScript (TM) for a 35mm Color Slide printer
## Shell-script
#-----------------------------------------------------------------------
#;  Copyright (C) 1995
#;  Associated Universities, Inc. Washington DC, USA.
#;
#;  This program is free software; you can redistribute it and/or
#;  modify it under the terms of the GNU General Public License as
#;  published by the Free Software Foundation; either version 2 of
#;  the License, or (at your option) any later version.
#;
#;  This program is distributed in the hope that it will be useful,
#;  but WITHOUT ANY WARRANTY; without even the implied warranty of
#;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#;  GNU General Public License for more details.
#;
#;  You should have received a copy of the GNU General Public
#;  License along with this program; if not, write to the Free
#;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
#;  MA 02139, USA.
#;
#;  Correspondence concerning AIPS should be addressed as follows:
#;         Internet email: aipsmail@nrao.edu.
#;         Postal address: AIPS Project Office
#;                         National Radio Astronomy Observatory
#;                         520 Edgemont Road
#;                         Charlottesville, VA 22903-2475 USA
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# This procedure assumes "xgrab" encapsulated PostScript (PS) output.
# The PS is rotated and fills a 35 mm slide image with a zoomed and
# centered rendition of a postscript file.  The slide is assumed to have
# an aspect ratio of 2 to 3 and that the maximum width is 800 units.
# The background of the slide is made black.
#
# A critical assumption is that the PS file contains a line with three
# elements, 1) x size, 2) y size and 3) the word "scale".  When this
# line is found, the PS is modified else the PS file is passed
# unchanged.
#
# Usage: as a filter, e.g. cat <file> | LANDEPS.SH | lpr -Pps35mm
#--------------------------------------------------------------------
#                                       Only modify the scale once
ONCE=0
#                                       For each non comment line:
while read line ; do
#                                       if have not found scaling
   if [ "$ONCE" = "0" ] ; then
#                                       General way of getting params
      set - $line
#                                       translate file or logical name
      THIRD=`eval echo $3`
#                                       if this is the scale line
      if [ "$THIRD" = "scale" ] ; then
         echo %% begin inserted rescaling
         echo %% $line
#                                       make background black
         echo /ymax 800 def          /xmax 533 def

         echo newpath 0 0 moveto
         echo 0 ymax rlineto xmax 0 rlineto
         echo 0 ymax -1 mul rlineto xmax -1 mul 0 rlineto
         echo closepath fill
#                                       determine landscape or port.
         FIRST=`eval echo $1`
         SECOND=`eval echo $2`
         echo /xsize $FIRST def       /ysize $SECOND def
         echo /xlim xmax .95 mul def  /ylim ymax .95 mul def
         if [ $FIRST -lt $SECOND ] ; then
#                                       higher than wide, use port.
            echo /xfact xsize xlim div def
            echo /yfact ysize ylim div def
#                                       chose max factor
            echo /xyfact xfact yfact gt {xfact} {yfact} ifelse def
            echo /xscal xsize xyfact div def
            echo /yscal ysize xyfact div def
            echo /xtran xmax xscal sub 2 div def
            echo /ytran ymax yscal sub 2 div def
            echo xtran ytran translate xscal yscal scale
         else
#                                       wider than high use land
            echo /xfact xsize ylim div def
            echo /yfact ysize xlim div def
#                                       chose max factor
            echo /xyfact xfact yfact gt {xfact} {yfact} ifelse def
            echo /xscal ysize xyfact div def
            echo /yscal xsize xyfact div def
            echo /xtran xmax xmax xscal sub 2 div sub def
            echo /ytran ymax yscal sub 2 div def
            echo xtran ytran translate xscal yscal scale 90 rotate
            fi
         echo %% end inserted rescaling
#                                       flag scaling is done
         ONCE=1
      else
#                                       else just print other lines
         echo $line
      fi
   else
      echo $line
   fi
done
