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
if test -f SUBLIB
then
   echo "Index of SUBLIB" `ar t SUBLIB | wc -l`
fi
echo "Index of" `pwd`/'*.o' `ls *.o | wc -l`
echo -n 'continue? (y/n):'
read ANS
if test "$ANS" != "y"
then
   exit
else
   rm -f SUBLIB
   echo "Creating new SUBLIB"
   ar cr SUBLIB `lorder *.o | tsort`
   echo "Index of SUBLIB" `ar t SUBLIB | wc -l`
   echo "Randomize SUBLIB" `ranlib SUBLIB`
   echo "Index of SUBLIB" `ar t SUBLIB | wc -l`
   echo -n 'rm *.o ? (y/n):'
   read ANS
   if test "$ANS" != "y"
   then
      exit
   else
      rm *.o &
   fi
fi
