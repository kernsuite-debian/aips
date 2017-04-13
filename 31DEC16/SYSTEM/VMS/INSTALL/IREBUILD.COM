$! IREBUILD.COM
$!----------------------------------------------------------------------
$!;  Copyright (C) 1995
$!;  Associated Universities, Inc. Washington DC, USA.
$!;
$!;  This program is free software; you can redistribute it and/or
$!;  modify it under the terms of the GNU General Public License as
$!;  published by the Free Software Foundation; either version 2 of
$!;  the License, or (at your option) any later version.
$!;
$!;  This program is distributed in the hope that it will be useful,
$!;  but WITHOUT ANY WARRANTY; without even the implied warranty of
$!;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
$!;  GNU General Public License for more details.
$!;
$!;  You should have received a copy of the GNU General Public
$!;  License along with this program; if not, write to the Free
$!;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
$!;  MA 02139, USA.
$!;
$!;  Correspondence concerning AIPS should be addressed as follows:
$!;         Internet email: aipsmail@nrao.edu.
$!;         Postal address: AIPS Project Office
$!;                         National Radio Astronomy Observatory
$!;                         520 Edgemont Road
$!;                         Charlottesville, VA 22903-2475 USA
$!----------------------------------------------------------------------
$!---------------------------------------------------------------------
$! Recompile and link all of AIPS.
$!---------------------------------------------------------------------
$                              ! Recompile all subroutines.
$                              ! APL
$ Set NoOn
$ COMRPL APLSUB:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL APLNOT:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL APLNVMS:*.FOR   DIRTY NOMAP NOLIST
$! COMRPL APLNVMS:*.MAR   DIRTY NOMAP NOLIST (complot, grisub, zetasubs)
$ COMRPL APLGEN:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL APLVMS:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL APLVMS:*.MAR   DIRTY NOMAP NOLIST
$                              ! Q
$ COMRPL QSUB:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL QNOT:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL QPSAP:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL QVMS:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL QVMS:*.MAR   DIRTY NOMAP NOLIST
$ COMRPL QFPS:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL QFPS16:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL Q120B:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL Q5000:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL QFPS32:*.FOR   DIRTY NOMAP NOLIST
$! COMRPL Q190:*.FOR   DIRTY NOMAP NOLIST
$                              ! Y
$ COMRPL YSUB:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL YNOT:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL YGEN:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL YSTUB:*.FOR  DIRTY NOMAP NOLIST
$ COMRPL YIIS:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL YM70:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL YM75:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL YDEA:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL YV20:*.FOR   DIRTY NOMAP NOLIST
$ COMRPL YIVAS:*.FOR  DIRTY NOMAP NOLIST
$ COMRPL YARGS:*.FOR  DIRTY NOMAP NOLIST
$                              ! AIPS pops language.
$ COMRPL AIPSUB:*.FOR   DIRTY NOMAP NOLIST
$                              ! Relink all tasks and programs.
$ COMLNK APLPGM:*.FOR   DIRTY NOMAP NOLIST
$ COMLNK APGNOT:*.FOR   DIRTY NOMAP NOLIST
$ COMLNK APGVMS:*.FOR   DIRTY NOMAP NOLIST
$ COMLNK AIPPGM:*.FOR   DIRTY NOMAP NOLIST
$ COMLNK AIPGVMS:*.FOR  DIRTY NOMAP NOLIST
$ COMLNK AIPNOT:*.FOR	DIRTY NOMAP NOLIST
$!
$ COMLNK QPGM:*.FOR     DIRTY NOMAP NOLIST
$ COMLNK QPGNOT:*.FOR   DIRTY NOMAP NOLIST
$!
$ COMLNK QYPGM:*.FOR    DIRTY NOMAP NOLIST
$ COMLNK QYPGNOT:*.FOR  DIRTY NOMAP NOLIST
$!
$ COMLNK YPGM:*.FOR     DIRTY NOMAP NOLIST
$ COMLNK YPGNOT:*.FOR   DIRTY NOMAP NOLIST
$! COMLNK YPGVMS:*.FOR   DIRTY NOMAP NOLIST nothing in there anymore
$!
$ EXIT
