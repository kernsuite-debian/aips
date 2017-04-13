$! ICREOPT.COM
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
$!----------------------------------------------------------------------
$! This program will create all of the option files for aips.
$!----------------------------------------------------------------------
$                                     ! IIS Model 70.
$ if (TVTYPE.eq.1) then TVLib == "YM70"
$                                     ! IIS Model 75.
$ if (TVTYPE.eq.2) then TVLib == "YM75"
$                                     ! DeAnza.
$ if (TVTYPE.eq.3) then TVLib == "YDEA"
$                                     ! Comtal version 20.
$ if (TVTYPE.eq.4) then TVLib == "YV20"
$                                     ! IIS IVAS
$ if (TVTYPE.eq.5) then TVLib == "YIVAS"
$                                     ! ARGS
$ if (TVTYPE.eq.6) then TVLib == "YARGS"
$                                     ! No TV.
$ if (TVTYPE.eq.0) then TVLib == "YSTUB"
$                                     !
$ APLIB2 = "LIBR:APLSUBLIB/LIB"
$ if (APTYPE.eq.1) then APLib1 == "Q120B"
$ if (APTYPE.eq.2) then APLib1 == "Q5000"
$ if (APTYPE.eq.1) .or. (APTYPE.eq.2) then -
     APLib2 = "LIBR:APLSUBLIB/LIB,FPS:HSRLIB/LIB,FPS:FPSLIB/LIB"
$ if (APTYPE.eq.3) then APLib1 == "QVMS"
$                                     ! AIPPGMOPT
$ open /write AIPPGMOPT AIPS_STARTUP:aippgmopt.opt
$ write AIPPGMOPT "LIBR:AIPSUBLIB/LIB,-"
$ write AIPPGMOPT "LIBR:YSUBLIB/LIB,LIBR:",TVLib,"LIB/LIB, -"
$ if (TVTYPE.ne.5) then goto noivs1
$ write AIPPGMOPT "LIBR:IVAS/LIB,LIBR:XANTH/LIB,-"
$ write AIPPGMOPT "SYS$LIBRARY:VAXCRTL/LIB,-"
$ noivs1:
$ write AIPPGMOPT "LIBR:APLSUBLIB/LIB,LIBR:APLVMSLIB/LIB,LIBR:APLSUBLIB/LIB,-"
$ write AIPPGMOPT "LIBR:APLSUBLIB/LIB,LIBR:APLVMSLIB/LIB,LIBR:APLSUBLIB/LIB"
$ close AIPPGMOPT
$                                     ! QPGNOTOPT
$ open /write QPGNOTOPT AIPS_STARTUP:qpgnotopt.opt
$ write QPGNOTOPT "LIBR:QNOTLIB/LIB,LIBR:APLNOTLIB/LIB,-"
$ write QPGNOTOPT "LIBR:QSUBLIB/LIB,-"
$ write QPGNOTOPT "LIBR:",APLib1,"LIB/LIB,-"
$ write QPGNOTOPT "LIBR:APLSUBLIB/LIB,LIBR:APLVMSLIB/LIB,-"
$ write QPGNOTOPT APLib2
$ close QPGNOTOPT
$                                     ! QYPGMOPT
$ open /write QYPGMOPT AIPS_STARTUP:qypgmopt.opt
$ write QYPGMOPT "LIBR:QSUBLIB/LIB,-"
$ write QYPGMOPT "LIBR:",APLib1,"LIB/LIB,-"
$ write QYPGMOPT "LIBR:YSUBLIB/LIB,LIBR:",TVLib,"LIB/LIB,-"
$ if (TVTYPE.ne.5) then goto noivs2
$ write QYPGMOPT "LIBR:IVAS/LIB,LIBR:XANTH/LIB,-"
$ write QYPGMOPT "SYS$LIBRARY:VAXCRTL/LIB,-"
$ noivs2:
$ write QYPGMOPT "LIBR:APLSUBLIB/LIB,LIBR:APLVMSLIB/LIB,LIBR:APLSUBLIB/LIB,-"
$ write QYPGMOPT APLib2
$ close QYPGMOPT
$                                     ! QYPGNOTOPT
$ open /write QYPGNOTOPT AIPS_STARTUP:qypgnotopt.opt
$ write QYPGNOTOPT "LIBR:QNOTLIB/LIB,LIBR:APLNOTLIB/LIB,-"
$ write QYPGNOTOPT "LIBR:QNOTLIB/LIB,LIBR:QSUBLIB/LIB,-"
$ write QYPGNOTOPT "LIBR:",APLib1,"LIB/LIB,-"
$ write QYPGNOTOPT "LIBR:YSUBLIB/LIB,LIBR:",TVLib,"LIB/LIB,-"
$ if (TVTYPE.ne.5) then goto noivs3
$ write QYPGNOTOPT "LIBR:IVAS/LIB,LIBR:XANTH/LIB,-"
$ write QYPGNOTOPT "SYS$LIBRARY:VAXCRTL/LIB,-"
$ noivs3:
$ write QYPGNOTOPT "LIBR:APLSUBLIB/LIB,LIBR:APLVMSLIB/LIB,LIBR:APLSUBLIB/LIB,-"
$ write QYPGNOTOPT APLib2
$ close QYPGNOTOPT
$                                     ! YPGMOPT
$ open /write YPGMOPT AIPS_STARTUP:ypgmopt.opt
$ write YPGMOPT "LIBR:YSUBLIB/LIB,LIBR:",TVLib,"LIB/LIB,-"
$ if (TVTYPE.ne.5) then goto noivs4
$ write YPGMOPT "LIBR:IVAS/LIB,LIBR:XANTH/LIB,-"
$ write YPGMOPT "SYS$LIBRARY:VAXCRTL/LIB,-"
$ noivs4:
$ write YPGMOPT "LIBR:APLSUBLIB/LIB,LIBR:APLVMSLIB/LIB,LIBR:APLSUBLIB/LIB"
$ close YPGMOPT
$                                     ! YPGNOTOPT
$ open /write YPGNOTOPT AIPS_STARTUP:ypgnotopt.opt
$ write YPGNOTOPT "LIBR:YNOTLIB/LIB,LIBR:APLNOTLIB/LIB,-"
$ write YPGNOTOPT "LIBR:YSUBLIB/LIB,LIBR:",TVLib,"LIB/LIB,-"
$ if (TVTYPE.ne.5) then goto noivs5
$ write YPGNOTOPT "LIBR:IVAS/LIB,LIBR:XANTH/LIB,-"
$ write YPGNOTOPT "SYS$LIBRARY:VAXCRTL/LIB,-"
$ noivs5:
$ write YPGNOTOPT "LIBR:APLSUBLIB/LIB,LIBR:APLVMSLIB/LIB,LIBR:APLSUBLIB/LIB"
$ close YPGNOTOPT
$                                     ! YPGVMSOPT
$ open /write YPGVMSOPT AIPS_STARTUP:ypgvmsopt.opt
$ write YPGVMSOPT "LIBR:YNOTLIB/LIB,LIBR:APLNOTLIB/LIB,-"
$ write YPGVMSOPT "LIBR:YSUBLIB/LIB,LIBR:",TVLib,"LIB/LIB,-"
$ if (TVTYPE.ne.5) then goto noivs6
$ write YPGVMSOPT "LIBR:IVAS/LIB,LIBR:XANTH/LIB,-"
$ write YPGVMSOPT "SYS$LIBRARY:VAXCRTL/LIB,-"
$ noivs6:
$ write YPGVMSOPT "LIBR:APLSUBLIB/LIB,LIBR:APLVMSLIB/LIB,LIBR:APLSUBLIB/LIB"
$ close YPGVMSOPT
$!
$ if (APTYPE.eq.3) then GOTO PSAP
$                                     ! QYPGMOPT1
$   open /write QYPGMOPT1 AIPS_STARTUP:qypgmopt1.opt
$   write QYPGMOPT1 "LIBR:QSUBLIB/LIB,-"
$   write QYPGMOPT1 "LIBR:QVMSLIB/LIB,-"
$   write QYPGMOPT1 "LIBR:YSUBLIB/LIB,LIBR:",TVLib,"LIB/LIB,-"
$ if (TVTYPE.ne.5) then goto noivs7
$ write QYPGMOPT1 "LIBR:IVAS/LIB,LIBR:XANTH/LIB,-"
$ write QYPGMOPT1 "SYS$LIBRARY:VAXCRTL/LIB,-"
$ noivs7:
$   write QYPGMOPT1 "LIBR:APLSUBLIB/LIB,LIBR:APLVMSLIB/LIB,LIBR:APLSUBLIB/LIB"
$   close QYPGMOPT1
$                                     ! QYPGNOTOPT1
$   open /write QYPGNOTOPT1 AIPS_STARTUP:qypgnotopt1.opt
$   write QYPGNOTOPT1 "LIBR:QNOTLIB/LIB,LIBR:APLNOTLIB/LIB,-"
$   write QYPGNOTOPT1 "LIBR:QNOTLIB/LIB,LIBR:QSUBLIB/LIB,-"
$   write QYPGNOTOPT1 "LIBR:QVMSLIB/LIB,-"
$   write QYPGNOTOPT1 "LIBR:YSUBLIB/LIB,LIBR:",TVLib,"LIB/LIB,-"
$ if (TVTYPE.ne.5) then goto noivs8
$ write QYPGNOTOPT1 "LIBR:IVAS/LIB,LIBR:XANTH/LIB,-"
$ write QYPGNOTOPT1 "SYS$LIBRARY:VAXCRTL/LIB,-"
$ noivs8:
$   write QYPGNOTOPT1 "LIBR:APLSUBLIB/LIB,LIBR:APLVMSLIB/LIB,LIBR:APLSUBLIB/LIB"
$   close QYPGNOTOPT1
$   GOTO INSTALLVAR
$ PSAP:
$                                     ! Psap versions are redundant.
$    write sys$output "Ignore file not found message if it occurs"
$    delete AIPS_STARTUP:*opt1.opt;*
$ INSTALLVAR:
$!
$ purge/nolog AIPS_STARTUP:*.opt
$ write sys$output "Creating file INSTALLVAR.COM"
$ TheDir = f$logical("SYS$DISK") + f$directory()
$ TopDir = f$parse(TheDir,,,"device","syntax_only,no_conceal")-
       + f$parse(TheDir,,,"directory","syntax_only,no_conceal")-
       - "][" - "]" + "."
$ open /write InstallVar INSTALLVAR.COM
$ write InstallVar "$ APTYPE == ", APTYPE
$ if (APTYPE.ne.3) then write InstallVar -
    "$ define FPS ",APDIR
$ write InstallVar "$ APLIB1 :== ",APLIB1
$ write InstallVar "$ TVTYPE == ",TVTYPE
$ write InstallVar "$ TVLIB :== ",TVLIB
$ write InstallVar "$ TAPEDENS :== ",TAPEDENS
$ write InstallVar "$ define /nolog AIPS_VERSION  ",THISVER
$ write InstallVar -
   "$ ! Point everything to VERDATE for installation purposes"
$ write InstallVar -
   "$ define /nolog/translate=concealed TST  ", TopDir, VERDATE,".]"
$ write InstallVar -
   "$ define /nolog/translate=concealed NEW  ", TopDir, VERDATE,".]"
$ close InstallVar
$!
$ EXIT
$!
$ EXIT
