$! OPTIONS - deal with options for compile and link procedures
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
$!
$! Inputs:
$!      Pn - list of options
$! Outputs:
$!      ZZFORTRAN - name of symbol to receive FORTRAN command
$!      ZZMACRO - name of symbol to receive MACRO command
$!      ZZLINKEXE - name of symbol to receive LINK command (executable)
$!      ZZLINKSHR - name of symbol to receive LINK command (shareable)
$!      ZZPURGE - logical switch for PURGE step
$!
$! Error handler
$!
$       ON ERROR THEN EXIT %X0000002C
$       ON CONTROL_Y THEN EXIT %X0000002C
$!
$! Start with default options
$! Add any additional ones specified in symbols
$!
$ PPMODE == "0"
$ IF F$TRNLNM("AIPS_VERSION").EQS."TST" THEN PPMODE == "1"
$ IF F$TYPE(FORTRAN).EQS."" THEN FORTRAN = "FORTRAN"
$    ZZFORTRAN == "FORTRAN/WARNINGS=(DECLARATIONS)" -
                + "/NODEBUG/OPTIMIZE" -
                + "/STANDARD=(SYNTAX,SOURCE_FORM)" -
                + F$EXTRACT(F$LOCATE("/",FORTRAN), 100, FORTRAN)
$ IF PPMODE.EQS."1" THEN -
     ZZFORTRAN == "FORTRAN/WARNINGS=(DECLARATIONS)" -
                + "/DEBUG/NOOPTIMIZE" -
                + "/STANDARD=(SYNTAX,SOURCE_FORM)" -
                + F$EXTRACT(F$LOCATE("/",FORTRAN), 100, FORTRAN)
$ IF F$TYPE(MACRO).EQS."" THEN MACRO = "MACRO"
$    ZZMACRO == "MACRO/DEBUG" -
                + F$EXTRACT(F$LOCATE("/",MACRO), 100, MACRO)
$ IF F$TYPE(LINK).EQS."" THEN LINK = "LINK"
$    ZZLINKEXE == "LINK" -
                + F$EXTRACT(F$LOCATE("/",LINK), 100, LINK)
$    ZZLINKSHR == "LINK/DEBUG" -
                + F$EXTRACT(F$LOCATE("/",LINK), 100, LINK)
$    ZZPURGE == "YES"
$!
$! Allowed options
$!
$       DE*BUG := GOTO DEBUG
$       NODE*BUG := GOTO NODEBUG
$       LI*ST := GOTO LIST
$       NOLI*ST := GOTO NOLIST
$       MA*P := GOTO MAP
$       NOMA*P := GOTO NOMAP
$       OP*TIMIZE := GOTO OPTIMIZE
$       NOOP*TIMIZE := GOTO NOOPTIMIZE
$       DI*RTY := GOTO DIRTY
$       NODI*RTY := GOTO NODIRTY
$       AN*SI := GOTO ANSI
$       NOAN*SI := GOTO NOANSI
$       PU*RGE := GOTO PURGE
$       NOPU*RGE := GOTO NOPURGE
$!
$! Set each option
$!
$       I = 0
$LOOP:
$       I = I + 1
$       IF I.GT.8 THEN EXIT
$       OPTION = F$EDIT(P'I',"UPCASE")
$       IF OPTION.EQS."" THEN EXIT
$!
$! Process option
$!
$       SET NOON
$       'OPTION'
$       IF .NOT.$SEVERITY THEN GOTO ABORT
$       SET ON
$       GOTO LOOP
$!
$! Individual options
$!
$DEBUG:
$       ZZFORTRAN == ZZFORTRAN + "/DEBUG/NOOPT"
$       ZZMACRO == ZZMACRO + "/DEBUG"
$       ZZLINKEXE == ZZLINKEXE + "/DEBUG"
$       ZZLINKSHR == ZZLINKSHR + "/DEBUG"
$       GOTO LOOP
$NODEBUG:
$       PPMODE == "0"
$       ZZFORTRAN == ZZFORTRAN + "/NODEBUG/OPT"
$       ZZMACRO == ZZMACRO + "/NODEBUG"
$       ZZLINKEXE == ZZLINKEXE + "/NODEBUG"
$       ZZLINKSHR == ZZLINKSHR + "/NODEBUG"
$       GOTO LOOP
$LIST:
$       ZZFORTRAN == ZZFORTRAN + "/LIST/SHOW=(INCLUDE)/CROSS"
$       ZZMACRO == ZZMACRO + "/LIST"
$       GOTO LOOP
$NOLIST:
$       ZZFORTRAN == ZZFORTRAN + "/NOLIST"
$       ZZMACRO == ZZMACRO + "/NOLIST"
$       GOTO LOOP
$MAP:
$       ZZLINKEXE == ZZLINKEXE + "/MAP"
$       ZZLINKSHR == ZZLINKSHR + "/MAP"
$       GOTO LOOP
$NOMAP:
$       ZZLINKEXE == ZZLINKEXE + "/NOMAP"
$       ZZLINKSHR == ZZLINKSHR + "/NOMAP"
$       GOTO LOOP
$OPTIMIZE:
$       ZZFORTRAN == ZZFORTRAN + "/OPTIMIZE/NODEBUG"
$       GOTO LOOP
$NOOPTIMIZE:
$       ZZFORTRAN == ZZFORTRAN + "/DEBUG/NOOPTIMIZE"
$       GOTO LOOP
$DIRTY:
$       ZZFORTRAN == ZZFORTRAN + "/NOWARNINGS/NOSTANDARD"
$       GOTO LOOP
$ANSI:
$NODIRTY:
$       ZZFORTRAN == ZZFORTRAN + "/WARNINGS=(DECLARATIONS)" -
                      + "/STANDARD=(SYNTAX,SOURCE_FORM)"
$       GOTO LOOP
$PURGE:
$       ZZPURGE == "YES"
$       GOTO LOOP
$NOPURGE:
$       ZZPURGE == "NO"
$       GOTO LOOP
$NOANSI:
$       ZZFORTRAN == ZZFORTRAN + "/WARNINGS=(DECLARATIONS)" -
                      + "/STANDARD=(NOSYNTAX,SOURCE_FORM)"
$       GOTO LOOP
$!
$! Error
$!
$ABORT:
$       WRITE SYS$OUTPUT "Invalid option ",OPTION
$       EXIT %X0000002C
