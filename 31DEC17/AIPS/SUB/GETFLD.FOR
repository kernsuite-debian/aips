      SUBROUTINE GETFLD
C-----------------------------------------------------------------------
C! finds the next symbol in KARBUF and determines its pointers
C# POPS-lang
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   GETFLD finds the next non-blank character in KARBUF and determines
C   whether the token begun with that character is symbolic (1st char
C   is A - Z), numeric (1st char is 0 - 9 or .), or hollerith (1st
C   char is ').  After the field length is found, appropriate calls
C   are made to the symbol processing routine, number scanning
C   routine, etc.  Communication back to POLISH is via SYTYPE and TAG
C   parameters determined by the processors SYMBOL, GETNUM, LTSTOR...
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, M*1, M1*2, STRNG*80
      INTEGER  POTERR, J
      DOUBLE PRECISION DBLX
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DSMS.INC'
      DATA PRGNAM /'GETFLD'/
C-----------------------------------------------------------------------
C                                       init pointers
 5    TAG = 0
      NKAR = 0
      SYTYPE = 0
      X(1) = 0
C                                        skip leading blanks
 10   IF (KBPTR.GT.KARLIM) GO TO 999
         M = KARBUF(KBPTR:KBPTR)
         IF (M.NE.' ') GO TO 20
         KBPTR = KBPTR + 1
         GO TO 10
C                                        classify type by first char.
20    IF ((M.LE.'Z') .AND. (M.GE.'A')) GO TO 30
      IF ((M.LE.'z') .AND. (M.GE.'a')) GO TO 30
      IF (M.EQ.'.') GO TO 50
      IF (M.EQ.'''') GO TO 70
      IF ((M.LE.'9') .AND. (M.GE.'0')) GO TO 50
C                                       In-line comment
      IF (M.NE.'$') GO TO 60
         KBPTR = KARLIM + 1
         GO TO 999
C-----------------------------------------------------------------------
C      S Y M B O L I C    F I E L D
C-----------------------------------------------------------------------
C                                        find end
 30   J = KBPTR
 35   NKAR = NKAR + 1
         J = J + 1
         POTERR = 4
         IF (J.GT.KARLIM) GO TO 980
         M = KARBUF(J:J)
         IF ((M.LE.'Z') .AND. (M.GE.'A')) GO TO 35
         IF ((M.LE.'z') .AND. (M.GE.'a')) GO TO 35
         IF (M.EQ.'_') GO TO 35
         IF ((M.LE.'9') .AND. (M.GE.'0')) GO TO 35
      POTERR = 5
      IF (NKAR.GT.10) GO TO 980
C                                        locate in symbol table
 40   CALL CHLTOU (NKAR, KARBUF(KBPTR:))
      CALL CHR2H (NKAR, KARBUF(KBPTR:), 1, KPAK)
      KBPTR = KBPTR + NKAR
      CALL SYMBOL (1)
      IF (ERRNUM.NE.0) GO TO 980
      GO TO 999
C-----------------------------------------------------------------------
C        N U M E R I C    F I E L D
C-----------------------------------------------------------------------
C                                        find value
 50   J = KBPTR
      CALL GETNUM (KARBUF, KARLIM, KBPTR, DBLX)
      IF (ERRNUM.NE.0) GO TO 980
      X(1) = DBLX
      NKAR = KBPTR - J
C                                        store value
      LX = 1
      SYTYPE = 11
      CALL LTSTOR
      IF (ERRNUM.EQ.1) ERRNUM = 74
      IF (ERRNUM.NE.0) GO TO 980
      GO TO 999
C-----------------------------------------------------------------------
C        ' O T H E R ' (SUCH THINGS AS + - * / ** ETC.)
C-----------------------------------------------------------------------
C                                        find end
 60   J = KBPTR
 65   NKAR = NKAR + 1
         J = J + 1
         POTERR = 2
         IF (NKAR.GT.2) GO TO 980
         M1 = KARBUF(J-1:J)
C                                        watch for combined symbols:
C                                        **, <=, <>, !!, and >=
         IF (M1.EQ.'**') GO TO 65
         IF (M1.EQ.'!!') GO TO 65
         IF (M1.EQ.'<>') GO TO 65
         IF (M1.EQ.'<=') GO TO 65
         IF (M1.EQ.'>=') GO TO 65
      GO TO 40
C-----------------------------------------------------------------------
C        H O L L E R I T H    F I E L D S
C-----------------------------------------------------------------------
 70   SYTYPE = 14
      CALL GETSTR (KARBUF, KARLIM, 80, KBPTR, STRNG, NKAR)
      IF (ERRNUM.NE.0) GO TO 980
      CALL CHR2H (NKAR, STRNG, 1, X)
C                                       force to integer # reals
      LX = (NKAR + 3) / 4
      CALL LTSTOR
      IF (ERRNUM.EQ.1) ERRNUM = 75
      IF (ERRNUM.NE.0) GO TO 980
      K(LOCSYM+1) = SYTYPE + 16*NKAR
      GO TO 999
C-----------------------------------------------------------------------
C                                               Error return.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
