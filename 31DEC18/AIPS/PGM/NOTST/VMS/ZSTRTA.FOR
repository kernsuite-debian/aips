      PROGRAM ZSTRTA
C-----------------------------------------------------------------------
C! Changes process name to AIPSn
C# Z System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   ZSTRTA changes the process name to 'AIPSx'
C   VMS version
C-----------------------------------------------------------------------
      INTEGER*2 LIST2(8)
      INTEGER   IBUF(256), IAIPS, LAIPS, RETLN, IERR, LERR(15), I, J,
     *   LIST4(4), INUN1, INUN2
      CHARACTER THISTT*10, TRYTT*10, RESSTT*7, MYNAME*5, POPSNO*1,
     *   TEMP*15
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE '($SYSSRVNAM)'
      INCLUDE '($DVIDEF)'
      INCLUDE '($LNMDEF)'
      EQUIVALENCE (LIST2, LIST4)
C-----------------------------------------------------------------------
C                                       Set values especially NINTRN.
      CALL ZDCHIN (.TRUE., IBUF)
C
      MYNAME = 'AIPS  '
C                                       Figure out who we are.
C                                       Extra translations if needed
      LIST2(1) = 10
      LIST2(2) = DVI$_TT_PHYDEVNAM
      LIST4(2) = %LOC(THISTT)
      LIST4(3) = %LOC(RETLN)
      LIST4(4) = 0
      IERR = SYS$GETDVI( , , 'TT', LIST4, , , , )
C                                       Find 1st non '_' in name.
      INUN1 = 1
      DO 10 I = 1,10
         IF (THISTT(INUN1:INUN1).NE.'_') GO TO 15
         INUN1 = INUN1 + 1
 10      CONTINUE
C                                       See if we are in list of
C                                       reserved terminals.
 15   LAIPS = 0
      LIST2(1) = 10
      LIST2(2) = LNM$_STRING
      LIST4(2) = %LOC(TRYTT)
      LIST4(3) = 0
      LIST4(4) = 0
      DO 30 IAIPS = 1,NINTRN
         CALL ZEHEX (IAIPS, 1, POPSNO)
         RESSTT = 'RESSTT' // POPSNO
         LERR(IAIPS) = SYS$TRNLNM ( , 'AIPS_USER', RESSTT, , LIST4)
         IF (LERR(IAIPS).EQ.1) THEN
C                                       Strip leading underscores.
            INUN2 = 1
            DO 20 I = 1,10
               IF (TRYTT(INUN2:INUN2).NE.'_') GO TO 25
               INUN2 = INUN2 + 1
 20            CONTINUE
C
 25         IF (THISTT(INUN1:INUN1+3).EQ.TRYTT(INUN2:INUN2+3)) THEN
               LAIPS = IAIPS
               GO TO 50
               END IF
            END IF
 30      CONTINUE
C                                       Change process name to 'AIPSx'
C                                       Not a reserved terminal
      DO 40 IAIPS = 1,NINTRN
         IF (LERR(IAIPS).NE.1) THEN
            CALL ZEHEX (IAIPS, 1, POPSNO)
            MYNAME = 'AIPS' // POPSNO
            IERR = SYS$SETPRN(MYNAME)
            IF (IERR.EQ.1) THEN
               LAIPS = IAIPS
               GO TO 60
               END IF
            END IF
 40      CONTINUE
      LAIPS = 0
      GO TO 60
C                                       reserved terminal
 50   CONTINUE
         CALL ZEHEX (LAIPS, 1, POPSNO)
         MYNAME(5:5) = POPSNO
         IERR = SYS$SETPRN(MYNAME)
         IF (IERR.NE.1) LAIPS = 0
C
 60   IF (LAIPS.LE.0) THEN
         WRITE (6,1060)
      ELSE
         WRITE (6,1065) POPSNO
         END IF
C
 999  CONTINUE
C-----------------------------------------------------------------------
 1060 FORMAT (' ******  ALL AIPS ARE IN USE.  ******')
 1065 FORMAT (' Begin the one true AIPS number ',A1)
      END
