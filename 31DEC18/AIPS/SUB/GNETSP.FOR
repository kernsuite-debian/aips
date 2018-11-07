      SUBROUTINE GNETSP (IV1, IV2, NAMES)
C-----------------------------------------------------------------------
C! find disk assignement limitations
C# POPS-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2010-2012
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
C   GNETSP reads NETSP for disk assignment limitations and the TIMDEST
C   Limit
C   Inputs:
C      IV1     I       First disk nunmber to find
C      IV2     I       Last disk number to find
C      NAMES   C*(*)   Disk area names to seek
C   Output
C      IERR    I       Error code: 0 good
C-----------------------------------------------------------------------
      INTEGER   IV1, IV2
      CHARACTER NAMES(*)*(*)
C
      INTEGER   I, IV, J, JTRIM, TXLUN, TXIND, IERR, MSGSAV, DONE(35),
     *   NDONE
      CHARACTER LINE*256
      DOUBLE PRECISION DP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       default values
      I = IV2 - IV1 + 1
      IF (I.LE.0) GO TO 999
      CALL FILL (I, 0, DONE(IV1))
      NDONE = 0
      CALL RFILL (I, 14.0, TIMEDA(IV1))
      I = 8 * I
      CALL FILL (I, 0, DASSGN(1,IV1))
C                                       open NETSP
      TXLUN = 3
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL ZTXOPN ('QRED', TXLUN, TXIND, 'NET0:NETSP', .FALSE., IERR)
      IF (IERR.NE.0) GO TO 990
C                                       read loop
 10   CALL ZTXIO ('READ', TXLUN, TXIND, LINE, IERR)
      IF (IERR.EQ.0) THEN
         J = JTRIM (LINE)
         IF (J.LE.0) GO TO 10
         DO 30 IV = IV1,IV2
            J = JTRIM (NAMES(IV))
            IF (NAMES(IV)(:J).EQ.LINE(:J)) THEN
               IF (DONE(IV).EQ.0) NDONE = NDONE + 1
               DONE(IV) = DONE(IV) + 1
C                                       timdest value
               J = J + 1
               CALL GETNUM (LINE, 256, J, DP)
               IF (DP.NE.DBLANK) TIMEDA(IV) = DP
               DO 20 I = 1,8
                  CALL GETNUM (LINE, 256, J, DP)
                  IF (DP.NE.DBLANK) DASSGN(I,IV) = DP + 0.1D0
 20               CONTINUE
               IF (NDONE.GE.IV2-IV1+1) GO TO 900
               END IF
 30         CONTINUE
         GO TO 10
         END IF
C
 900  CALL ZTXCLS (TXLUN, TXIND, IERR)
 990  MSGSUP = MSGSAV
C
 999  RETURN
      END
