      SUBROUTINE CHSTAT (STAT, ISTAT, IERR)
C-----------------------------------------------------------------------
C! changes numeric code used to record the status of the catalog entry
C# Catalog
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
C   CHSTAT changes status word ISTAT according to requested operation
C   Input:
C     STAT  C*4  operation code ->  'WRIT' = mark slot as write enabled
C                                   'READ' = mark slot as read  enabled
C                                   'CLWR' = clear write status
C                                   'CLRD' = clear 1 read status
C                                   'REST' = nop
C   Input old value, Output changed value:
C     ISTAT I    Status work in cat dir: 0 => no activity
C                N (>0) => N tasks are reading file
C                N (<0) => N-1 tasks are reading, one is writing
C   Output:
C     IERR  I    error return; 0 = ok
C                 1 = WARNING!, READ requested while WRIT is busy
C                 2 = STAT inconsistent e.g. CLWR when no write
C                     is in progress
C                 3 = Can't issue WRIT, READ is busy
C                 4 = illegal status code
C-----------------------------------------------------------------------
      CHARACTER STAT*4
      INTEGER   ISTAT, IERR
C
      INTEGER   IST, NST, STWRT, STNOW
      CHARACTER STCODE(5)*4
      DATA NST, STCODE /5, 'WRIT', 'READ', 'CLWR', 'CLRD', 'REST'/
      DATA STWRT /-1/
C-----------------------------------------------------------------------
C                                       find operation code
      IERR = 0
      DO 10 IST = 1,NST
         IF (STAT.EQ.STCODE(IST)) GO TO 15
 10      CONTINUE
      IERR = 4
      GO TO 999
C                                       got valid op code
 15   STNOW = ISTAT
      GO TO (20, 30, 40, 50, 999), IST
C                                       set to WRITE
 20   IF (STNOW.EQ.0) GO TO 25
         IERR = 3
         GO TO 999
 25   CONTINUE
         ISTAT = STWRT
         GO TO 999
C                                       add a READ
 30   IF (STNOW.LT.0) GO TO 35
         ISTAT = STNOW + 1
         GO TO 999
 35   CONTINUE
         ISTAT = STNOW -1
         IERR = 1
         GO TO 999
C                                       clear WRITE
 40   IF (STNOW.GE.0) GO TO 45
         ISTAT = -(1 + STNOW)
         GO TO 999
 45   CONTINUE
         IERR = 2
         GO TO 999
C                                       clear READ
 50   IF ((STNOW.EQ.0) .OR. (STNOW.EQ.STWRT)) GO TO 55
         IF (STNOW.LT.STWRT) ISTAT = STNOW + 1
         IF (STNOW.GT.0) ISTAT = STNOW - 1
         GO TO 999
 55   CONTINUE
         IERR = 2
         GO TO 999
C
 999  RETURN
      END
