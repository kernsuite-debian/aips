      SUBROUTINE CLD (JD, IY, IM, ID, IWD)
C-----------------------------------------------------------------------
C! Converts Julian date to civil date
C# Math
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
C   Given is the Julian date corresponding to any instant of
C   absolute time.  Computed are the year, the month, the day in the
C   month, and the day within the week, on the the Greenwich
C   meridian.  NOTE: JD IS ROUNDED BACK TO THE NEAREST NOON.
C-----------------------------------------------------------------------
      DOUBLE PRECISION JD, JD0
      INTEGER   IY, IM, ID, IWD, MTDAY(13), IADD, LEAP,  I
      DOUBLE PRECISION    IDAYS, IREST, JDATE
      DATA MTDAY /0,31,59,90,120,151,181,212,243,273,304,334,365/
      DATA JD0 /1721425.5D0/
C-----------------------------------------------------------------------
      IDAYS = DINT (JD - JD0)
      JDATE = IDAYS
      IY = (IDAYS / 146097)
      IY = IY * 400
      IREST = MOD (IDAYS, 146097.D0)
      IADD = IREST / 36524.0D0
      IF (IADD.NE.4) GO TO 5
         IY = IY + 400
         IDAYS = 365.0D0
         GO TO 15
 5    CONTINUE
         IY = IY + IADD * 100
         IREST = MOD (IREST, 36524.0D0)
         IADD = IREST / 1461.0D0
         IY = IY + IADD * 4
         IREST = MOD (IREST, 1461.D0)
         IADD = IREST / 365.D0
         IF (IADD.NE.4) GO TO 10
            IY = IY + 4
            IDAYS = 365.D0
            GO TO 15
 10      CONTINUE
            IY = IY + IADD + 1
            IDAYS = MOD (IREST, 365.D0)
 15   LEAP = 0
      IF (MOD(IY,4).NE.0) GO TO 20
         LEAP = 1
         IF ((MOD(IY,100).EQ.0) .AND. (MOD(IY,400).NE.0)) LEAP = 0
         IDAYS = IDAYS - LEAP
 20   DO 30 I = 2,13
         IF (IDAYS.GE.MTDAY(I)) GO TO 30
            ID = IDAYS - MTDAY(I-1) + 1
            IM = I - 1
            IF ((LEAP.EQ.0) .OR. (I.GT.3)) GO TO 40
               ID = ID + 1
               IF (ID.NE.32) GO TO 40
                  ID = 1
                  IM = 2
                  GO TO 40
 30      CONTINUE
 40   IWD = MOD (JDATE, 7.D0) + 1
C
 999  RETURN
      END
