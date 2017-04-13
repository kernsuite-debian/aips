      SUBROUTINE CVLDOP (DISKIN, CNOIN, SUT, IANT, SOURID, OLDSOU,
     *   CVLSOU, DOPVEL, IERR)
C-----------------------------------------------------------------------
C! Determine the doppler velocity of any point on Earth's surface.
C# UV Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2007, 2010, 2012-2013
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
C   CVLDOP is a routine to determine the doppler velocity of a
C   particular source as seen from a particular point on the Earth's
C   surface at a particular time.  The routine does not recalculate the
C   parameters each time but interpolates over a reasonable time
C   interval updating when necessary.
C   Input:
C      DISKIN   I      Volume number
C      CNOIN    I      File catalogue number
C      SUT      R      UT of datum point (days)
C      IANT     I      Antenna number: <= 0 => Earth centric (VLBA...)
C      SOURID   I      Source number
C      IYEAR    I      Year of observation
C      IRDAY    I      Reference day of observation
C      OLDSOU   I      Previous source, if -1 no previous source
C   Input from common:
C      ANTX     D(*)   Antenna x coordinates (m)
C      ANTY     D(*)   Antenna y coordinates (m)
C      ANTZ     D(*)   Antenna z coordinates (m)
C      HELIO    L      True if in heliocentric reference frame
C   Output:
C      CVLSOU   I      Source being shifted
C      DOPVEL   R      Doppler velocity (km/s)
C      IERR     I      Error flag
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION    UT, DELTIM, VSUN, VT, DTIME, DOPVEL, DELVEL,
     *   TBEGIN(MAXANT+1), TSTOP(MAXANT+1), STRTVL(MAXANT+1),
     *   STOPVL(MAXANT+1), DXANT, DYANT, DZANT
      REAL      SUT
      INTEGER   DISKIN, CNOIN, IANT, SOURID, NEWDOP, IIYEAR, NDAY,
     *   MXANT, OLDSOU, ISLUN, CVLSOU, IERR, JANT, ID(3)
      LOGICAL   DOINT, T, F
      CHARACTER CTEMP*8
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCVL.INC'
      SAVE TBEGIN, TSTOP, STRTVL, STOPVL
      DATA T /.TRUE./, F /.FALSE./
      DATA MXANT /MAXANT/
      DATA ISLUN /27/
C-----------------------------------------------------------------------
C                                       Initialization
      DELTIM = 300.0D0 / 86400.0D0
      DOINT = T
C                                       defend ourselves
      IF (IYEAR.LE.0) THEN
         CALL H2CHR (8, 1, CATH(KHDOB), CTEMP)
         CALL DATEST (CTEMP, ID)
         IYEAR = ID(1)
         CALL DAYNUM (ID(1), ID(3), ID(2), IRDAY)
         END IF
      IIYEAR = IYEAR
C                                       VLBA uses geocentric ref.
      IF (IANT.LE.0) THEN
         DXANT = 0.0D0
         DYANT = 0.0D0
         DZANT = 0.0D0
         JANT = 1
      ELSE
         JANT = IANT + 1
         DXANT = ANTX(IANT)
         DYANT = ANTY(IANT)
         DZANT = ANTZ(IANT)
         END IF
C                                       Time in d.p
      UT = DBLE (SUT)
C                                       Is antenna number correct
      IF (IANT.GT.MAXANT) THEN
         WRITE (MSGTXT,1000) IANT
         IERR = 1
         GO TO 990
         END IF
C                                       Source change
      IF (SOURID.NE.OLDSOU) THEN
         CALL DFILL (MXANT, 999.D0, TBEGIN)
         CALL GETSOU (SOURID, DISKIN, CNOIN, CATBLK, ISLUN, IERR)
         IF (IERR .NE. 0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
         CVLSOU = SOURID
         DOINT = F
         END IF
      OLDSOU = SOURID
      OLDCSU = SOURID
C                                       Check to see if current record
C                                       falls within boundaries of last
C                                       interpolation interval.
      NEWDOP = 0
 100  IF (UT.LT.TBEGIN(JANT)) THEN
         TBEGIN(JANT) = UT - DELTIM / 5.0D0
         TSTOP(JANT)  = TBEGIN(JANT) + DELTIM
         NEWDOP = 2
         END IF
      IF (UT.GE.TSTOP(JANT)) THEN
         TBEGIN(JANT) = TBEGIN(JANT) + DELTIM
         TSTOP(JANT)  = TSTOP(JANT)  + DELTIM
         NEWDOP = NEWDOP + 1
         GO TO 100
         END IF
C                                       Setup both boundaries
C                                       Determine day number
      IF ((.NOT.DOINT) .OR. (NEWDOP.GT.1)) THEN
         NDAY = IRDAY + TBEGIN(JANT)
         DTIME = TBEGIN(JANT) - (REAL(NDAY-IRDAY))
         CALL DOPLR (RAAPP, DECAPP, IIYEAR, NDAY, DTIME, DXANT, DYANT,
     *      DZANT, VSUN, STRTVL(JANT))
C
         NDAY = IRDAY + TSTOP(JANT)
         DTIME = TSTOP(JANT) - (REAL(NDAY-IRDAY))
         CALL DOPLR (RAAPP, DECAPP, IIYEAR, NDAY, DTIME, DXANT, DYANT,
     *      DZANT, VSUN, STOPVL(JANT))
C
         DELVEL = STOPVL(JANT) - STRTVL(JANT)
C                                       Calculate new doppler vels.
C                                       at interpolation boundaries
      ELSE IF (NEWDOP.NE.0) THEN
         STRTVL(JANT) = STOPVL(JANT)
C                                       Determine day number
         NDAY = IRDAY + TSTOP(JANT)
         DTIME = TSTOP(JANT) - (REAL(NDAY-IRDAY))
         CALL DOPLR (RAAPP, DECAPP, IIYEAR, NDAY, DTIME, DXANT, DYANT,
     *      DZANT, VSUN, VT)
         STOPVL(JANT) = VT
         DELVEL = STOPVL(JANT) - STRTVL(JANT)
         END IF
C                                       velocity
      DOPVEL = (UT - TBEGIN(JANT)) * DELVEL / DELTIM  +  STRTVL(JANT)
      IF (HELIO) DOPVEL = DOPVEL - VSUN
      GO TO 999
C                                       Write error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CVLDOP: ANTENNA NUMBER ',I4,' OUTSIDE RANGE')
 1010 FORMAT ('CVLDOP: ERROR ',I3,' OBTAINING SOURCE INFO.')
      END
