      SUBROUTINE PARANG (TIME, PANGLE)
C-----------------------------------------------------------------------
C! Computes antenna parallactic angles
C# Math Coordinates UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2008
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
C   Subroutine to compute the parallactic angles at a given time of the
C   antennas in the antenna common.  If the array center is that of the
C   VLA (i.e. VLA data) the parallactic angle is computed for the
C   center of the array.
C   Inputs:
C      TIME       R    Current data time (days).
C   Input from common (DSOU, DANS):
C      RAAPP      D    Apparent RA of source
C      DECAPP     D    Apparent Declination of source.
C      MNTYP(*)   I    Mount type 0=> atl-az.
C      STNLAT(*)  D    Antenna latitude (rad).
C      STNLON(*)  D    Antenna east longitudes (rad).
C      GSTIAT     D    GST at IAT=0 of reference day (rad).
C      ROTIAT     D    Rotation of the earth rate in IAT.
C   Output:
C      PANGLE(*)  R   Parallactic angles of the antennas.
C   RG Dodson 21/02/04 Added a call to SOUELV (source elevation) to
C   derive the required values for the Nasymth correction. Also added
C   Co-parallactic angle correcction for the Hobart telescope (i.e. EW
C   mount).
C   The parallactic angle is the difference between the feed angle and the
C   position angle.  For an Az-El mounted telescope, this is given by
C   from Pg 21 of de Deuge, 1985 (Honours thesis, Uni of Tas) :
C        PAR = arctan[(-sin(HA)cos(LAT))/
C                    (-cos(DEC)Sin(LAT) + cos(HA)cos(LAT)sin(DEC)]
C   For an XY mounted telescope, this is referred to as the
C   coparallactic angle, which is given by
C   from Pgs 13&22 of de Deuge, 1985 (Honours thesis, Uni of Tas) :
C        COPAR = PAR + arctan[cos(DEC)sin(HA)
C                      {(sin(DEC)sin(LAT)+cos(DEC)cos(HA)cos(LAT))/
C                      (sin(DEC)cos(LAT)-cos(DEC)cos(HA)sin(LAT))}]
C-----------------------------------------------------------------------
      REAL      TIME, PANGLE(*)
C
      REAL      HA, EL, AZ
      DOUBLE PRECISION DPARM
      INTEGER   LOOPAN
      LOGICAL   ISVLA, FIRST
      DOUBLE PRECISION HRANG, ANTLST, ARLONG, ARLAT, VLALON, VLALAT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE FIRST
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
C                                       Is this the VLA?
      ISVLA = (ABS (CNTRX+1.6011685D6).LE.100.0D0) .AND.
     *   (ABS (CNTRY+5.041977D6).LE.100.0D0) .AND.
     *   (ABS (CNTRZ-3.554875D6).LE.100.0D0)
C                                       All VLA antennas have the same
C                                       parallactic angle.
      IF (ISVLA) THEN
         VLALON = ATAN2 (CNTRY, CNTRX)
         VLALAT = ASIN (CNTRZ / SQRT (CNTRX*CNTRX + CNTRY*CNTRY +
     *      CNTRZ*CNTRZ))
      ELSE
         VLALON = 0.0D0
         VLALAT = 0.0D0
         END IF
      ARLONG = VLALON
      ARLAT = VLALAT
C                                       Loop over antennas
      DO 300 LOOPAN = 1,NSTNS
         PANGLE(LOOPAN) = 0.0
         IF (.NOT.ISVLA) THEN
            ARLONG = STNLON(LOOPAN)
            ARLAT = STNLAT(LOOPAN)
            END IF
C                                       Don't compute for equatorial
C                                       or space mounts
         IF ((MNTYP(LOOPAN).LT.1) .OR. (MNTYP(LOOPAN).GT.2)) THEN
C                                       Antenna LST
            ANTLST = GSTIAT + ARLONG + TIME * ROTIAT
C                                       Hour angle
            HRANG = ANTLST - RAAPP
C                                       Parallactic angle
            PANGLE(LOOPAN) = ATAN2 (COS (ARLAT) *
     *         SIN (HRANG), (SIN (ARLAT) * COS (DECAPP) -
     *         COS (ARLAT) * SIN (DECAPP) * COS(HRANG)))
C                                       Compute only for X-Y mount
C                                       (Hobart only?)
C                                       Co-parallacti
            IF (MNTYP(LOOPAN).EQ.3) THEN
               if (first) then
                  WRITE (MSGTXT,1000)
     *               'Using CO-PARALACTIC ANGLES for ', LOOPAN
                  CALL MSGWRT (2)
                  FIRST = .FALSE.
                  END IF
               PANGLE(LOOPAN) =  ATAN2 (-COS(HRANG),
     *            -SIN(HRANG) * SIN(DECAPP))
C                                       Nasmyth
            ELSE IF ((MNTYP(LOOPAN).EQ.4) .OR. (MNTYP(LOOPAN).EQ.5))
     *         THEN
               DPARM = TIME
               CALL SOUELV (LOOPAN, DPARM, HA, EL, AZ)
C                                       Right handed is FA=PA+EL
C                                       Left handed is FA=PA-EL
               IF (MNTYP(LOOPAN).EQ.5) EL = -EL
               PANGLE(LOOPAN) = PANGLE(LOOPAN) + EL
               IF (FIRST) THEN
                  WRITE (MSGTXT,1000) 'Using Nasmyth formula for ',
     *               LOOPAN
                  CALL MSGWRT (2)
                  FIRST = .FALSE.
                  END IF
               END IF
            END IF
 300     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A,'antenna',I4)
      END
