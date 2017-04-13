      SUBROUTINE TICSTR (ITRY, DEGC, PT5SEC, AXISTP, COOTYP, CHDL, HML,
     *   TICLAB, ILEN)
C-----------------------------------------------------------------------
C! Prepares string for tick units label
C# Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 2003, 2012
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
C   TICSTR prepares strings for labeling tick marks
C   Inputs:
C      ITRY     I      TICINC array location => format
C      DEGC     D      Tick value
C      PT5SEC   D      Round off
C      AXISTP   I      Axis type: 0 linear, else celestial
C      COOTYP   I      1 RA, 2 Dec
C   In/out:
C      CHDL     C*4    Previous coordinate leading values
C      HML      I(2)   HH/MM or DD/MM of previous call
C   Output:
C      TICLAB   C*18   Character string to display
C      ILEN     I      Length to display
C-----------------------------------------------------------------------
      DOUBLE PRECISION DEGC, PT5SEC
      INTEGER   ITRY, AXISTP, COOTYP, HML(2), ILEN
      CHARACTER CHDL*(*), TICLAB*(*)
C
      DOUBLE PRECISION RAD
      CHARACTER CHD*4, CHM*3, SPRTXT*18
      INTEGER   FRMT, IFRMT, HM(2), IS, INP
      REAL      SEC, RT
C-----------------------------------------------------------------------
C                                       Convert degrees to DEC/RA.
      IF (AXISTP.NE.0) THEN
         RAD = DEGC + SIGN (PT5SEC, DEGC)
         IF (((RAD.LT.0.0D0) .AND. (COOTYP.EQ.1)) .OR.
     *      (RAD.LT.-90.0D0)) RAD = RAD + 2 * PT5SEC
         CALL CORDDS (COOTYP, RAD, CHM, HM, SEC)
C                                       Write DEC/RA.
         IF ((CHM.EQ.'  -') .OR. (CHM.EQ.' ') .OR. (COOTYP.EQ.2)) THEN
            CHD = ' ' // CHM
         ELSE
            CHD = CHM // '/'
            IF (AXISTP.EQ.5) CHD = ' '
            END IF
 10      IF (ITRY.GE.19) THEN
            IS = SEC
            WRITE (SPRTXT,1000) CHD, HM, IS
         ELSE IF (ITRY.GE.16) THEN
            WRITE (SPRTXT,1001) CHD, HM, SEC
         ELSE IF (ITRY.GE.13) THEN
            WRITE (SPRTXT,1002) CHD, HM, SEC
         ELSE IF (ITRY.GE.10) THEN
            WRITE (SPRTXT,1003) CHD, HM, SEC
         ELSE IF (ITRY.GE.7) THEN
            WRITE (SPRTXT,1004) CHD, HM, SEC
         ELSE IF (ITRY.GE.4) THEN
            WRITE (SPRTXT,1005) CHD, HM, SEC
         ELSE
            WRITE (SPRTXT,1006) CHD, HM, SEC
            END IF
C                                       test for trouble...
         IF (SPRTXT(11:12).EQ.'60') THEN
            SEC = SEC - 60.0
            HM(2) = HM(2) + 1
            IF (HM(2).EQ.60) THEN
               HM(2) = HM(2) - 60
               HM(1) = HM(1) + 1
               END IF
            GO TO 10
            END IF
         INP = 10
         IF (HM(2).NE.HML(2)) INP = 7
         IF (HM(1).NE.HML(1)) INP = 0
         IF (CHD.NE.CHDL) INP = 0
         CHDL = CHD
         HML(1) = HM(1)
         HML(2) = HM(2)
C                                       fill with zeros to make nice
         IF (SPRTXT(11:11).EQ.' ') SPRTXT(11:11) = '0'
         IF (SPRTXT(12:12).EQ.' ') SPRTXT(12:12) = '0'
         IF (INP.GT.0) SPRTXT(1:INP) = ' '
         IF (ITRY.GT.23) THEN
            INP = 10
            IF (ITRY.GT.28) INP = 7
            SPRTXT(INP:) = ' '
            END IF
C                                       non RA-DEC
      ELSE
         RT = DEGC
         IF (ITRY.GE.19) THEN
            IFRMT = FRMT (18, 5, RT, SPRTXT)
         ELSE IF (ITRY.GE.16) THEN
            WRITE (SPRTXT,1031) DEGC
         ELSE IF (ITRY.GE.13) THEN
            WRITE (SPRTXT,1032) DEGC
         ELSE IF (ITRY.GE.10) THEN
            WRITE (SPRTXT,1033) DEGC
         ELSE IF (ITRY.GE.7) THEN
            WRITE (SPRTXT,1034) DEGC
         ELSE IF (ITRY.GE.4) THEN
            WRITE (SPRTXT,1035) DEGC
         ELSE
            WRITE (SPRTXT,1036) DEGC
            END IF
         END IF
C                                       Trim blanks
      CALL CHTRIM (SPRTXT, 18, SPRTXT, ILEN)
      TICLAB = SPRTXT
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A4,I2.2,2(1X,I2.2))
 1001 FORMAT (A4,I2.2,1X,I2.2,1X,F4.1)
 1002 FORMAT (A4,I2.2,1X,I2.2,1X,F5.2)
 1003 FORMAT (A4,I2.2,1X,I2.2,1X,F6.3)
 1004 FORMAT (A4,I2.2,1X,I2.2,1X,F7.4)
 1005 FORMAT (A4,I2.2,1X,I2.2,1X,F8.5)
 1006 FORMAT (A4,I2.2,1X,I2.2,1X,F9.6)
 1031 FORMAT (F18.1)
 1032 FORMAT (F18.2)
 1033 FORMAT (F18.3)
 1034 FORMAT (F18.4)
 1035 FORMAT (F18.5)
 1036 FORMAT (F18.6)
      END
