      SUBROUTINE AXSTRN (AXTYP, AXVAL, AXPTR, NCH, STR)
C-----------------------------------------------------------------------
C! encodes axis type and value in a string
C# Coordinates Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2003, 2014
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
C   AXSTRN returns a 20-character, left-justified string encoding the
C   axis type and value appropriately.
C   Inputs:
C      AXTYP   C*(*)    Axis type -- 1st 8 real chars used
C      AXVAL   D        Value on that axis
C      AXPTR   I        0-relative pointer of which axis N.B.
C   Output:
C      NCH     I        Number of characters in STR (can be rather
C                       less than 20 - 16 preferred for pos.)
C      STR     C*20     Axis descriptor string
C   Common: /MAPHDR/ CATBLK
C-----------------------------------------------------------------------
      CHARACTER AXTYP*(*), STR*(*)
      DOUBLE PRECISION  AXVAL, X
      INTEGER   AXPTR, NCH
C
      INTEGER   CATBLK(256), NA, NTY, IFRM, NB, IT, HM(2), NC
      REAL      Y, CATR(256)
      CHARACTER XSTR*20, WST*20, SXTYP(15)*4, CHSTOK(20)*4, CHM*1
      DOUBLE PRECISION SEC
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR)
      DATA NTY, SXTYP /15, 'LL  ','RA  ','RA--','GLON','ELON',
     *   'MM  ','DEC ','DEC-','GLAT','ELAT','STOK','FREQ','VELO',
     *   'FELO','TIME'/
      DATA CHSTOK /'BEAM','IPOL','QPOL','UPOL','VPOL','PPOL','FPOL',
     *   'PANG','SPIX','OPTD','ROTM','????','RR','LL','RL','LR',
     *   'XX','YY','XY','YX'/
C-----------------------------------------------------------------------
      STR = ' '
C                                       is it a special type?
      DO 10 IT = 1,NTY
         IF (AXTYP(1:4).EQ.SXTYP(IT)) GO TO 100
 10      CONTINUE
C                                       No: pack AXTYP
      XSTR = AXTYP
      CALL CHTRIM (XSTR, 8, XSTR, NA)
      NA = MIN (NA + 1, 11)
      IF (NA.GT.1) THEN
         XSTR(NA:) = ' '
         NA = NA + 1
         END IF
C                                       Select a format
 15   X = ABS (AXVAL)
      IFRM = 1
      IF (X.LT.1.0D0) IFRM = 2
      IF (X.LT.1.D-3) IFRM = 3
      IF (X.LT.1.D-5) IFRM = 5
      IF (X.GT.1.D3) IFRM = 4
      IF (X.GT.1.D5) IFRM = 5
 20   IF (IFRM.EQ.1) WRITE (WST,1020,ERR=880) AXVAL
      IF (IFRM.EQ.2) WRITE (WST,1021,ERR=880) AXVAL
      IF (IFRM.EQ.3) WRITE (WST,1022,ERR=880) AXVAL
      IF (IFRM.EQ.4) WRITE (WST,1023,ERR=880) AXVAL
      IF (IFRM.NE.5) GO TO 900
         WRITE (WST,1024,ERR=880) AXVAL
         IF (NA.GT.9) NA = 9
         XSTR(NA-1:) = ' '
         GO TO 900
C                                       Special types: branch
 100  NA = IT - 10
      IF (NA.GT.0) GO TO (130, 140, 150, 150, 160), NA
C                                       An angle axis: fix prefix
         IF ((IT.EQ.1) .OR. (IT.EQ.3)) IT = 2
         IF ((IT.EQ.6) .OR. (IT.EQ.8)) IT = 7
         NA = 5
         IF (IT.EQ.2) NA = 3
         IF (IT.EQ.7) NA = 4
         XSTR = SXTYP(IT)
         XSTR(NA:NA) = ' '
         NA = NA + 1
C                                       Check field of view
         X = AXVAL
         Y = ABS (CATR(KRCIC+AXPTR)) * 3600.0
         IF (Y.LE.0.0) Y = 121.
         IF (MOD(IT-1,5).GT.2) Y = Y * 20.
C                                       Do in degrees
         IF (Y.LE.1200.) GO TO 110
            IF (IT.EQ.2) X = X / 15.0D0
            IF (IT.NE.2) WRITE (WST,1020,ERR=880) X
            IF (IT.EQ.2) WRITE (WST,1025,ERR=880) X
            GO TO 900
C                                       Do in sexagesimal
 110     CONTINUE
            IF (IT.EQ.2) CALL COORDI (1, X, CHM, HM, SEC)
            IF (IT.NE.2) CALL COORDI (2, X, CHM, HM, SEC)
            Y = ABS(CATR(KRCIC+AXPTR)) * 3600.0
            IF (Y.GT.1.0) THEN
               IF (IT.EQ.2) THEN
                  WRITE (WST,1110,ERR=880) CHM, HM, SEC
               ELSE
                  WRITE (WST,1111,ERR=880) CHM, HM, SEC
                  END IF
            ELSE IF (Y.GT.0.01) THEN
               IF (IT.EQ.2) THEN
                  WRITE (WST,1115,ERR=880) CHM, HM, SEC
               ELSE
                  WRITE (WST,1116,ERR=880) CHM, HM, SEC
                  END IF
            ELSE IF (Y.GT.0.0001) THEN
               IF (IT.EQ.2) THEN
                  WRITE (WST,1120,ERR=880) CHM, HM, SEC
               ELSE
                  WRITE (WST,1121,ERR=880) CHM, HM, SEC
                  END IF
            ELSE
               IF (IT.EQ.2) THEN
                  WRITE (WST,1125,ERR=880) CHM, HM, SEC
               ELSE
                  WRITE (WST,1126,ERR=880) CHM, HM, SEC
                  END IF
               END IF
            IF (WST(8:8).EQ.' ') WST(8:8) = '0'
            IF (WST(9:9).EQ.' ') WST(9:9) = '0'
            GO TO 900
C                                       Stokes
 130  CONTINUE
         NC = AXVAL + 1.5
         IF ((NC.LE.0) .OR. (NC.GT.11)) NC = 12
         IF ((AXVAL.LT.-0.5) .AND. (AXVAL.GT.-8.5)) NC = -AXVAL + 12.5
         STR = CHSTOK(NC)
         NCH = 4 - 2 * (NC / 13)
         GO TO 999
C                                       Frequency
 140  CONTINUE
         NA = 1
         IF (ABS(AXVAL).LT.1.E4) THEN
            WRITE (WST,1140,ERR=880) AXVAL
         ELSE IF (ABS(AXVAL).LT.1.E7) THEN
            X = AXVAL / 1.E3
            WRITE (WST,1141,ERR=880) X
         ELSE IF(ABS(AXVAL).LE.1.E11) THEN
            X = AXVAL / 1.E6
            WRITE (WST,1145,ERR=880) X
         ELSE
            WRITE (WST,1147,ERR=880) AXVAL
            END IF
         GO TO 900
C                                       Velocity
 150  CONTINUE
         NA = 1
         IF (ABS(AXVAL).LT.1.E4) THEN
            WRITE (WST,1150,ERR=880) AXVAL
         ELSE
            X = AXVAL / 1.E3
            WRITE (WST,1155,ERR=880) X
            END IF
         GO TO 900
C                                       Time
 160  CONTINUE
         NA = 1
         IF (ABS(AXVAL).GT.8.64D4) GO TO 165
            WRITE (WST,1160,ERR=880) AXVAL
            GO TO 900
 165     CONTINUE
            X = AXVAL / 8.64D4
            WRITE (WST,1165,ERR=880) X
            GO TO 900
C                                       Error Handeling
 880  CONTINUE
C                                       One more Try
      WRITE(WST,1200,ERR=890) AXVAL
 890  CONTINUE
C                                       Final string packing
 900  CALL CHTRIM (WST, 20, WST, NB)
      IF (NB.GT.21-NA) NB = 21 - NA
      XSTR(NA:NA+NB-1) = WST(1:NB)
      NCH = NA + NB - 1
      STR = XSTR(1:NCH)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (F8.3)
 1021 FORMAT (F8.5)
 1022 FORMAT (F8.6)
 1023 FORMAT (F8.0)
 1024 FORMAT (1PE11.4)
 1025 FORMAT (F8.5)
 1110 FORMAT (A1,I2.2,I3.2,F7.3)
 1111 FORMAT (A1,I2.2,I3.2,F6.2)
 1115 FORMAT (A1,I2.2,I3.2,F9.5)
 1116 FORMAT (A1,I2.2,I3.2,F8.4)
 1120 FORMAT (A1,I2.2,I3.2,F11.7)
 1121 FORMAT (A1,I2.2,I3.2,F10.6)
 1125 FORMAT (A1,I2.2,I3.2,F13.9)
 1126 FORMAT (A1,I2.2,I3.2,F12.8)
 1140 FORMAT (F10.1,' Hz')
 1141 FORMAT (F12.3,' kHz')
 1145 FORMAT (F12.3,' MHz')
 1147 FORMAT (1PE11.4,' Hz')
 1150 FORMAT (F9.1,' m/s')
 1155 FORMAT (F12.1,' km/s')
 1160 FORMAT (F9.1,' sec')
 1165 FORMAT (F9.3,' days')
 1200 FORMAT (1PE11.4,'   ')
      END
