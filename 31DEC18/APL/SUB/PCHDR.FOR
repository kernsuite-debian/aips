      SUBROUTINE PCHDR (PCMATX, CDMATX, PVMATX)
C-----------------------------------------------------------------------
C! converts PCi_j/CDi_j into aips coordinate values
C# FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 2003-2004, 2012-2013
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
C   PCHDR interprets PCi_j and CDi_j cards values into header
C   coordinates.  Note: the matrices allow a very wide range of
C   coordinate descriptions that AIPS does not support.
C   Inputs:
C      PCMATX   R(7,7)   The PC matrix values
C      CDMATX   R(7,7)   The CD matrix values
C      PVMATX   R(7,7)   The CD matrix values
C   In/out DCAT.INC common
C   PVMATX currently only used to detect -NCP on -SIN obliquities
C-----------------------------------------------------------------------
      REAL      PCMATX(7,7), CDMATX(7,7), PVMATX(7,7)
C
      INTEGER   I, J, I1, I2
      LOGICAL   ISPC, ISCD, ISPV
      REAL      CDELT, RHOA, RHOB, RHOD, R1, R2, D1, D2
      CHARACTER CTEMP*8, CLONG*8, CLAT*8
      DOUBLE PRECISION DEC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      ISPC = .FALSE.
      ISCD = .FALSE.
      DO 20 J = 1,7
         DO 10 I = 1,7
            IF (PCMATX(I,J).NE.0.0) ISPC = .TRUE.
            IF (CDMATX(I,J).NE.0.0) ISCD = .TRUE.
            IF (PVMATX(I,J).NE.0.0) ISPV = .TRUE.
 10         CONTINUE
 20      CONTINUE
C                                       something to do
      IF ((ISPC) .OR. (ISCD)) THEN
C                                       both had values, select one
         IF ((ISPC) .AND. (ISCD)) THEN
            MSGTXT = 'FITS HEADER HAS BOTH CDi_j and PCi_j !!'
            CALL MSGWRT (7)
            DO 30 I = 1,7
               J = KRCIC + I - 1
               IF ((CATR(J).NE.1.0) .AND. (CATR(J).NE.0.0))
     *            ISCD = .FALSE.
 30            CONTINUE
            IF (ISCD) ISPC = .FALSE.
            END IF
C                                       convert PC to CD
         IF (ISPC) THEN
            DO 50 I = 1,7
               CDELT = CATR(KRCIC+I-1)
               IF (CDELT.EQ.0.0) CDELT = 1.0
               DO 40 J = 1,7
                  CDMATX(I,J) = CDELT * PCMATX(I,J)
 40               CONTINUE
 50            CONTINUE
            END IF
C                                       find longitude and latitude
         I1 = 0
         I2 = 0
         J = KHCTP - 2
         DO 60 I = 1,7
            J = J + 2
            CALL H2CHR (8, 1, CATH(J), CTEMP)
            IF ((CTEMP(:4).EQ.'RA--') .OR. (CTEMP(2:4).EQ.'LON')) THEN
               CLONG = CTEMP
               I1 = I
            ELSE IF ((CTEMP(:4).EQ.'DEC-') .OR. (CTEMP(2:4).EQ.'LAT'))
     *         THEN
               CLAT = CTEMP
               I2 = I
               END IF
 60         CONTINUE
         IF ((I1.EQ.0) .OR. (I2.EQ.0)) THEN
            WRITE (MSGTXT,1060) I1, I2
            CALL MSGWRT (6)
            I1 = 0
            I2 = 0
            GO TO 100
            END IF
         IF (((CLONG(:4).EQ.'RA--') .AND. (CLAT(:4).NE.'DEC-')) .OR.
     *      ((CLAT(:4).EQ.'DEC-') .AND. (CLONG(:4).NE.'RA--')) .OR.
     *      (CLONG(5:8).NE.CLAT(5:8))) THEN
            MSGTXT = 'LONGITUDE ''' // CLONG // ''' AND LATITUDE ''' //
     *         CLAT // ''' DON''T MATCH'
            CALL MSGWRT (6)
            I1 = 0
            I2 = 0
            GO TO 100
            END IF
C                                       now check axes
         IF (CDMATX(I2,I1).GT.0.0) THEN
            RHOA = ATAN2 (CDMATX(I2,I1), CDMATX(I1,I1)) * RAD2DG
         ELSE IF (CDMATX(I2,I1).EQ.0.0) THEN
            RHOA = 0.0
         ELSE
            RHOA = ATAN2 (-CDMATX(I2,I1), -CDMATX(I1,I1)) * RAD2DG
            END IF
         IF (CDMATX(I1,I2).GT.0.0) THEN
            RHOB = ATAN2 (CDMATX(I1,I2), -CDMATX(I2,I2)) * RAD2DG
         ELSE IF (CDMATX(I1,I2).EQ.0.0) THEN
            RHOB = 0.0
         ELSE
            RHOB = ATAN2 (-CDMATX(I1,I2), CDMATX(I2,I2)) * RAD2DG
            END IF
         IF (RHOA-RHOB.GT.170.) THEN
            RHOB = RHOB + 180.0
         ELSE IF (RHOA-RHOB.LT.-170.0) THEN
            RHOA = RHOA + 180.0
            END IF
         IF (ABS(RHOA-RHOB).GT.0.1) THEN
            WRITE (MSGTXT,1061) RHOA, RHOB
            CALL MSGWRT (7)
            MSGTXT = 'USE DSKEW ON THIS IMAGE TO CORRECT THE SKEW'
            CALL MSGWRT (7)
            END IF
         RHOA = (RHOA + RHOB) / 2.0
         IF (RHOA.GT.91.) RHOA = RHOA - 180.0
         IF (RHOA.LT.-91.) RHOA = RHOA + 180.0
         RHOD = RHOA * DG2RAD
         IF (ABS(SIN(RHOD)).LT.0.004) THEN
            CATR(KRCIC+I1-1) = CDMATX(I1,I1) / COS (RHOD)
            CATR(KRCIC+I2-1) = CDMATX(I2,I2) / COS (RHOD)
         ELSE IF (ABS(COS(RHOD)).LT.0.004) THEN
            CATR(KRCIC+I1-1) = CDMATX(I2,I1) / SIN (RHOD)
            CATR(KRCIC+I2-1) = -CDMATX(I1,I2) / SIN (RHOD)
         ELSE
            R1 = CDMATX(I1,I1) / COS (RHOD)
            R2 = CDMATX(I2,I1) / SIN (RHOD)
            D1 = CDMATX(I2,I2) / COS (RHOD)
            D2 = -CDMATX(I1,I2) / SIN (RHOD)
            IF (ABS(SIN(RHOD)).LT.0.1) THEN
               CATR(KRCIC+I1-1) = R1
               CATR(KRCIC+I2-1) = D1
            ELSE IF (ABS(COS(RHOD)).LT.0.1) THEN
               CATR(KRCIC+I1-1) = R2
               CATR(KRCIC+I2-1) = D2
            ELSE
               CATR(KRCIC+I1-1) = (R1 + R2) / 2.0
               CATR(KRCIC+I2-1) = (D1 + D2) / 2.0
               END IF
            IF ((ABS((R1-R2)/(R1+R2)).GT.0.0005) .OR.
     *         (ABS((D1-D2)/(D1+D2)).GT.0.0005)) THEN
               R1 = R1 * 3600.0
               D1 = D1 * 3600.0
               R2 = R2 * 3600.0
               D2 = D2 * 3600.0
               WRITE (MSGTXT,1062) R1, D1, R2, D2
               CALL MSGWRT (7)
               MSGTXT = 'USE DSKEW ON THIS IMAGE TO CORRECT THE SKEW'
               CALL MSGWRT (7)
               END IF
            END IF
         CATR(KRCRT+I2-1) = RHOA
C                                       Fill in other CDELTs
 100     DO 110 I = 1,7
            IF ((I.NE.I1) .AND. (I.NE.I2)) THEN
               J = KRCIC + I - 1
               IF (CDMATX(I,I).NE.0.0) CATR(J) = CDMATX(I,I)
               IF (CATR(J).EQ.0.0) CATR(J) = 1.0
               END IF
 110        CONTINUE
         END IF
C                                       oblique -SIN ?
      IF (ISPV) THEN
         I1 = 0
         I2 = 0
         J = CATBLK(KIDIM)
         DO 200 I = 1,J
            CALL H2CHR (8, 1, CATH(KHCTP+2*(I-1)), CTEMP)
            IF (((CTEMP(:4).EQ.'DEC-') .OR. (CTEMP(2:4).EQ.'LAT')) .AND.
     *         (CTEMP(5:8).EQ.'-SIN')) THEN
               I1 = I
               DEC = CATD(KDCRV+I-1)
               END IF
            IF (((CTEMP(:4).EQ.'RA--') .OR. (CTEMP(2:4).EQ.'LON')) .AND.
     *         (CTEMP(5:8).EQ.'-SIN')) I2 = I
 200        CONTINUE
         ISPV = (I2.GT.0)
         DO 210 I = 1,7
            DO 205 J = 1,7
               IF ((I.EQ.I1) .AND. (J.EQ.2)) THEN
                  IF (PVMATX(I,J).EQ.0.0) ISPV = .FALSE.
               ELSE
                  IF (PVMATX(I,J).NE.0.0) ISPV = .FALSE.
                  END IF
 205           CONTINUE
 210        CONTINUE
         ISPV = ISPV .AND. (DEC.NE.0.0D0)
         IF (ISPV) THEN
            IF (ABS(PVMATX(I1,2)-1.0D0/TAN(DG2RAD*DEC)).GT.0.1) ISPV =
     *         .FALSE.
            END IF
         IF (ISPV) THEN
            MSGTXT = 'Changing oblique -SIN to -NCP'
            CALL MSGWRT (3)
            CTEMP = 'NCP'
            CALL CHR2H (3, CTEMP, 6, CATH(KHCTP+2*I1-2))
            CALL CHR2H (3, CTEMP, 6, CATH(KHCTP+2*I2-2))
         ELSE
            MSGTXT = 'PROJECTION PARAMETERS PVi_j NOT UNDERSTOOD'
            CALL MSGWRT (7)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('PCHDR: LONGITUDE AND/OR LATITUDE AXES NOT FOUND',2I2)
 1061 FORMAT ('PCHDR: IMAGE APPEARS TO BE SKEWED:',F8.2,' NOT',F8.2)
 1062 FORMAT ('PCHDR: IMAGE SKEWED:',2F9.5,' NOT',2F9.5)
      END
