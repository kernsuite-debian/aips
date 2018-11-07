      SUBROUTINE GETCDS (DISK, CNO, VER, ISUB, FREQID, CATBLK, TCAL,
     *   IERR)
C-----------------------------------------------------------------------
C! Reads in the values found in the CD table (CalDevice)
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2010, 2015, 2017
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
C   Reads the CD table TCALS
C   Inputs:
C      DISK     I      Disk number
C      CNO      I      Catalog number
C      CATBLK   I(*)   Data set header
C      ISUB     I      Restrict to this subarray (0 => any)
C      FREQID   I      Restrict to this freq ID (0 => any)
C   In/out:
C      VER      I      CD file version number: 0 -> high
C   Output:
C      TCALS    R(*)   Tcal in Kelvin (4,MAXIF,MAXANT) =
C                         (pol/solar,if,ant)
C      IERR     I      Error code
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DISK, CNO, VER, ISUB, FREQID, CATBLK(*), IERR
      REAL      TCAL(4,MAXIF,*)
C
      INTEGER   CDBUFF(512), LUN, LUNTMP, CDRNO, CDKOLS(7), CDNUMV(7),
     *   NUMA, NUMP, NUMI, IA, ISA, IFQ, NUMR, I, J, K, NA, NPRINT, M
      REAL      TC(4,MAXIF)
      CHARACTER RDATE*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      NPRINT = 0
      I = 2 * MAXIF * MAXANT
      CALL RFILL (I, FBLANK, TCAL)
      LUN = LUNTMP (1)
      CALL CDINI ('READ', CDBUFF, DISK, CNO, VER, CATBLK, LUN, CDRNO,
     *   CDKOLS, CDNUMV, NUMA, NUMP, NUMI, RDATE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN CD TABLE'
         GO TO 990
         END IF
      NUMR = CDBUFF(5)
      K = 4 * NUMI
      NA = 0
      DO 20 I = 1,NUMR
         CALL TABCD ('READ', CDBUFF, CDRNO, CDKOLS, CDNUMV, NUMP, NUMI,
     *      IA, ISA, IFQ, TC, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ CD TABLE'
            GO TO 980
            END IF
         IF ((IERR.EQ.0) .AND. (IA.GT.0) .AND. (IA.LE.MAXANT) .AND.
     *      ((ISUB.LE.0) .OR. (ISA.LE.0) .OR. (ISA.EQ.ISUB)) .AND.
     *      ((FREQID.LE.0) .OR. (IFQ.LE.0) .OR. (IFQ.EQ.FREQID))) THEN
            CALL RCOPY (K, TC, TCAL(1,1,IA))
            DO 10 J = 1,NUMI
               DO 5 M = 1,4
                  IF ((TCAL(M,J,IA).LE.0.0) .OR. (TCAL(M,J,IA).EQ.1.0))
     *               THEN
                     NPRINT = NPRINT + 1
                     WRITE (MSGTXT,1010) J, M, IA, TCAL(M,J,IA)
                     IF (NPRINT.LT.5) CALL MSGWRT (6)
                     TCAL(M,J,IA) = FBLANK
                     END IF
 5                CONTINUE
 10            CONTINUE
            NA = NA + 1
            END IF
 20      CONTINUE
      IERR = 0
      IF (NA.LE.0) THEN
         IERR = 10
         MSGTXT = 'NO CD RECORDS FOUND FOR REQUESTED SUBARRAY/FREQID'
      ELSE IF (NA.LT.NUMA) THEN
         WRITE (MSGTXT,1020) NA, NUMA
         CALL MSGWRT (6)
         END IF
      IF (NPRINT.GE.3) THEN
         WRITE (MSGTXT,1025) NPRINT
         CALL MSGWRT (6)
         END IF
C                                       close
 980  CALL TABCD ('CLOS', CDBUFF, CDRNO, CDKOLS, CDNUMV, NUMP, NUMI,
     *   IA, ISA, IFQ, TC, I)
C
 990  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETCDS: ERROR',I4,' ON ',A)
 1010 FORMAT ('GETCDS: IF/POL',I3,I2,' ANT',I3,' TCAL',1PE12.4,
     *   ' BLANKED')
 1020 FORMAT ('GETCDS WARNING: READ',I4,' ANTENNAS, EXPECTED',I4)
 1025 FORMAT ('GETCDS BLANKED',I5,
     *   ' POLS/IFS/ANTENNAS FOR CD <= 0 OR = 1')
      END
