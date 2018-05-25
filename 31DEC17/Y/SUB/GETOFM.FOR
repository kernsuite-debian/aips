      SUBROUTINE GETOFM (OFMFIL, DOTV, TVC, DOOFM, DOCOLR, ROFM, GOFM,
     *   BOFM, NOFM, IERR)
C-----------------------------------------------------------------------
C! Get on OFM from TV or file, init the TV for a 3-color display
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2002, 2008-2009, 2012
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
C   Get an OFM from a file or the TV and then init the TV if needed
C   Inputs:
C      OFMFIL   C*48   OFM file name: ' ' -> none
C      DOTV     L      Writing to the TV
C      TVC      I(3)   TV channels rgb
C   Outputs:
C      DOOFM    L      Use OFM in plotting
C      DOCOLR   L      OFMs are in color
C      ROFM     R(*)   Red OFM memory (0-1)
C      GOFM     R(*)   Green OFM memory (0-1)
C      BOFM     R(*)   Blue OFM memory (0-1)
C      NOFM     I      Number values returned per OFM
C      IERR     I      Error code
C-----------------------------------------------------------------------
      CHARACTER OFMFIL*(*)
      LOGICAL   DOTV, DOOFM, DOCOLR
      INTEGER   TVC(3), NOFM, IERR
      REAL      ROFM(*), GOFM(*), BOFM(*)
C
      INCLUDE 'INCS:PTVC.INC'
      CHARACTER LOFM*128, HUSER*3, LINE*72
      INTEGER   I, NLEVS, N, JTRIM, TXLUN, TXIND, IC(3), J, TVL, TVI,
     *   II, JJ
      REAL      OFFS, SLOPE, OFMBUF(TVMOFM)
      LOGICAL   WASOPN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      DOOFM = OFMFIL.NE.' '
      DOCOLR = .FALSE.
      IERR = 0
      IF (DOOFM) THEN
         IF (OFMFIL.EQ.'TV') THEN
C                                       is TV open
            TVL = 9
            CALL LSERCH ('SRCH', TVL, TVI, .FALSE., IERR)
            WASOPN = .TRUE.
            IF (IERR.EQ.1) THEN
               WASOPN = .FALSE.
               CALL TVOPEN (OFMBUF, IERR)
               END IF
            IF (IERR.NE.0) GO TO 999
            CALL YOFM ('READ', 4, .TRUE., OFMBUF, IERR)
            CALL RCOPY (TVMLOU, OFMBUF, ROFM)
            IF (IERR.EQ.0) CALL YOFM ('READ', 2, .TRUE., OFMBUF, IERR)
            CALL RCOPY (TVMLOU, OFMBUF, GOFM)
            IF (IERR.EQ.0) CALL YOFM ('READ', 1, .TRUE., OFMBUF, IERR)
            CALL RCOPY (TVMLOU, OFMBUF, BOFM)
            DO 10 I = 1,TVMLOU
               IF (ABS(ROFM(I)-GOFM(I)).GT.0.001) DOCOLR = .TRUE.
               IF (ABS(ROFM(I)-BOFM(I)).GT.0.001) DOCOLR = .TRUE.
 10            CONTINUE
            IF (.NOT.WASOPN) THEN
               CALL TVCLOS (OFMBUF, IERR)
               IERR = 0
               END IF
            NOFM = TVMLOU
         ELSE
            TXLUN = 3
C                                       full file spec
            IF (INDEX(OFMFIL,':').NE.0) THEN
               LOFM = OFMFIL
C                                       standard file spec instead
            ELSE
               N = JTRIM (OFMFIL)
               CALL ZEHEX (NLUSER, 3, HUSER)
C                                       try $OFMFIL area
               LOFM = 'OFMFIL:' // OFMFIL(:N) // '.' // HUSER
               MSGSUP = 32000
               CALL ZTXOPN ('QRED', TXLUN, TXIND, LOFM, .FALSE., IERR)
               IF (IERR.EQ.0) THEN
                  CALL ZTXCLS (TXLUN, TXIND, IERR)
C                                       try $AIPSOFM
               ELSE IF (IERR.EQ.5) THEN
                  LOFM = 'AIPSOFM:' // OFMFIL(:N)// '.000'
                  CALL ZTXOPN ('QRED', TXLUN, TXIND, LOFM, .FALSE.,
     *               IERR)
                  IF (IERR.EQ.0) CALL ZTXCLS (TXLUN, TXIND, IERR)
                  END IF
               MSGSUP = 0
               END IF
            CALL ZTXOPN ('READ', TXLUN, TXIND, LOFM, .FALSE., IERR)
            IF (IERR.NE.0) GO TO 990
C                                       Loop reading
 20         CALL ZTXIO ('READ', TXLUN, TXIND, LINE, IERR)
            IF (IERR.EQ.0) THEN
               J = JTRIM (LINE)
               IF (J.LE.0) GO TO 20
               IF (LINE(1:1).EQ.';') GO TO 20
               IF (LINE(6:6).EQ.':') THEN
                  READ (LINE,1010) J, IC
               ELSE
                  READ (LINE,1011) J, IC
                  END IF
               IF ((J.GE.0) .AND. (J.LT.TVMLOU)) THEN
                  NOFM = J + 1
                  ROFM(NOFM) = IC(3) / 1.0E4
                  GOFM(NOFM) = IC(2) / 1.0E4
                  BOFM(NOFM) = IC(1) / 1.0E4
                  IF ((IC(1).NE.IC(2)) .OR. (IC(1).NE.IC(3))) DOCOLR =
     *               .TRUE.
                  END IF
               GO TO 20
            ELSE IF (IERR.EQ.2) THEN
               CALL ZTXCLS (TXLUN, TXIND, IERR)
               IERR = 0
            ELSE
               GO TO 990
               END IF
            END IF
         END IF
C                                       Off OFM, 3 channels
      IF ((DOOFM) .AND. (DOTV)) THEN
         I = OFMINP + 1
         CALL RFILL (I, 0.0, OFMBUF)
         NLEVS = LUTOUT + 1
         IF (I.LT.NLEVS) NLEVS = I
         OFFS = 1.0 / REAL (OFMOUT)
         SLOPE = OFFS * (OFMOUT + 1.0) / REAL(NLEVS)
         DO 30 I = 1,NLEVS
            OFMBUF(I) = I * SLOPE - OFFS
 30         CONTINUE
         I = OFMINP + 1
         I = I / NLEVS
         JJ = NLEVS
         DO 31 II = 2,I
            CALL RCOPY (NLEVS, OFMBUF, OFMBUF(JJ+1))
            JJ = JJ + NLEVS
 31         CONTINUE
         CALL YOFM ('WRIT', 7, .TRUE., OFMBUF, IERR)
         IF (IERR.EQ.0) CALL YSLECT ('OFFF', TVC(1), 0, OFMBUF, IERR)
         IF (IERR.EQ.0) CALL YSLECT ('OFFF', TVC(2), 0, OFMBUF, IERR)
         IF (IERR.EQ.0) CALL YSLECT ('OFFF', TVC(3), 0, OFMBUF, IERR)
         END IF
C
 990  IF (IERR.NE.0) DOOFM = .FALSE.
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (I4,2X,3(1X,I5))
 1011 FORMAT (I5,2X,3(1X,I5))
      END
