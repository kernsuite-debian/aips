      SUBROUTINE CONV (ISOPT, IN, VAL, WTIN, OUT, IERR)
C-----------------------------------------------------------------------
C! Tim Cornwell routine: Convolve a map with a beam.
C# Math Map AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2008
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
C   CONV calculates the convolution of the map IN1 with the beam
C   and stores the answer in OUT.
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      INTEGER   IN, OUT, IDIR, JLIM, FIL, KAP, WTIN, NEED
      REAL      VAL, RMAX, RMIN, RFACT, WREAL, WIMAG, R0
      LOGICAL   ISOPT
      INTEGER   AKOPEN, AKCESS, AKCLOS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA R0 /0.0/
C-----------------------------------------------------------------------
      IERR = 0
      RFACT = REAL(NX) * REAL(NY) * VAL
      NEED = (2 * NX * NY) / 1024
C                                       Convolve (radio)
      IF (.NOT.ISOPT) THEN
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            MSGTXT = 'CONV: UNABLE TO GET AP MEMORY AT 1'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         CALL APCONV (HNX, HNY, SCRNM(IN), SCRNM(WK2), SCRNM(WK3),
     *      SCRNM(OUT), SCRNM(WTIN), RFACT, BUFSZ(1), BUFFR1, BUFFR2,
     *      BUFFR3, RESMAX, RESMIN, IERR)
         CALL QRLSE
         IF (IERR.NE.0) GO TO 990
         GO TO 999
         END IF
C                                       Convolve (optical)
      CALL SMTOBG (IN, R0, WK1, IERR)
      IF (IERR.NE.0) GO TO 990
      FIL = WK1
C                                        Do FFT to get transform of IMG
      IDIR = 3
      CALL QINIT (NEED, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         MSGTXT = 'CONV: UNABLE TO GET AP MEMORY AT 2'
         CALL MSGWRT (8)
         GO TO 990
         END IF
      CALL DSKFFT (NX, NY, IDIR, T, SCRNM(FIL), SCRNM(WK3),
     *   SCRNM(WK2), BUFSZ(1), BUFFR1, BUFFR2, RMAX, RMIN, IERR)
      IF (IERR.EQ.0) GO TO 115
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
 115  CONTINUE
C
      CALL QRLSE
C
      IF (AKOPEN (WTIN, 1, 'READ', BUFFR1) .NE.0) GO TO 990
C
      VMSZ(1,WK2) = NY * 2
      VMSZ(2,WK2) = NX / 2 + 1
      IF (AKOPEN (WK2, 2, 'READ', BUFFR2) .NE.0) GO TO 990
      VMSZ(1,WK1) = NY * 2
      VMSZ(2,WK1) = NX / 2 + 1
      IF (AKOPEN (WK1, 3, 'WRIT', BUFFR3) .NE.0) GO TO 990
C                                       Begin loop thru map.
C                                       Normalise appropriately
      JLIM = NX / 2 + 1
      DO 125 IY = 1,JLIM
         IF (AKCESS (WTIN, BUFFR1) .NE.0) GO TO 990
         IF (AKCESS (WK2, BUFFR2) .NE.0) GO TO 990
         IF (AKCESS (WK1, BUFFR3) .NE.0) GO TO 990
         I1 = BIND(1)
         I2 = BIND(2)
         I3 = BIND(3)
      INCLUDE 'INCS:ZVND.INC'
         DO 120 IX = 1,NY
            WREAL = RFACT * BUFFR1(I1)
            WIMAG = RFACT * BUFFR1(I1+1)
            BUFFR3(I3)   = WREAL * BUFFR2(I2)   - WIMAG * BUFFR2(I2+1)
            BUFFR3(I3+1) = WREAL * BUFFR2(I2+1) + WIMAG * BUFFR2(I2)
            I1 = I1 + 2
            I2 = I2 + 2
            I3 = I3 + 2
 120        CONTINUE
 125     CONTINUE
      IF (AKCLOS (WTIN, BUFFR1) .NE.0) GO TO 990
      IF (AKCLOS (WK2, BUFFR2) .NE.0) GO TO 990
      IF (AKCLOS (WK1, BUFFR3) .NE.0) GO TO 990
C                                        FFT back to map plane
 128  IDIR = -1
C
      CALL QINIT (NEED, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         MSGTXT = 'CONV: UNABLE TO GET AP MEMORY AT 3'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C
      CALL DSKFFT (NX, NY, IDIR, T, SCRNM(WK1), SCRNM(WK3),
     *   SCRNM(WK2), BUFSZ(1), BUFFR1, BUFFR2, RESMAX, RESMIN, IERR)
      IF (IERR.EQ.0) GO TO 130
         WRITE (MSGTXT,1180) IERR
         CALL MSGWRT (8)
         GO TO 999
 130  CONTINUE
C
      CALL QRLSE
C
      VMSZ(1,WK1) = NX
      VMSZ(2,WK1) = NY
      VMSZ(1,WK2) = NX
      VMSZ(2,WK2) = NY
C
      CALL BGTOSM (WK2, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C
 990  WRITE (MSGTXT,1010)
      CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CONV: ERROR ',I3,' IN DSKFFT MAP TO U,V')
 1010 FORMAT ('CONV')
 1180 FORMAT ('CONV: ERROR ',I3,' IN DSKFFT U,V TO MAP')
      END
