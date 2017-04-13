      SUBROUTINE BPSHFT (SPECT, NCOR, NIF, NCHAN, KSNCS, KSNCIF,
     *   KSNCF, SHIFT, DATTYP)
C-----------------------------------------------------------------------
C! Control frequency shift of cross power spectra
C# UV IO-appl Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2015
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
C  A routine to control the shifting of spectral data. It does the shift
C  on each IF and polzn spectrum independently.
C
C  Input/Output:
C     SPECT(*)        R        Input spectrum, output shifted spectrum
C  Input:
C     NCOR            I        # polzns
C     NIF             I        # IFs
C     NCHAN           I        # channels
C     KSNCS           I        polzn increment in buffer
C     KSNCIF          I        IF increment in buffer
C     KSNCF           I        channel increment in buffer
C     SHIFT(*)        D        shift (in channels) to be performed,
C                              IF dependent
C     DATTYP          I        Type of data that was used to form the
C                              BP table: 0 = don't know, assume x-p
C                                        1 = x-p
C                                        2 = t-p
C                                        3 = both
C-----------------------------------------------------------------------
      REAL SPECT(*)
      DOUBLE PRECISION SHIFT(*)
      INTEGER NCOR, NIF, NCHAN, KSNCS, KSNCIF, KSNCF, DATTYP
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER IPOL, IFNO, INDEX, IFRQ, FINDEX, ISB, ISMTH
      LOGICAL DOINTP, ALLFLG
      REAL    VISTMP(2,MAXCHA), TMPWT(MAXCHA), INTWTS(MAXCHA)
      SAVE VISTMP, TMPWT, INTWTS
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DO 300 IPOL = 1,NCOR
         DO 200 IFNO = 1,NIF
C                                       Copy data to temp array.
            DOINTP = .FALSE.
            INDEX = 1 + (IPOL-1) * KSNCS + (IFNO-1) * KSNCIF
            DO 120 IFRQ = 1,NCHAN
               FINDEX = INDEX + (IFRQ-1) * KSNCF
               VISTMP(1,IFRQ) = SPECT(FINDEX)
               VISTMP(2,IFRQ) = SPECT(FINDEX+1)
               TMPWT(IFRQ) = 1.0
               IF ((SPECT(FINDEX).EQ.FBLANK) .OR.
     *            (SPECT(FINDEX+1).EQ.FBLANK)) TMPWT(IFRQ) = -1.0
               IF (TMPWT(IFRQ).LE.0.0) DOINTP = .TRUE.
 120         CONTINUE
C                                       Deal with flagged
C                                       spectral data
            ALLFLG = .FALSE.
            IF (DOINTP) CALL SPINTP (NCHAN, VISTMP, TMPWT,
     *         INTWTS, ALLFLG)
            IF (ALLFLG) THEN
               DO 125 IFRQ = 1,NCHAN
                  FINDEX = INDEX + (IFRQ-1) * KSNCF
                  SPECT(FINDEX) = FBLANK
                  SPECT(FINDEX+1) = FBLANK
 125              CONTINUE
               GO TO 200
               END IF
C                                       Shift it!
            ISB = 1
            IF (DATTYP.EQ.2) THEN
               ISMTH = 1
               CALL TPSHFT (VISTMP, NCHAN, SHIFT(IFNO), INTWTS, ISMTH)
            ELSE
               CALL XCSHNQ (VISTMP, ISB, NCHAN, SHIFT(IFNO), INTWTS)
               END IF
C                                       Copy data back to vis
C                                       array
            DO 140 IFRQ = 1,NCHAN
               FINDEX = INDEX + (IFRQ-1) * KSNCF
               IF (DATTYP.EQ.2) THEN
                  SPECT(FINDEX) = SQRT (VISTMP(1,IFRQ)*VISTMP(1,IFRQ)
     *               + VISTMP(2,IFRQ)*VISTMP(2,IFRQ))
                  SPECT(FINDEX+1) = 0.0
               ELSE
                  SPECT(FINDEX) = VISTMP(1,IFRQ)
                  SPECT(FINDEX+1) = VISTMP(2,IFRQ)
                  END IF
 140           CONTINUE
 200        CONTINUE
 300     CONTINUE
C
      RETURN
      END

