      SUBROUTINE GETBND (NUMIF, FREQS, BNDCOD, IBANDF, CHBAND)
C-----------------------------------------------------------------------
C! find the bandcode to apply with these data
C# Utility UV VLA Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2014
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
C   Finds the band code number
C   Inputs:
C      NUMIF    I        Number frequencies
C      FREQS    D(*)     Frequencies: ones < 1000 -> GHz, else Hz
C      BNDCOD   C(*)*8   band codes
C   Output:
C      IBANDF   I        band number 1-10 - message if confusion
C      CHBAND   C*(*)    1-character code for IBANDF
C-----------------------------------------------------------------------
      INTEGER   NUMIF, IBANDF
      DOUBLE PRECISION FREQS(*)
      CHARACTER BNDCOD(*)*(*), CHBAND*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, J, K, L, LBAND(MAXIF), LERR(MAXIF)
      DOUBLE PRECISION FORMAL(10), SAFE(10,2), EXTREM(10,2), DM
      CHARACTER CODES(10)*1, LCODE*1
      INCLUDE 'INCS:DMSG.INC'
      DATA CODES / '4', 'P', 'L', 'S', 'C', 'X', 'U', 'K', 'A', 'Q'/
      DATA FORMAL /0.15D9, 0.7D9, 2.0D9, 4.D0, 8.D9, 12.D9, 18.D9,
     *   26.5D9, 40.0D9, 56.0D9/
      DATA EXTREM /0.0D0, 0.15D9, 0.7D9, 2.0D9, 4.0D0, 8.0D9, 12.D9,
     *   18.D9,  26.5D9, 38.5D9,
     *            0.15D9, 0.7D9,  2.0D9, 4.0D0, 8.0D9, 12.D9, 18.D9,
     *   26.5D9, 40.5D9, 60.0D9/
      DATA SAFE /0.0D0, 0.15D9, 0.7D9, 2.0D9, 4.0D0, 8.0D9, 12.D9,
     *   18.D9,  26.5D9, 40.5D9,
     *            0.15D9, 0.7D9,  2.0D9, 4.0D0, 8.0D9, 12.D9, 18.D9,
     *   26.5D9, 38.5D9, 56.0D9/
C-----------------------------------------------------------------------
      IBANDF = 0
      DM = 1.0D0
      IF (FREQS(1).LE.1.D3) DM = 1.0D9
C                                       loop for each IF
      DO 30 I = 1,NUMIF
         LBAND(I) = 0
         LERR(I) = 0
C                                       did they tell us?
         IF (BNDCOD(I).NE.' ') THEN
            IF (BNDCOD(I)(:5).EQ.'EVLA_') THEN
               LCODE = BNDCOD(I)(6:6)
            ELSE
               LCODE = BNDCOD(I)(:1)
               END IF
            DO 10 J = 1,10
               IF (LCODE.EQ.CODES(J)) THEN
                  LBAND(I) = J
                  IBANDF = J
                  IF ((DM*FREQS(I).GE.SAFE(J,1)) .AND.
     *               (DM*FREQS(I).LE.SAFE(J,2))) LERR(I) = -1
                  IF ((DM*FREQS(I).LT.EXTREM(J,1)) .AND.
     *               (DM*FREQS(I).GT.EXTREM(J,2))) LERR(I) = 1
                  END IF
 10            CONTINUE
            END IF
C                                       no - look at formal boundaries
         IF (LBAND(I).LE.0) THEN
            DO 20 J = 1,10
               IF (DM*FREQS(I).LT.FORMAL(J)) THEN
                  LBAND(I) = J
                  IF ((DM*FREQS(I).GE.SAFE(J,1)) .AND.
     *               (DM*FREQS(I).LE.SAFE(J,2))) LERR(I) = -1
                  GO TO 30
                  END IF
 20            CONTINUE
            END IF
 30      CONTINUE
C                                       now what is the single answer
      J = NUMIF / 2
      J = MAX (1, J)
      IF ((LBAND(J).GT.0) .AND. (LERR(J).LT.0)) THEN
         IBANDF = LBAND(J)
      ELSE
         DO 35 K = 1,J
            L = J + K
            IF ((L.LE.NUMIF) .AND. (LBAND(L).GT.0) .AND. (LERR(L).LT.0))
     *         THEN
               IBANDF = LBAND(L)
               GO TO 50
               END IF
            L = J - K
            IF ((L.GE.1) .AND. (LBAND(L).GT.0) .AND. (LERR(L).LT.0))
     *         THEN
               IBANDF = LBAND(L)
               GO TO 50
               END IF
 35         CONTINUE
         DO 40 K = 1,J
            L = J - K + 1
            IF ((L.GE.1) .AND. (LBAND(L).GT.0) .AND. (LERR(L).LE.0))
     *         THEN
               IBANDF = LBAND(L)
               GO TO 50
               END IF
            L = J + K
            IF ((L.LE.NUMIF) .AND. (LBAND(L).GT.0) .AND. (LERR(L).LE.0))
     *         THEN
               IBANDF = LBAND(L)
               GO TO 50
               END IF
 40         CONTINUE
         MSGTXT = 'GETBND: NO OBVIOUS BAND IDENTIFICATION'
         CALL MSGWRT (7)
         IF (IBANDF.LE.0) GO TO 999
         END IF
C                                       is it consistent?
 50   DO 70 I = 1,NUMIF
         IF (LBAND(I).NE.IBANDF) THEN
            IF ((DM*FREQS(I).LT.EXTREM(IBANDF,1)) .OR.
     *         (DM*FREQS(I).GT.EXTREM(IBANDF,2))) THEN
               WRITE (MSGTXT,1050) I, CODES(LBAND(I)), CODES(IBANDF)
               CALL MSGWRT (6)
               END IF
            END IF
 70      CONTINUE
      CHBAND = CODES(IBANDF)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('GETBND IF',I3,' APPARENT BAND ID ',A1,
     *   ' INCONSISTENT WITH SELECTED BAND ',A1)
      END
