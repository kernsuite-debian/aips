      SUBROUTINE DGGET (VISIN, IND, TRANSL, MVIS, JADR, SELFAC, ALLWT,
     *   VISOUT, DROP)
C-----------------------------------------------------------------------
C! Selects uv data and changes Stokes' type.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2003
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
C   Gets requested data from visibility record, reformatting if needed.
C   REQUIRES setup by DGINIT to set values of MVIS, JADR, SELFAC and
C   ALLWT.
C   Inputs:
C      VISIN    R(IND,*)   Input visibility array
C      IND      I          First dimension of VISIN (CATBLK(KINAX))
C      TRANSL   L          f => Do simple copy, not combining pols
C      MVIS     I(8)       Number of visibilities in requested output
C                          format., # pol, BIF, EIF, BCH, ECH, JNCIF,
C                          JNCF
C      JADR     I(2,4)     Pointers to the first and second visibility
C                          input records to be used in the output
C                          record. If JADR(1,n) is negative use IABS
C                          (JADR(1,n)) and multiply the visibility by i
C                          (=SQRT(-1)).  If JADR(2,n) < 0, use IABS and
C                          construct (1) + i*(2)
C      SELFAC   R(2,4)     Factors to be multiplied by the first and
C                          second input vis's to make the output vis.
C      ALLWT    L          Flag, = .TRUE. if all visibilities must have
C                          positive weight.
C   Output:
C      VISOUT   R(3,*)     Output visibility record
C      DROP     L          .TRUE. if all data in record flaged.
C-----------------------------------------------------------------------
      INTEGER   MVIS(8), JADR(2,4), IND
      LOGICAL   TRANSL, ALLWT, DROP
      REAL      VISIN(IND,*), SELFAC(2,4), VISOUT(3,*)
C
      INTEGER   I, IP1, IP2, LF, LC, LP, LFOFF, IOFF
      LOGICAL   GOOD, HASWT
      REAL      WT1, WT2, TEMP, XFACT1, XFACT2
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      GOOD = .FALSE.
C                                       See if weight in data
      HASWT = IND.GE.3
C                                       Check input.
      IF (MVIS(1).LE.0) GO TO 999
C                                       Loop checking
C                                       and summing weights
C                                       and getting visibilities.
      I = 0
      LFOFF = (MVIS(3) - 2) * MVIS(7) + (MVIS(5) - 2) * MVIS(8)
      DO 70 LF = MVIS(3),MVIS(4)
         LFOFF = LFOFF + MVIS(7)
         IOFF = LFOFF
         DO 60 LC = MVIS(5),MVIS(6)
            IOFF = IOFF + MVIS(8)
            IF (TRANSL) THEN
               DO 40 LP = 1,MVIS(2)
                  I = I + 1
C                                       Initialize output
                  VISOUT(1,I) = 0.0
                  VISOUT(2,I) = 0.0
                  VISOUT(3,I) = 0.0
C                                       Set pointers.
                  IP1 = ABS (JADR(1,LP)) + IOFF
                  IP2 = ABS (JADR(2,LP)) + IOFF
C                                       Get weights.
                  IF (HASWT) THEN
                     WT1 = MAX (0.0, VISIN(3,IP1))
                     WT2 = MAX (0.0, VISIN(3,IP2))
                  ELSE
                     WT1 = 1.0
                     WT2 = 1.0
                     IF (VISIN(1,IP1).EQ.FBLANK) WT1 = 0.0
                     IF (VISIN(1,IP2).EQ.FBLANK) WT2 = 0.0
                     END IF
C                                       Check weights.
                  IF ((ALLWT) .AND. ((WT1.LE.0.0) .OR. (WT2.LE.0.0)))
     *                  GO TO 40
C                                       Set visibility.
                  VISOUT(3,I) = WT1 + WT2
                  IF (VISOUT(3,I).GT.0.0) THEN
                     XFACT1 = 0.0
                     XFACT2 = 0.0
                     IF (WT1.GT.0.0) XFACT1 = SELFAC(1,LP)
                     IF (WT2.GT.0.0) XFACT2 = SELFAC(2,LP)
                     GOOD = .TRUE.
C                                       RL, LR from Q, U
                     IF (JADR(2,LP).LT.0) THEN
                        VISOUT(1,I) = XFACT1 * VISIN(1,IP1) - XFACT2
     *                     * VISIN(2,IP2)
                        VISOUT(2,I) = XFACT1 * VISIN(2,IP1) + XFACT2 *
     *                     VISIN(1,IP2)
                     ELSE
                        VISOUT(1,I) = XFACT1 * VISIN(1,IP1) + XFACT2
     *                     * VISIN(1,IP2)
                        VISOUT(2,I) = XFACT1 * VISIN(2,IP1) + XFACT2 *
     *                     VISIN(2,IP2)
C                                       Upol, mult. by i
                        IF (JADR(1,LP).LT.0) THEN
                           TEMP = VISOUT(1,I)
                           VISOUT(1,I) = - VISOUT(2,I)
                           VISOUT(2,I) = TEMP
                           END IF
                        END IF
C                                       If one wt .le. 0  double VIS.
                     IF ((WT1.LE.0.0) .OR. (WT2.LE.0.0)) THEN
                        VISOUT(1,I) = 2.0 * VISOUT(1,I)
                        VISOUT(2,I) = 2.0 * VISOUT(2,I)
C                                       correct weight combination
                     ELSE IF (IP1.NE.IP2) THEN
                        VISOUT(3,I) = 4.0 * WT1 * WT2 / (WT1 + WT2)
                     ELSE
                        VISOUT(3,I) = (WT1 + WT2) / 2.0
                        END IF
                     END IF
 40               CONTINUE
C                                       No translation
            ELSE
               DO 50 LP = 1,MVIS(2)
                  I = I + 1
                  IP1 = ABS (JADR(1,LP)) + IOFF
C                                       Get weights.
                  IF (HASWT) THEN
                     WT1 = MAX (0.0, VISIN(3,IP1))
                  ELSE
                     WT1 = 1.0
                     IF (VISIN(1,IP1).EQ.FBLANK) WT1 = 0.0
                     END IF
C                                       Set visibility.
                  VISOUT(3,I) = WT1
                  IF (WT1.GT.0.0) GOOD = .TRUE.
                  VISOUT(1,I) = VISIN(1,IP1)
                  VISOUT(2,I) = VISIN(2,IP1)
 50               CONTINUE
               END IF
 60         CONTINUE
 70      CONTINUE
C                                       See if any good data.
      DROP = .NOT. GOOD
C
 999  RETURN
      END
