      SUBROUTINE DXYGET (VISIN, IND, IA1, IA2, VISOUT, DROP)
C-----------------------------------------------------------------------
C! Selects uv data and changes Stokes' type.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2017-2018
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
C   Gets requested data from XY Stokes visibility record, reformatting as
C   needed.  REQUIRES setup by DGINIT to set values in common DSEL.INC.
C   Inputs:
C      VISIN    R(IND,*)   Input visibility array
C      IND      I          First dimension of VISIN (CATBLK(KINAX))
C      IA1      I          Antenna 1 number
C      IA2      I          Antenna 2 number
C   Input from DSEL.INC
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
      INTEGER   IND, IA1, IA2
      LOGICAL   DROP
      REAL      VISIN(IND,*), VISOUT(3,*)
C
      INTEGER   I, LF, LC, LP, LFOFF, IOFF, K
      LOGICAL   GOOD, HASWT
      REAL      WT1, RV(4), IV(4), DCOS, DSIN, SCOS, SSIN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DPDC.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      GOOD = .FALSE.
C                                       See if weight in data
      HASWT = IND.GE.3
C                                       Check input.
      IF (MVIS(1).LE.0) GO TO 999
C                                       zero output
      I = 3 * MVIS(2) * (MVIS(6)-MVIS(5)+1) * (MVIS(4)-MVIS(3)+1)
      CALL RFILL (I, 0.0, VISOUT)
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
            WT1 = 0.0
C                                       advance output pointer
C                                       in case flagged
            I = I + MVIS(2)
            IF (HASWT) THEN
               DO 10 LP = 1,4
                  IF (VISIN(3,LP+IOFF).LE.0.0) GO TO 60
                  WT1 = WT1 + VISIN(3,LP+IOFF)
 10               CONTINUE
            ELSE
               DO 11 LP = 1,4
                  IF (VISIN(1,LP+IOFF).EQ.FBLANK) GO TO 60
                  IF (VISIN(2,LP+IOFF).EQ.FBLANK) GO TO 60
                  WT1 = WT1 + 1.0
 11               CONTINUE
               END IF
C                                       reset pointer
            I = I - MVIS(2)
C                                       pick up data: note reorder
            GOOD = .TRUE.
            WT1 = WT1 / 2.0
            RV(1) = VISIN(1,IOFF+1)
            RV(4) = VISIN(1,IOFF+2)
            RV(2) = -VISIN(1,IOFF+3)
            RV(3) = -VISIN(1,IOFF+4)
            IV(1) = VISIN(2,IOFF+1)
            IV(4) = VISIN(2,IOFF+2)
            IV(2) = -VISIN(2,IOFF+3)
            IV(3) = -VISIN(2,IOFF+4)
            DCOS = COS (PANGLE(IA1) - PANGLE(IA2))
            DSIN = -SIN (PANGLE(IA1) - PANGLE(IA2))
            SCOS = COS (PANGLE(IA1) + PANGLE(IA2))
            SSIN = -SIN (PANGLE(IA1) + PANGLE(IA2))
C                                       loop through request pols
            DO 40 LP = 1,MVIS(2)
               I = I + 1
C                                       Set visibility.
               VISOUT(3,I) = WT1
               K = -SELFAC(1,LP) - 9.95
C                                       I pol
               IF (K.EQ.1) THEN
                  VISOUT(1,I) = (DCOS*RV(1) + DSIN*RV(2) - DSIN*RV(3) +
     *               DCOS*RV(4)) / 2.0
                  VISOUT(2,I) = (DCOS*IV(1) + DSIN*IV(2) - DSIN*IV(3) +
     *               DCOS*IV(4)) / 2.0
C                                       Q pol
               ELSE IF (K.EQ.2) THEN
                  VISOUT(1,I) = (SCOS*RV(1) - SSIN*RV(2) - SSIN*RV(3) -
     *               SCOS*RV(4)) / 2.0
                  VISOUT(2,I) = (SCOS*IV(1) - SSIN*IV(2) - SSIN*IV(3) -
     *               SCOS*IV(4)) / 2.0
C                                       U pol
               ELSE IF (K.EQ.3) THEN
                  VISOUT(1,I) = (SSIN*RV(1) + SCOS*RV(2) + SCOS*RV(3) -
     *               SSIN*RV(4)) / 2.0
                  VISOUT(2,I) = (SSIN*IV(1) + SCOS*IV(2) + SCOS*IV(3) -
     *               SSIN*IV(4)) / 2.0
C                                       V pol
               ELSE IF (K.EQ.4) THEN
                  VISOUT(1,I) = (-DSIN*IV(1) + DCOS*IV(2) - DCOS*IV(3) -
     *               DSIN*IV(4)) / 2.0
                  VISOUT(2,I) =  (DSIN*RV(1) - DCOS*RV(2) + DCOS*RV(3) +
     *               DSIN*RV(4)) / 2.0
                  END IF
 40            CONTINUE
 60         CONTINUE
 70      CONTINUE
C                                       See if any good data.
      DROP = .NOT. GOOD
C
 999  RETURN
      END
