      SUBROUTINE SETVIS (MODE, NCH, IFNUM, MVIS, JADR, SFACT, ALLWT,
     *   IERR)
C-----------------------------------------------------------------------
C! initializes pointers to select/convert uv data to desired form
C# UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   SETVIS setup the arrays JADR, SFACT and the flag ALLWT for
C   reformatting uv data as specified by MODE.  There is also a check to
C   make sure the desired data is available.  Calls to GETVIS will
C   reformat the data.  Needs values set by UVPGET and VHDRIN.
C   Only 1 IF will be processed.
C   Inputs:
C      MODE    I        Desired output data format:
C                        1 => I
C                        2 => IQU
C                        3 => IQUV
C                        4 => IV
C                        5 => R (right hand circular)
C                        6 => L
C                        7 => RL
C                        8 => straight correlators (used in UVFND)
C                        10+n => n I pol. line maps. (n .le. 8)
C                        20+n => n R pol. line maps.
C                        30+n => n L pol. line maps.
C      NCH     I        First line channel desired.
C      IFNUM   I        IF number wanted.
C   Output:
C      MVIS    I        Number of visibilities in requested output
C                       format.
C      JADR    I(2,*)   Pointers to the first and second visibility
C                       input records to be used in the output record.
C      SFACT   R(2,*)   Factors to be multiplied by the first and
C                       second input vis's to make the output vis.
C      ALLWT   L        Flag, = .TRUE. if all visibilities must have
C                       positive weight.
C      IERR    I        Error flag. 0 =>OK, otherwise data unavailable.
C   Common (input):
C      /MAPHDR/  must have uv header
C      /UVHDR/   must be initialized by UVPGET
C-----------------------------------------------------------------------
      INTEGER   MODE, NCH, IFNUM, MVIS, JADR(2,*), IERR
      LOGICAL   ALLWT
      REAL      SFACT(2,*)
C
      INTEGER   NUMCH, CATBLK(256), JNCS, JNCF, JNCIF, IC, LMODE,
     *   I, IOFF
      REAL      CATR(256)
      DOUBLE PRECISION    CATD(128)
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD)
C-----------------------------------------------------------------------
      IERR = 1
      IF ((MODE.LE.0) .OR. (MODE.GT.38)) GO TO 999
C                                       Set pointers.
      JNCS = INCS / 3
      JNCF = INCF / 3
      JNCIF = INCIF / 3
      IF (JNCIF.LT.1) JNCIF = 1
      IOFF = (IFNUM-1) * JNCIF + (NCH-1) * JNCF
C                                       Check IF
      IF ((IFNUM.GT.1) .AND. (IFNUM.GT.CATBLK(KINAX+JLOCIF))) GO TO 999
      IF (IFNUM.LE.0) GO TO 999
C                                       Branch by MODE
      IF (MODE.GT.10) GO TO 500
      GO TO (10, 10, 10, 10, 150, 200, 150, 350, 400, 450), MODE
C                                       IPOL wanted, Check data type.
 10   CONTINUE
         MVIS = 1
C                                       True IPOL, check.
         IF (ICOR0.LE.0) GO TO 20
            IF (ICOR0.GT.1) GO TO 999
            JADR(1,1) = IOFF + (1-ICOR0) * JNCS + 1
            JADR(2,1) = JADR(1,1)
            SFACT(1,1) = 1.0
            SFACT(2,1) = 0.0
            GO TO 30
C                                       RR, LL etc., check.
 20      CONTINUE
            IF (ICOR0.LT.-2) GO TO 999
            JADR(1,1) = IOFF + 1
            JADR(2,1) = JADR(1,1) + JNCS
            SFACT(1,1) = 0.5
            SFACT(2,1) = 0.5
C                                       Check if only RR or LL and
C                                       if so use it.
            IF ((NCOR.LT.2) .OR. (ICOR0.NE.-1)) JADR(2,1) = JADR(1,1)
C                                       What next?
 30      GO TO (900, 50, 50, 100), MODE
C                                       Q & U wanted - Check data type.
 50      CONTINUE
            MVIS = 3
            IC = 4
C                                       True stokes. Check.
            IF (ICOR0.LT.0) GO TO 70
               IF((NCOR.LT.3) .OR. (ICOR0.GT.2)) GO TO 999
               JADR(1,2) = IOFF + (2-ICOR0) * JNCS + 1
               JADR(1,3) = JADR(1,2) + JNCS
               JADR(2,2) = JADR(1,2)
               JADR(2,3) = JADR(1,3)
               SFACT(1,2) = 1.0
               SFACT(1,3) = 1.0
               SFACT(2,2) = 0.0
               SFACT(2,3) = 0.0
               GO TO 90
C                                       RL, LR - Check.
 70         IF ((NCOR.LT.3) .OR. (ICOR0.LT.-2)) GO TO 999
               JADR(1,2) = IOFF + (3+ICOR0) * JNCS + 1
               JADR(2,2) = JADR(1,2) + JNCS
               JADR(1,3) = JADR(1,2)
               JADR(2,3) = JADR(2,2)
               SFACT(1,2) = 0.5
               SFACT(2,2) = 0.5
               SFACT(1,3) = -0.5
               SFACT(2,3) = 0.5
C                                       What next?
 90         GO TO (900, 900, 110), MODE
C                                       VPOL wanted.
 100     CONTINUE
            IC = 2
C                                       Check type.
 110        MVIS = IC
C                                       True VPOL. Check.
            IF (ICOR0.LT.0) GO TO 120
               IF (NCOR.LT.4) GO TO 999
               JADR(1,IC) = IOFF + (4-ICOR0) * JNCS + 1
               JADR(2,IC) = JADR(1,IC)
               SFACT(1,IC) = 1.0
               SFACT(2,IC) = 0.0
               GO TO 900
C                                       RR, LL - Check.
 120        IF ((NCOR.LT.2) .OR. (ICOR0.LT.-1)) GO TO 999
               JADR(1,IC) = IOFF + (1+ICOR0) * JNCS + 1
               JADR(2,IC) = JADR(1,IC) + JNCS
               SFACT(1,IC) = 0.5
               SFACT(2,IC) = -0.5
               GO TO 900
C                                       RPOL, check type.
 150  CONTINUE
         IC = 2
         MVIS = 1
C                                       True stokes - check.
         IF (ICOR0.LE.0) GO TO 170
            IF ((ICOR0.GT.1) .OR. (NCOR.LT.4)) GO TO 999
            JADR(1,1) = IOFF + (1-ICOR0) * JNCS + 1
            JADR(2,1) = IOFF + (4-ICOR0) * JNCS + 1
            SFACT(1,1) = 1.0
            SFACT(2,1) = 1.0
            IF (MODE.EQ.5) GO TO 900
            GO TO 210
C                                       RR - check.
 170     IF (ICOR0.LT.-1) GO TO 999
            JADR(1,1) = IOFF + 1
            JADR(2,1) = JADR(1,1)
            SFACT(1,1) = 1.0
            SFACT(2,1) = 0.0
            IF (MODE.EQ.5) GO TO 900
            GO TO 210
C                                       LPOL, check type.
 200  CONTINUE
         IC = 1
 210     MVIS = IC
C                                       True stokes - check.
         IF (ICOR0.LE.0) GO TO 230
            IF ((ICOR0.GT.1) .OR. (NCOR.LT.4)) GO TO 999
            JADR(1,IC) = IOFF + (1-ICOR0) * JNCS + 1
            JADR(2,IC) = IOFF + (4-ICOR0) * JNCS + 1
            SFACT(1,IC) = 1.0
            SFACT(2,IC) = -1.0
            GO TO 900
C                                       RR - check.
 230     CONTINUE
            IF (ICOR0.LT.-2) GO TO 999
            IF ((ICOR0.EQ.-1) .AND. (NCOR.EQ.1)) GO TO 999
            JADR(1,IC) = IOFF + (2+ICOR0) * JNCS + 1
            JADR(2,IC) = JADR(1,1)
            SFACT(1,IC) = 1.0
            SFACT(2,IC) = 0.0
            GO TO 900
C                                       pure correlators
 350  CONTINUE
         MVIS = MIN (4, NCOR)
         DO 360 I = 1,MVIS
            JADR(1,I) = IOFF + (I-1) * JNCS + 1
            JADR(2,I) = JADR(1,I)
            SFACT(1,I) = 1.0
            SFACT(2,I) = 0.0
 360        CONTINUE
         IERR = 0
         ALLWT = .FALSE.
         GO TO 999
C                                       Unimplemented types.
 400  CONTINUE
 450  CONTINUE
      GO TO 999
C                                       Line maps.
 500  CONTINUE
         NUMCH = MOD (MODE, 10)
         LMODE = (MODE - NUMCH) / 10
         MVIS = NUMCH
         IOFF = (IFNUM-1) * JNCIF + (NCH-2) * JNCF
C                                       Check number of channels.
         IF ((NUMCH.GT.8) .OR. (NUMCH.LT.1)) GO TO 999
         IF ((NCH+NUMCH-1).GT.CATBLK(KINAX+JLOCF)) GO TO 999
C                                       Check type requested.
         GO TO (550, 600, 650), LMODE
C                                       IPOL - check type.
C                                       True IPOL - check.
 550     IF (ICOR0.LT.0) GO TO 570
            IF (ICOR0.GT.1) GO TO 999
            DO 560 IC = 1,NUMCH
               JADR(1,IC) = IOFF + IC * JNCF + 1
               JADR(2,IC) = JADR(1,IC)
               SFACT(1,IC) = 1.0
               SFACT(2,IC) = 0.0
 560           CONTINUE
            GO TO 900
C                                       RR, LL - check.
 570     IF (ICOR0.LT.-2) GO TO 999
            DO 580 IC = 1,NUMCH
               JADR(1,IC) = IOFF + IC * JNCF + 1
               JADR(2,IC) = JADR(1,IC) + JNCS
               SFACT(1,IC) = 0.5
               SFACT(2,IC) = 0.5
C                                       If only RR or LL use it.
               IF ((NCOR.LT.2) .OR. (ICOR0.NE.-1)) JADR(2,IC) =
     *            JADR(1,IC)
 580           CONTINUE
            GO TO 900
C                                       RPOL - check type.
 600     IF (ICOR0.LT.0) GO TO 630
C                                       True stokes - check.
            IF ((ICOR0.GT.1) .OR. (NCOR.LT.4)) GO TO 999
            DO 610 IC = 1,NUMCH
               JADR(1,IC) = IOFF + IC * JNCF + 1
               JADR(2,IC) = JADR(1,IC) + 3 * JNCS
               SFACT(1,IC) = 1.0
               SFACT(2,IC) = 1.0
 610           CONTINUE
            GO TO 900
C                                       RR, LL - check.
 630     IF (ICOR0.LT.-1) GO TO 999
            DO 640 IC = 1,NUMCH
               JADR(1,IC) = IOFF + IC * JNCF + (1+ICOR0) * JNCS + 1
               JADR(2,IC) = JADR(1,IC)
               SFACT(1,IC) = 1.0
               SFACT(2,IC) = 0.0
 640           CONTINUE
            GO TO 900
C                                       LPOL - check type.
C                                       True stokes - check.
 650     IF (ICOR0.LT.0) GO TO 670
            IF (ICOR0.GT.1) GO TO 999
            DO 660 IC = 1,NUMCH
               JADR(1,IC) = IOFF + IC * JNCF + 1
               JADR(2,IC) = JADR(1,IC) + 3 * JNCS
               SFACT(1,IC) = 1.0
               SFACT(2,IC) = -1.0
 660           CONTINUE
            GO TO 900
C                                       RR, LL - check.
 670     IF (ICOR0.LT.-2) GO TO 999
            IF ((ICOR0.EQ.-1) .AND. (NCOR.EQ.1)) GO TO 999
            DO 680 IC = 1,NUMCH
               JADR(1,IC) = IOFF + IC * JNCF + (2+ICOR0) * JNCS + 1
               JADR(2,IC) = JADR(1,IC)
               SFACT(1,IC) = 1.0
               SFACT(2,IC) = 0.0
 680           CONTINUE
            GO TO 900
C                                          Finished - set ALLWT
 900  ALLWT = .NOT. ((MODE.EQ.1) .OR. (((MODE.EQ.5) .OR.
     *   (MODE.EQ.6)) .AND. (ICOR0.LT.0)))
      IERR = 0
C
 999  RETURN
      END
