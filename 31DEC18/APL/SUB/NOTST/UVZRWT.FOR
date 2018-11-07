      SUBROUTINE UVZRWT( UIN, VIN, BEAM, ZEROSP, TFLUXG, FLUXZ, WEIGHT)
C-----------------------------------------------------------------------
C!  Puts Zero spacing flux and weight into a UV Buffer.
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
C UV ZeRo SPace: Puts Zero spacing flux and weight into a UV Buffer.
C Band width synthesis is handled by this routine
C Inputs:
C  UIN        R     Location of UV point in lambda
C  VIN        R     Location of UV point in lambda
C  BEAM       L     True if calculating beam zero spacing value
C  ZEROSP     R(5)  Zero Spacing Flux, major axis, minor axis, angle and
C                   weight
C  TFLUXG     R     Total Gridded Flux already subtracted from UV-data
C Output:
C  FLUXZ      R     Flux for zero spacing for Gaussian at UIN, VIN
C  WEIGHT     R     Weight of Zero Spacing Flux
C  BUFF       R(*)  Visibility record
C-----------------------------------------------------------------------
      REAL      UIN, VIN, ZEROSP(5), TFLUXG, FLUXZ, WEIGHT
      LOGICAL   BEAM
C
      REAL      SINT, COST, THETA, DU, DV
      INTEGER   I
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Always do u=v=0
      FLUXZ = 1.
C                                       if U or V not zero
      IF ((ABS(UIN).GT.0.001) .OR. (ABS(VIN).GT.0.001)) THEN
C                                       Assume Flux is zero
         FLUXZ = 0.0
C                                       Extemded
         IF ((ZEROSP(2).GE.1.0E-6) .AND. (ZEROSP(3).GE.1.0E-6)) THEN
C                                       rotate and covert to radians
            THETA = (90. + ZEROSP(4)) / 57.29577
            SINT  = SIN(THETA)
            COST  = COS(THETA)
C                                       rotate In u and v
            DU = (UIN*COST) - (VIN*SINT)
            DV = (VIN*COST) + (UIN*SINT)
C                                       decrease gaussian weight
            FLUXZ = (DU*ZEROSP(2))**2 + (DV*ZEROSP(3))**2
C                                       convert from FWHM to sigma
C                                       2.3548 = 2 * sqrt(2*ln(2))
C           FLUXZ = FLUXZ*(2.3548**2)
C                                       convert from arcsec to radians
C           FLUXZ = EXP(-FLUXZ/(206264.81**2))
            FLUXZ = EXP (-FLUXZ / (87592.6**2))
            END IF
C                                       end if U not 0 or V not 0
         END IF
C                                       Use input weight,
      WEIGHT = ZEROSP(5)
C                                       Do not use insignificant points
      IF (FLUXZ.LT.1E-3) WEIGHT = 0.0
C                                       if making beam
      IF (BEAM) THEN
C                                       tell once about zero spacing
         IF ((VIN.EQ.0.0) .AND. (UIN.EQ.0.0)) THEN
            WRITE (MSGTXT,1250) ZEROSP(1), ZEROSP(5)
            CALL MSGWRT (3)
C                                       major axis in arc seconds
            IF ((ZEROSP(2).GE.1.0E-6) .AND. (ZEROSP(3).GE.1.0E-6)) THEN
               WRITE (MSGTXT,1260) (ZEROSP(I), I=2,4)
               CALL MSGWRT (3)
               END IF
            END IF
C                                       Tell user about Zero Spacing
C         WRITE (MSGTXT,1240,ERR=999) UIN, VIN, FLUXZ*ZEROSP(1)
C         IF ((WEIGHT.GT.0.0) .AND. (UIN.GE.VIN) .AND.
C     *      (VIN.GE.0.0) .AND. (UIN.GT.0.0)) CALL MSGWRT(2)
C                                       Beam Flux is 1 - gridded flux
         FLUXZ  = (1.0 - TFLUXG)
C                                       else normal map use input flux
      ELSE
         FLUXZ = (ZEROSP(1) - TFLUXG) * FLUXZ
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1240 FORMAT ('UVZRWT: Cell ',1PE10.3,',',E10.3,' lambdas, Flux ',E10.3)
 1250 FORMAT ('UVZRWT: Zero spacing ',1PE10.3,', Weight ',E10.3)
 1260 FORMAT ('UVZRWT: Maj, Min Ax. ',1PE10.3,'",',E10.3,
     *        '", Angle ',0PF6.1,' Deg')
      END
