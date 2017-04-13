      SUBROUTINE COLORL (NLEVS, ICOLR, NBRK, GAMMA, BUFFER, IERR)
C-----------------------------------------------------------------------
C! continuous colors fom blue thru green to red (or rotations thereof)
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   COLORL creates a continuous coloring from blue thru green to red.
C   Inputs:
C      NLEVS   I      # of intensities (usually 256 or 1024)
C      ICOLR   I      initial color R,G,B = 1,2,3
C                     < 0 => go RBG order instead
C      NBRK    I      break point between blue & red
C      GAMMA   R      gamma correction power
C   Output:
C      BUFFER  R(*)   scratch buffer
C      IERR    I      error code of ZM70XF: 0 - ok
C-----------------------------------------------------------------------
      INTEGER   NLEVS, ICOLR, NBRK, IERR
      REAL      GAMMA, BUFFER(*)
C
      INTEGER   NMX, NBR, KOLOR, NEND(2), ICI, INC, JERR
      REAL      GAMINV, SLOPE(2), OFFSET(2), GO(2,3), GP(2,3), FO, FP,
     *   TF
      LOGICAL   WASYNC
      INCLUDE 'INCS:DTVC.INC'
      DATA GO / 1., 28.,  8., 63., 26., 64./
      DATA GP /10., 16., 21., 35., 39., 65./
C-----------------------------------------------------------------------
      GAMINV = GAMMA
      IF (GAMINV.LE.0.0) GAMINV = 1.0
      GAMINV = 1.0 / GAMINV
      IF (NLEVS.GT.OFMINP+1) NLEVS = OFMINP + 1
      NMX = MAX (3, NLEVS)
      NBR = MAX (2, MIN (NMX-1, NBRK))
      TF = REAL (NBR) / REAL(NLEVS)
      NEND(1) = NBR
      NEND(2) = NMX
      ICI = ABS (ICOLR)
      INC = -1
      IF (ICOLR.LT.0) INC = 1
      IF ((ICI.LT.1) .OR. (ICI.GT.3)) ICI = 3
      WASYNC = ISYNCH.EQ.0
      IF (WASYNC) CALL YHOLD ('ONNN', JERR)
C                                        first color (blue)
      KOLOR = 2 ** (3-ICI)
      FO = GO(1,1) / 64.0
      FP = GP(1,1) / 64.0
      FO = FO + (4.*FO-1.)*(NLEVS-1.)*TF + (2.-4.*FO)*(NLEVS-1.)*TF*TF
      FP = FP + (4.*FP-1.)*(NLEVS-1.)*TF + (2.-4.*FP)*(NLEVS-1.)*TF*TF
      IF (FP.EQ.FO) FP = FP + 1
      SLOPE(1) = 1.0 / (FP - FO)
      OFFSET(1) = FO / (FO - FP)
      NEND(1) = FP
      FO = GO(2,1) / 64.0
      FP = GP(2,1) / 64.0
      FO = FO + (4.*FO-1.)*(NLEVS-1.)*TF + (2.-4.*FO)*(NLEVS-1.)*TF*TF
      FP = FP + (4.*FP-1.)*(NLEVS-1.)*TF + (2.-4.*FP)*(NLEVS-1.)*TF*TF
      IF (FP.EQ.FO) FP = FP + 1
      SLOPE(2) = 1.0 / (FP - FO)
      OFFSET(2) = FO / (FO - FP)
      CALL ILNCLR (KOLOR, 2, NEND, SLOPE, OFFSET, GAMINV, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        second color (green)
      ICI = ICI + INC
      IF (ICI.EQ.4) ICI = 1
      IF (ICI.EQ.0) ICI = 3
      KOLOR = 2 ** (3-ICI)
      FO = GO(1,2) / 64.0
      FP = GP(1,2) / 64.0
      FO = FO + (4.*FO-1.)*(NLEVS-1.)*TF + (2.-4.*FO)*(NLEVS-1.)*TF*TF
      FP = FP + (4.*FP-1.)*(NLEVS-1.)*TF + (2.-4.*FP)*(NLEVS-1.)*TF*TF
      IF (FP.EQ.FO) FP = FP + 1
      SLOPE(1) = 1.0 / (FP - FO)
      OFFSET(1) = FO / (FO - FP)
      NEND(1) = FP
      FO = GO(2,2) / 64.0
      FP = GP(2,2) / 64.0
      FO = FO + (4.*FO-1.)*(NLEVS-1.)*TF + (2.-4.*FO)*(NLEVS-1.)*TF*TF
      FP = FP + (4.*FP-1.)*(NLEVS-1.)*TF + (2.-4.*FP)*(NLEVS-1.)*TF*TF
      IF (FP.EQ.FO) FP = FP + 1
      SLOPE(2) = 1.0 / (FP - FO)
      OFFSET(2) = FO / (FO - FP)
      CALL ILNCLR (KOLOR, 2, NEND, SLOPE, OFFSET, GAMINV, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        third color (red)
      ICI = ICI + INC
      IF (ICI.EQ.4) ICI = 1
      IF (ICI.EQ.0) ICI = 3
      KOLOR = 2 ** (3-ICI)
      FO = GO(1,3) / 64.0
      FP = GP(1,3) / 64.0
      FO = FO + (4.*FO-1.)*(NLEVS-1.)*TF + (2.-4.*FO)*(NLEVS-1.)*TF*TF
      FP = FP + (4.*FP-1.)*(NLEVS-1.)*TF + (2.-4.*FP)*(NLEVS-1.)*TF*TF
      IF (FP.EQ.FO) FP = FP + 1
      SLOPE(1) = 1.0 / (FP - FO)
      OFFSET(1) = FO / (FO - FP)
      NEND(1) = NMX
      CALL ILNCLR (KOLOR, 1, NEND, SLOPE, OFFSET, GAMINV, BUFFER, IERR)
C
 990  IF (WASYNC) CALL YHOLD ('OFFF', JERR)
C
 999  RETURN
      END
