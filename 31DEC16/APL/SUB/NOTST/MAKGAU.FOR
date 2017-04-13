      SUBROUTINE MAKGAU (OUT, IERR)
C-----------------------------------------------------------------------
C! Tim Cornwell routine: Make a Gaussian convolution function.
C# IO-appl Math Map
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
C   MAKGAU make a Gaussian convolution function
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      INTEGER   OUT, JLIM
      REAL      RFACT, U, V, R, RMAX, COSPA, SINPA, RNORM, DEGRAD, DU,
     *   DV
      INTEGER   AKOPEN, AKCESS, AKCLOS
      INTEGER   IU, IV
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA DEGRAD /57.29577951/
C-----------------------------------------------------------------------
      IERR = 0
C
      RMAX = 30.0
      COSPA = COS ((BPA+90.0)/DEGRAD)
      SINPA = SIN ((BPA+90.0)/DEGRAD)
      RFACT = 4.0*(ATAN (1.0))**2/LOG (2.0)
      DU = 1.0 / (CELLX * FLOAT(NX))
      DV = 1.0 / (CELLY * FLOAT(NY))
      RNORM = 1.1331 * BMAJ * BMIN * ABS (DU) * ABS(DV)
C                                       Open file
      IF (AKOPEN (OUT, 1, 'WRIT', BUFFR1).NE.0) GO TO 990
C                                       Begin loop thru map.
      JLIM = NX / 2 + 1
      DO 125 IU = 1,JLIM
         IF (AKCESS (OUT, BUFFR1).NE.0) GO TO 990
         I1 = BIND(1)
      INCLUDE 'INCS:ZVND.INC'
         DO 120 IV = 1,NY/2
            U = DU * COSPA * REAL (IU-1) - DV * SINPA * REAL (IV-1)
            V = DU * SINPA * REAL (IU-1) + DV * COSPA * REAL (IV-1)
            R = RFACT * ((BMAJ*U)**2 + (BMIN*V)**2)
            IF (R.GT.RMAX) THEN
               BUFFR1(I1) = 0.0
            ELSE
               BUFFR1(I1)   = RNORM * EXP (-R)
               END IF
            BUFFR1(I1+1) = 0.0
            I1 = I1 + 2
 120        CONTINUE
      INCLUDE 'INCS:ZVND.INC'
         DO 121 IV = NY/2+1, NY
            U = DU * COSPA * REAL (IU-1) - DV * SINPA * REAL (IV-NY)
            V = DU * SINPA * REAL (IU-1) + DV * COSPA * REAL (IV-NY)
            R = RFACT * ((BMAJ*U)**2 + (BMIN*V)**2)
            IF (R.GT.RMAX) THEN
               BUFFR1(I1) = 0.0
            ELSE
               BUFFR1(I1)   = RNORM * EXP (-R)
               END IF
            BUFFR1(I1+1) = 0.0
            I1 = I1 + 2
 121        CONTINUE
 125     CONTINUE
      IF (AKCLOS (OUT, BUFFR1).NE.0) GO TO 990
C
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1010)
      CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('MAKGAU')
      END
