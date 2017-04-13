      SUBROUTINE TXRGBL (X0, Y0, BLC, TRC, LEVTXT, RGBLEV, GBUFF, IERR)
C-----------------------------------------------------------------------
C! writes characters in colors to a plot string to describe RGBLEV
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2006
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
C   TXRGBL draws the RGBLEVs string in suitable colors in a plot file
C   Inputs:
C      X0       R        Start BLC X position of string
C      Y0       R        Start BLC Y position of string
C      BLC      R(2)     BLC of full plot area
C      TRC      R(2)     TRC of full plot area
C      LEVTXT   C*(*)    Text string containing blank-separated LEVS
C      RGBLEV   R(3,*)   Colors of each LEVS
C   In/out
C      GBUFF    I(*)     Plot buffer
C   Outputs:
C      IERR     I        Error code
C-----------------------------------------------------------------------
      INTEGER   GBUFF(256), IERR
      REAL      X0, Y0, BLC(2), TRC(2), RGBLEV(3,*)
      CHARACTER LEVTXT*(*)
C
      INTEGER   NC, ITRIM, LP, IP, LC
      REAL      CS, X
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       decide scale
      IERR = 0
      X = X0
      NC = ITRIM (LEVTXT)
      IF (NC.LE.1) GO TO 999
      CS = 0.009 * (TRC(1)-BLC(1))
 10   IF (X0+CS*1.4*NC.GT.TRC(1)-3.*CS) THEN
         CS = 0.8 * CS
         GO TO 10
         END IF
C                                       precursor section
      IP = INDEX (LEVTXT, '(')
      IF (IP.GT.0) THEN
         CALL G3VCOL (0.7, 0.7, 0.1, GBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (1000,MSGTXT) 'G3VCOL', IERR
            GO TO 990
            END IF
         CALL GCHDRW (X, Y0, CS, LEVTXT(:IP), 1, GBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (1000,MSGTXT) 'GCHDRW', IERR
            GO TO 990
            END IF
         END IF
      LP = IP
C                                       parse for string
      LC = 0
 15   IP = LP
 20      IP = IP + 1
         IF (IP.GT.NC) GO TO 999
         IF ((LEVTXT(IP:IP).NE.' ') .AND. (LEVTXT(IP:IP).NE.')'))
     *      GO TO 20
C                                       got one set color
            LC = LC + 1
            CALL G3VCOL (RGBLEV(1,LC), RGBLEV(2,LC), RGBLEV(3,LC),
     *         GBUFF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (1000,MSGTXT) 'G3VCOL', IERR
               GO TO 990
               END IF
            LP = LP + 1
            IF (LEVTXT(IP:IP+1).EQ.'  ') IP = IP + 1
            CALL GCHDRW (X, Y0, CS, LEVTXT(LP:IP), 1, GBUFF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (1000,MSGTXT) 'GCHDRW', IERR
               GO TO 990
               END IF
            LP = IP
            GO TO 15
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TXRGBL: ',A,' RETURNS ERROR CODE',I4)
      END
