      SUBROUTINE IMWIN (WIN, CHAN, ERROR)
C-----------------------------------------------------------------------
C! Set up window on TV device
C# TV IO-wawa
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
C   Set up window on TV device
C   Inputs:
C      WIN(4)   I      Corners of image on TV
C      CHAN     I      Which channels to read, write
C   Outputs:
C      IERR     I
C-----------------------------------------------------------------------
      INTEGER   WIN(4), CHAN, ERROR
      INTEGER   NOTOPN, IFIL, I
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      DATA NOTOPN /1/
C-----------------------------------------------------------------------
      ERROR = NOTOPN
      DO 10 IFIL = 6, EFIL
         IF (FILTAB(POLUN,IFIL).EQ.TVLUN) GO TO 20
 10      CONTINUE
      GO TO 999
C
 20   FILTAB (POIOP, IFIL) = -ABS(FILTAB(POIOP, IFIL))
      FILTAB (PODIM, IFIL) = 2
      FILTAB (POVOL, IFIL) = CHAN
      I = PODEP - PONAX + 7
      CALL FILL (I, 1, FILTAB(PONAX, IFIL))
      CALL COPY (2, WIN, FILTAB(POBLC, IFIL))
      CALL COPY (2, WIN, FILTAB(PODEP, IFIL))
      CALL COPY (2, MAXXTV, FILTAB(PONAX, IFIL))
      CALL COPY (2, WIN(3), FILTAB(POTRC, IFIL))
      ERROR = 0
C
 999  RETURN
      END
