      SUBROUTINE MAPWIN (LUN, BLC, TRC, ERROR)
C-----------------------------------------------------------------------
C! set/reset the window parameters for an open file (in WaWa)
C# IO-WaWa
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   WaWa IO system: Set or reset parameters for a window on MAP i/o
C   File must be opened first with FILOPN.
C   Inputs:
C      LUN     I      Logical Unit No. of file (must be open)
C      BLC     R(7)   Lower bounds of map subrectangle
C      TRC     R(7)   Upper bounds of map subrectangle
C   Outputs:
C      ERROR   I      Error codes: 0 => ok
C                         1 => file not open
C                        10 => catalog error
C                        16 => bad window specification
C                        17 => partial row specified on write.
C-----------------------------------------------------------------------
      INTEGER   LUN, ERROR
      REAL      BLC(7), TRC(7)
C
      REAL      NBLC(7), NTRC(7)
      INTEGER   IERR, NOTOPN, WINER, WRITER, IROUND, I, IFIL
      INCLUDE 'INCS:DITB.INC'
      DATA NOTOPN, WINER, WRITER /1,16,17/
C-----------------------------------------------------------------------
C                                       Is file open?
      ERROR = NOTOPN
      DO 10 IFIL = 6,EFIL
         IF (FILTAB(POLUN,IFIL).EQ.LUN) GO TO 20
 10      CONTINUE
      GO TO 999
C                                       Set IOP<0 to show window
 20   FILTAB(POIOP,IFIL) = -ABS (FILTAB(POIOP,IFIL))
      CALL RCOPY (7, BLC, NBLC)
      CALL RCOPY (7, TRC, NTRC)
C                                       Normalize window
      CALL WINDOW (FILTAB(PODIM,IFIL), FILTAB(PONAX,IFIL), NBLC, NTRC,
     *   IERR)
      ERROR = WINER
      IF (IERR.NE.0) GO TO 999
C                                       Can't window on write in x-dir
      ERROR = WRITER
      IF ((FILTAB(POIOP,IFIL).EQ.-1) .AND. ((NBLC(1).NE.1.0) .OR.
     *   (NTRC(1).NE.FILTAB(PONAX,IFIL)))) GO TO 999
C                                       Copy window into FILTAB
      DO 30 I = 1,7
         FILTAB(POBLC+I-1,IFIL) = IROUND (NBLC(I))
         FILTAB(POTRC+I-1,IFIL) = IROUND (NTRC(I))
 30      CONTINUE
C                                       Initialize line counters
      CALL COPY (6, FILTAB(POBLC+1,IFIL), FILTAB(PODEP,IFIL))
C                                       Compute Block offset to start
C                                       of windowed region
      CALL COMOFF (FILTAB(PODIM,IFIL), FILTAB(PONAX,IFIL),
     *   FILTAB(POBLC+2,IFIL), FILTAB(POBL,IFIL), IERR)
      FILTAB(POBL,IFIL) = 1 + FILTAB(POBL,IFIL)
      ERROR = 0
C
 999  RETURN
      END
