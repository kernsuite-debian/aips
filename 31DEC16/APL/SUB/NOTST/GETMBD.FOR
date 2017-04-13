      SUBROUTINE GETMBD (TIME, SNRECR, NUMIF, SNTIME, MBCOL, RATCOL,
     *   MBDELY)
C-----------------------------------------------------------------------
C! Corrects multiband delays for fringe rate in SN table record.
C# EXT-appl UV Calibration
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
C   Corrects multiband delays for fringe rate in SN table record.
C   Inputs:
C      TIME   D    Time at which the multiband delay is desired (days)
C      SNRECR R(*) SN table record.
C      NUMIF  I    Number of IFs.
C      SNTIME D    Time stamp of SN record.
C      MBCOL  I    Pointer in SNRECR for multi band delay.
C      RATCOL I    Pointer in SNREC for rates.
C   Output:
C      MBDELY R    Multi band delay at TIME.
C-----------------------------------------------------------------------
      DOUBLE PRECISION TIME, SNTIME
      REAL      SNRECR(*), MBDELY
      INTEGER   NUMIF, MBCOL, RATCOL
C
      INTEGER   LOOP, COUNT
      REAL      RATSUM, RATE
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Get value in SN record.
      MBDELY = SNRECR(MBCOL)
C                                       Determine average rate.
      RATSUM = 0.0
      RATE = 0.0
      COUNT = 0
      DO 100 LOOP = 1,NUMIF
         IF (SNRECR(RATCOL+LOOP-1).NE.FBLANK) THEN
            COUNT = COUNT + 1
            RATSUM = RATSUM + SNRECR(RATCOL+LOOP-1)
            END IF
 100     CONTINUE
      IF (COUNT.GT.0) RATE = RATSUM / COUNT
C                                       Correct for average rate
      MBDELY = MBDELY + RATE * ((TIME - SNTIME) * 86400.0D0)
C
 999  RETURN
      END
