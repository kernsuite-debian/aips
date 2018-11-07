      SUBROUTINE TSKEND (IRET)
C-----------------------------------------------------------------------
C! closes down a task and its files in the WaWa system
C# IO-WaWa
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
C   WaWa IO system: Terminate a task, including calls to CLENUP and
C   RELPOP if appropriate.  Also close down messages.
C   Inputs:
C      IRET      I      A return code passed to AIPS if task was run
C                       in wait mode: 0 => ok, else => failure.
C   Written by WaWa (last rev 4/21/81).
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I, IERR, INEXT
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Clear cat, destroy scratch file
      CALL CLENUP
C                                       If error delete all files
C                                       created by MAPCR.  They are
C                                       probably messed up.
      IF ((IRET.EQ.0) .OR. (NCFILE.LE.0)) GO TO 20
         DO 10 I = 1,NCFILE
            IF (FRW(I).EQ.2) CALL MDESTR (FVOL(I), FCNO(I), CATBLK,
     *         WBUFF, INEXT, IERR)
 10         CONTINUE
C                                       Close down message
C                                       Resume AIPS if needed
 20   RQUICK = QUACK.NE.0
      CALL DIETSK (IRET, RQUICK, WBUFF)
C
 999  RETURN
      END
