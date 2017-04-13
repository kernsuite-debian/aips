      SUBROUTINE TINBEG (PRGNAM, NPARM, RPARM, ERROR)
C-----------------------------------------------------------------------
C! interactive task start up operations (common inits, GTPARM) for WaWa
C# IO-WaWa
C-----------------------------------------------------------------------
C;  Copyright (C) 2011, 2015
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
C   WaWa IO package: Do most of the operations necessary to begin a
C   task:  Calls IOSET, calls GTPARM to get parameters, and makes the
C   task not resume AIPS until the end.  For <= 5 simultaneously open
C   map files.   You should end with TSKEND..
C   Inputs:
C      PRGNAM   C*6    Task name
C      NPARM    I      No. of real parameters passed by AIPS
C   OUTPUTS:
C      RPARM    R(*)   Array to receive passed parameters
C      ERROR    I      Error return: 0 => Okay
C                         0 < ERROR <10  => Error return from GTPARM
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*(*)
      INTEGER   NPARM, ERROR
      REAL      RPARM(*)
C
      INTEGER   IRET
      LOGICAL   RQUICK
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DITB.INC'
C-----------------------------------------------------------------------
C                                       Set WaWa IO tables
      TSKNAM = PRGNAM
      CALL IOSET
C                                       Get task parms from AIPS
      CALL GTPARM (PRGNAM, NPARM, RQUICK, RPARM, WBUFF, ERROR)
      RQUICK = .FALSE.
      IRET = 0
      IF (ERROR.NE.0) IRET = 16
C                                       Pass RQUICK via integer
      QUACK = 0
C
 999  RETURN
      END
