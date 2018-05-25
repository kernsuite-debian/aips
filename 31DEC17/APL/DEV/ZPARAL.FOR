      SUBROUTINE ZPARAL (NPROC)
C-----------------------------------------------------------------------
C! Set and report on number of processors to use for parallel tasks
C# Z General
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
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
C   If NPROC is positive then set the number of threads of execution to
C   be NPROC for subsequent tasks.  Set NPROC to the actual number of
C   threads of execution regardless of its initial value.
C
C   Input/output:
C      NPROC     I       On entry: the number of threads/processors
C                                  requested
C                        On exit: the actual number of threads that
C                                 will be used
C
C   Generic version: parallelism not supported.
C-----------------------------------------------------------------------
      INTEGER   NPROC
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C   Porting hints
C   =============
C
C   Many parallel implementations of FORTRAN use an environment variable
C   to control the number of threads or processors that will be used.
C   In cases where parallel programs must be run under a special control
C   program, one approach is to set an environment variable to the
C   value of NPROC and test for this in ZACTV8 or ZACTV9.
C-----------------------------------------------------------------------
      MSGTXT = 'PARALLELISM NOT SUPPORTED IN THIS INSTALLATION'
      CALL MSGWRT (6)
      NPROC = 1
C
 999  RETURN
      END
