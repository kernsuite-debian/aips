      SUBROUTINE ZFI2 (OPER, FCB, BUFF, NREC, MSGBUF, IERR)
C-----------------------------------------------------------------------
C! read/write one 256-integer record from/to a non-map disk file
C# Z2 IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2004
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
C   Transfer one 256 local integer record between an I/O buffer and
C   disk starting at random access record "nrec" using "non-map" I/O
C   (i.e., single buffered, synchronous I/O).
C   Inputs:
C      OPER   H*4      Operation code "READ" or "WRIT"
C                      CHARACTER on VMS, HOLLERITH otherwise
C      FCB    I(*)     File control block for opened non-map disk file
C      NREC   I        Disk record number (1-relative, units of
C                      256-integer records)
C   In/out:
C      BUFF   I(256)   I/O buffer
C   Output:
C      MSGBUF H*80     extra error message (null terminated)
C                      CHARACTER on VMS, HOLLERITH otherwise
C      IERR   I        Error return code: 0 => no error
C                         2 => bad opcode
C                         3 => I/O error
C                         4 => end of file
C  Generic version - stub
C--------------------------------------------------------------------
      HOLLERITH OPER, MSGBUF(*)
      INTEGER   FCB, NREC, BUFF(256), IERR
C
      CHARACTER CMSGB*80
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CMSGB = 'ZFI2: REQUIRES LOCAL DEVELOPMENT'
      CALL CHR2H (80, CMSGB, 1, MSGBUF)
      IERR = 2
C
 999  RETURN
      END
