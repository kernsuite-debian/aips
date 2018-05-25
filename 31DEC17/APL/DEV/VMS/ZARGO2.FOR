      SUBROUTINE ZARGO2 (FCB, PNAME, IERR)
C----------------------------------------------------------------------
C! open ARGS TV device
C# Z2 IO-TV
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
C   Opens an ARGS TV device for non-map, single-buffer IO.  Put assigned
C   channel in the appropriate places in the FCB.
C   Inputs:
C      PNAME   C*48   Physical file name: device_name:???
C   Outputs:
C      FCB     I(*)   File control block (part of FTAB).
C      IERR    I      Error return. 0 = success. 1 = failure.
C                        2 = invalid device name.
C                        4 = in use
C----------------------------------------------------------------------
      INTEGER   FCB(*), IERR
      CHARACTER PNAME*48
C
      INTEGER   ILEN, SYS$ASSIGN, JERR
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE '($SSDEF)'
C-----------------------------------------------------------------------
C                                       find device name (before colon)
      ILEN = INDEX (PNAME, ':') - 1
      IF (ILEN.LE.0) THEN
         IERR = 2
C                                       call assign service: arg 2 is
C                                       I*2 - use word swap and 0 upper
      ELSE
         FCB(1+FCBFD) = 0
         JERR = SYS$ASSIGN (PNAME(1:ILEN), FCB(1+FCBFD), , )
         IF (JERR.EQ.1) THEN
            IERR = 0
            FCB(1+FCBERR) = 0
         ELSE
            FCB(1+FCBERR) = JERR
            IERR = 1
            IF (JERR.EQ.SS$_DEVALLOC) IERR = 4
            IF (JERR.EQ.SS$_IVDEVNAM) IERR = 2
            IF (JERR.EQ.SS$_IVLOGNAM) IERR = 2
            IF (JERR.EQ.SS$_NOSUCHDEV) IERR = 2
            FCB(1+FCBFD) = 0
            END IF
         END IF
C
 999  RETURN
      END
