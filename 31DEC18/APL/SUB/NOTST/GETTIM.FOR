      SUBROUTINE GETTIM (DISK, CNO, BUFFTM, CATBLK, YEAR, RDAY, IATUT,
     *   IRET)
C-----------------------------------------------------------------------
C! Determine time info from AN table
C# Table Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2001, 2009
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
C   Routine to determine the year and day number of observation from the
C   antenna table.
C   Inputs:
C      DISK     I      Disk # containing AN table
C      CNO      I      Cat. #
C      BUFFTM   I(*)   Work buffer
C      CATBLK   I(*)   Catalogue header
C   Outputs:
C      YEAR     I      Year
C      RDAY     I      Reference day of observation
C      IATUT    R      data time - UTC time difference (secs)
C      IRET     I      Error, = 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, BUFFTM(*), CATBLK(*), YEAR, RDAY, IRET
      REAL      IATUT
C
      INTEGER   IDD, IMM, IVER, LUN, ID(3)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUN /40/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Read AN table
      IVER = 0
      CALL ANTINI ('READ', BUFFTM, DISK, CNO, IVER, CATBLK, LUN, IANRNO,
     *   ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE, POLRXY,
     *   UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB, NOPCAL,
     *   ANFQID, ANTNIF, IRET)
      IATUT = DATUTC
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Decode date
      CALL DATEST (RDATE, ID)
      YEAR = ID(1)
      IMM = ID(2)
      IDD = ID(3)
C                                       Determine day number
      CALL DAYNUM (YEAR, IDD, IMM, RDAY)
C                                       This should no longer be
C                                       necessary:
CC                                       Correct IATUTC if = 0
C      IF (IATUT.EQ.0.0) THEN
C         CALL JULDAY (RDATE, JD)
C         TJD = JD - 2400000.5D0
C         IATUT = TAI (TJD)
C         END IF
C                                       Close AN extension file
      CALL TABIO ('CLOS', 1, IANRNO, BUFFTM, BUFFTM, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1200) IRET
         GO TO 990
         END IF
      GO TO 999
C                                       Write error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETTIM: ERROR',I3,' OPEN-FOR READ AN FILE')
 1200 FORMAT ('GETTIM: ERROR',I3,' CLOSING AN FILE')
      END
