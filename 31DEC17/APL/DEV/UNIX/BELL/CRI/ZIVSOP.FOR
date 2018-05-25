      SUBROUTINE ZIVSOP (LUN, IND, IERR)
C-----------------------------------------------------------------------
C! opens IVAS TV device - using the IIS package
C# TV-IO
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
C   ZIVSOP opens the IVAS TV device use the IIS software package.  It
C   does make an entry in the AIPS FTAB, but never really uses it.
C   Inputs:
C      LUN    I   FORTRAN logical file number.
C   Output:
C      IND    I   Index into FTAB for the file control block.
C      IERR   I   Error return code: 0 = no error
C                    1 = LUN already in use
C                    2 = file not found
C                    3 = volume not found
C                    4 = excl requested but not available
C                    5 = no room for lun
C                    6 = other open errors
C   UNICOS version - translates logical TVDEVn (n = NTVDEV), calls
C   FIVASOPEN (name too long for UNICOS).
C-----------------------------------------------------------------------
      INTEGER   LUN, IND, IERR
C
      INTEGER   ERRLUN, ERRTER, IER, LLEN, XLEN, XLNB, JERR, I, J,
     *   FIVASOPEN
      LOGICAL   F
      CHARACTER LOGNAM*6, XLATED*128
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA F /.FALSE./
      DATA ERRLUN, ERRTER /12, 6/
C-----------------------------------------------------------------------
      IERR = 6
      IF ((LUN.EQ.ERRLUN) .OR. (LUN.EQ.ERRTER)) GO TO 980
      IF ((LUN.LE.0) .OR. (LUN.GT.50)) GO TO 980
      IF (DEVTAB(LUN).NE.4) GO TO 970
      IF ((NTVDEV.LE.0) .OR. (NTVDEV.GT.15)) GO TO 975
      IERR = 0
C                                       allocate table
      CALL LSERCH ('OPEN', LUN, IND, F, IER)
      IF (IER.EQ.0) GO TO 20
         IF (IER.NE.2) GO TO 10
            IERR = 1
            WRITE (MSGTXT,1000) LUN
            GO TO 990
 10      IF (IER.NE.3) GO TO 20
            IERR = 5
            WRITE (MSGTXT,1010) LUN
            GO TO 990
C                                       Form logical device name and
C                                       translate it.
 20   LOGNAM = 'TVDEV  '
      CALL ZEHEX (NTVDEV, 2, LOGNAM(6:7))
      LLEN = LEN (LOGNAM)
      XLEN = LEN (XLATED)
      CALL ZTRLOG (LLEN, LOGNAM, XLEN, XLATED, XLNB, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IERR
         IERR = 3
         GO TO 900
C                                       Call IVAS library open.
 30   XLATED(XLNB+1:XLNB+1) = CHAR (0)
C      JERR = FIVASOPEN (YBUFF(11), XLATED, -1)
      IF (JERR.NE.0) THEN
         IERR = 6
         WRITE (MSGTXT,1030) JERR, LUN
C                                       store file name
      ELSE
         CALL FSERCH (IND, I, J, JERR)
         IF (JERR.EQ.0) THEN
            IF (I.EQ.1) DEVNAM(J) = XLATED(1:48)
            IF (I.EQ.2) NONNAM(J) = XLATED(1:48)
            IF (I.EQ.3) MAPNAM(J) = XLATED(1:48)
            END IF
         GO TO 999
         END IF
C                                       Errors
C                                       remove FTAB
 900  CALL LSERCH ('CLOS', LUN, IND, F, IER)
      IF ((IERR.EQ.3) .OR. (IERR.EQ.6)) GO TO 990
C                                       illegal device
 970  WRITE (MSGTXT,1970) LUN
      GO TO 990
C                                       illegal TV number
 975  WRITE (MSGTXT,1975) NTVDEV
      GO TO 990
C                                       illegal lun
 980  WRITE (MSGTXT,1980) LUN
C                                       print msg
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZIVSOP: LUN',I4,' ALREADY IN USE')
 1010 FORMAT ('ZIVSOP: LUN TABLE FULL, CAN''T FIT LUN',I4)
 1020 FORMAT ('ZIVSOP: TRANSLATE LOGICAL ERROR ',I4)
 1030 FORMAT ('ZIVSOP: FIVASOPEN ERROR',I12,' LUN',I3)
 1970 FORMAT ('ZIVSOP: CALLED FOR NON-TV DEVICE WITH LUN',I4)
 1975 FORMAT ('ZIVSOP: CALLED FOR ILLEGAL TV DEVICE NUMBER',I4)
 1980 FORMAT ('ZIVSOP: LUN',I6,' ILLEGAL FOR TV')
      END
