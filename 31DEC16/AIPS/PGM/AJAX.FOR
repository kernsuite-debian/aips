      PROGRAM AJAX
C-----------------------------------------------------------------------
C! AJAX destroys AIPS scratch files.
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   AJAX destroys a standard AIPS scratch files for all users except
C   if a task by the name and AIPS number of the scratch file is now
C   active.
C-----------------------------------------------------------------------
      CHARACTER PHNAME*48, PRGNAM*6
      INTEGER   SCR(256), IERR, IVOL, IUS, IUS1, IUS2, IWORK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGNAM /'AJAX  '/
C-----------------------------------------------------------------------
      TSKNAM = PRGNAM
      CALL ZDCHIN (.TRUE.)
      CALL ZMYVER
      CALL ACOUNT (1)
      IUS1 = 1
      IUS2 = 1
      IF (UCTSIZ.GT.0) IUS2 = USELIM
C                                       Do it: destroy all
 100  DO 130 IUS = IUS1,IUS2
         DO 110 IVOL = 1,NVOL
            NLUSER = IUS
            CALL ZPHFIL ('CA', IVOL, 0, 0, PHNAME, IERR)
            CALL ZEXIST (IVOL, PHNAME, IWORK, IERR)
            IF (IERR.EQ.0) GO TO 120
               IF (IERR.EQ.1) GO TO 110
                  NLUSER = 1
                  WRITE (MSGTXT,1100) IUS, IVOL, IERR
                  CALL MSGWRT (6)
 110           CONTINUE
            GO TO 130
C                                       Do it: this user has some
 120        NLUSER = 1
            WRITE (MSGTXT,1120) IUS
            CALL MSGWRT (2)
            NLUSER = IUS
            CALL DESCR (SCR)
 130     CONTINUE
C
 990  CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1100 FORMAT ('USER, VOL =',2I6,' ZEXIST (CA) ERROR=',I6)
 1120 FORMAT ('Checking on user number',I5)
      END
