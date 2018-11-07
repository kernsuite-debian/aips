      SUBROUTINE RNXGET (DISK, CNO, CATBLK)
C-----------------------------------------------------------------------
C! Make list of time intervals in an NX table
C# UV EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2011
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
C   Inits the pre-existing NX info for use by RNXINI and other auto-NX
C   creation and filling routines.
C   Inputs:
C      DISK     I      Disk number of the input UV data set
C      CNO      I      Catalog number of the input UV data set
C      CATBLK   I(*)   Header of the input UV data set
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(256)
C
      INTEGER   LUN, LUNTMP, IERR, VER, NSUB, ISUB
      LOGICAL   TABLE, EXIST, FITASC
      INCLUDE 'INCS:DRNX.INC'
C-----------------------------------------------------------------------
      LUN = LUNTMP (1)
C                                       exist?
      CALL FNDEXT ('AN', CATBLK, NSUB)
      VER = 1
      CALL ISTAB ('NX', DISK, CNO, VER, LUN, RNXBUF, TABLE, EXIST,
     *   FITASC, IERR)
      IF ((IERR.NE.0) .OR. (.NOT.TABLE) .OR. (.NOT.EXIST) .OR.
     *   (NSUB.LE.0)) THEN
         CALL FILL (MAXSUB, 0, RNXNOS)
      ELSE
         DO 20 ISUB = 1,NSUB
            IF (ISUB.EQ.1) THEN
               RNXFIR(ISUB) = 1
            ELSE
               RNXFIR(ISUB) = RNXFIR(ISUB-1) + RNXNOS(ISUB-1)
               END IF
            CALL GETNX (LUN, DISK, CNO, CATBLK, ISUB, RNXBUF,
     *         RNXNOS(ISUB), RNXTSC(RNXFIR(ISUB)))
 20         CONTINUE
         END IF
C
 999  RETURN
      END
