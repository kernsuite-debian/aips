      SUBROUTINE ANMAXA (IUDISK, IUCNO, IUVER, CATBLK, MAXA, IERR)
C-----------------------------------------------------------------------
C! Find max antenna number in an antenna file
C# EXT-util UV VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2009
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
C   finds the max antenna number in an antenna file - this may easily
C   not be the same as the numbert of records
C   Inputs:
C      IUDISK   I        Disk number
C      IUCNO    I        Catalog number
C      IUVER    I        AN version number = subarray number
C      CATBLK   I(256)   Catalog block
C   Output:
C      MAXA     I        Max antenna number
C      IERR     I        Error code
C   Output in Common: DANT.INC after ANTINI and several TABAN calls
C-----------------------------------------------------------------------
      INTEGER   IUDISK, IUCNO, IUVER, CATBLK(256), MAXA, IERR
C
      INTEGER   NUMSUB, IA, ANBUFF(512), ILUN, NANT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA ILUN /110/
C-----------------------------------------------------------------------
C                                       is there a question
      MAXA = 0
      IERR = 0
      CALL FNDEXT ('AN', CATBLK, NUMSUB)
C                                       yes: compare #1 with each
      IF ((NUMSUB.GE.IUVER) .AND. (IUVER.GE.1)) THEN
         IA = 0
         CALL ANTINI ('READ', ANBUFF, IUDISK, IUCNO, IUVER, CATBLK,
     *      ILUN, IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY,
     *      SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME,
     *      XYZHAN, TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'INIT', IUVER
            GO TO 980
            END IF
         NANT = ANBUFF(5)
C                                       read the an tables
         DO 90 IA = 1,NANT
            CALL TABAN ('READ', ANBUFF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA,POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ', IUVER
               GO TO 970
               END IF
            MAXA = MAX (MAXA, NOSTA)
 90         CONTINUE
         CALL TABIO ('CLOS', 0, IANRNO, ANBUFF, ANBUFF, IERR)
         END IF
      GO TO 999
C
 970  CALL MSGWRT (6)
      CALL TABIO ('CLOS', 0, IANRNO, ANBUFF, ANBUFF, IA)
      GO TO 999
C
 980  CALL MSGWRT (6)
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANMAXA: ERROR',I5,1X,A,'ING AN TABLE',I4)
      END
