      SUBROUTINE REMKEY (IVOL, CNO, KEYWRD, NUMKEY, BUFFER, IERR)
C-----------------------------------------------------------------------
C! Removes keywords from the keyword section of an AIPS header file
C# Header Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
C;  Associated Universities, Inc. Washington DC, USA.
C;  Eric W. Greisen
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
C   Deletes KEYWORDs from an AIPS image (or uv) header.  The order of
C   the keywords is arbitrary.  Uses LUN 15, so any CA or CB files must
C   be closed before calling this routine.
C   Inputs:
C      IVOL     I          File disk number
C      CNO      I          File catalog block number
C      KEYWRD   C(*)*8     Keywords to read/write: output on ALL
C      NUMKEY   I          Number of keywords to read/write.
C                             Input on OPCODE='ALL' = max to read.
C                             Output on OPCODE='ALL' = # read.
C   Output:
C      BUFFER   I(512)     Scratch buffer - note size
C      IERR     I          Return code, 0=>OK,
C                             1-10 => ZFIO error
C                             19   => unrecognized data type.
C                             20   => bad OPCODE
C                             -n   => n keywords not found.
C-----------------------------------------------------------------------
      CHARACTER KEYWRD(*)*(*)
      INTEGER   IVOL, CNO, NUMKEY, BUFFER(512), IERR
C
      INTEGER   J, JP, CLUN, CIND, MAXREC, HEDKEY, RECNO, GOTIT(100),
     *   II, IO, IREC, OREC
      CHARACTER CTEST*8, PHNAME*48
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA CLUN /15/
C-----------------------------------------------------------------------
      IERR = 0
      IF (NUMKEY.LE.0) GO TO 999
C                                       Open CB file
      CALL ZPHFIL ('CB', IVOL, CNO, 1, PHNAME, IERR)
      CALL ZOPEN (CLUN, CIND, IVOL, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 995
         END IF
      IREC = 2
      CALL ZFIO ('READ', CLUN, CIND, IREC, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1005) IERR, 'READ', IREC
         GO TO 980
         END IF
      MAXREC = BUFFER(1)
      HEDKEY = BUFFER(2)
C                                       quit now if empty
      IF (HEDKEY.LE.0) THEN
         CALL ZCLOSE (CLUN, CIND, IERR)
         IERR = -NUMKEY
         GO TO 999
         END IF
      II = MIN (100, NUMKEY)
      CALL FILL (II, 0, GOTIT)
      OREC = 2
      IO = 0
      CALL FILL (256, 0, BUFFER(257))
      BUFFER(257) = MAXREC
      DO 50 II = 1,HEDKEY
         RECNO = II/51 + 2
         IF (RECNO.GT.IREC) THEN
            IREC = RECNO
            CALL ZFIO ('READ', CLUN, CIND, IREC, BUFFER, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1005) IERR, 'READ', IREC
               GO TO 980
               END IF
            END IF
         JP = 5 * MOD (II,51) + 2
         CALL H2CHR (8, 1, BUFFER(JP), CTEST)
C                                       Search input list
         DO 20 J = 1,NUMKEY
            IF (CTEST.EQ.KEYWRD(J)) THEN
               IF (J.LE.100) GOTIT(J) = GOTIT(J) + 1
               GO TO 50
               END IF
 20         CONTINUE
C                                       Save this word
         IO = IO + 1
         RECNO = IO/51 + 2
         IF (RECNO.NE.OREC) THEN
            CALL ZFIO ('WRIT', CLUN, CIND, OREC, BUFFER(257), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1005) IERR, 'WRIT', OREC
               GO TO 980
               END IF
            CALL FILL (256, 0, BUFFER(257))
            OREC = RECNO
            END IF
         J = 5 * MOD (IO,51) + 2 + 256
         CALL COPY (5, BUFFER(JP), BUFFER(J))
 50      CONTINUE
C                                       Write last rec if needed
      IF (OREC.NE.2) THEN
         CALL ZFIO ('WRIT', CLUN, CIND, OREC, BUFFER(257), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1005) IERR, 'WRIT', OREC
            GO TO 980
            END IF
         OREC = 2
         CALL ZFIO ('READ', CLUN, CIND, OREC, BUFFER(257), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1005) IERR, 'READ', OREC
            GO TO 980
            END IF
         END IF
      BUFFER(258) = IO
      CALL ZFIO ('WRIT', CLUN, CIND, OREC, BUFFER(257), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1005) IERR, 'WRIT', OREC
         GO TO 980
         END IF
C                                       set error return
      II = MIN (100, NUMKEY)
      DO 60 J = 1,II
         IF (GOTIT(J).NE.1) IERR = IERR - 1
 60      CONTINUE
 100  CALL ZCLOSE (CLUN, CIND, II)
      IF (IERR.EQ.0) IERR = II
      IF (IERR.GT.0) GO TO 995
      GO TO 999
C                                       Error
 980  CALL MSGWRT (8)
      CALL ZCLOSE (CLUN, CIND, II)
C
 995  WRITE (MSGTXT,1995) IVOL, CNO
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REMKEY: ERROR',I5,' OPENING THE HEADER FILE')
 1005 FORMAT ('REMKEY: ERROR',I5,1X,A4,'ING THE HEADER FILE RECORD',I5)
 1995 FORMAT ('REMKEY: ERROR ON VOL,CNO',I3,I6)
      END
