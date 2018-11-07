      SUBROUTINE EXTCOP (TYPE, INVER, OUTVER, LUNOLD, LUNNEW, VOLOLD,
     *   VOLNEW, CNOOLD, CNONEW, CATNEW, BUFF1, BUFF2, BUFF3, IRET)
C-----------------------------------------------------------------------
C! copies extension file of the EXTINI/EXTIO variety
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   EXTCOP copies an extension file(s) of the EXTINI-EXTIO variety.
C   Inputs:
C      TYPE    C*2     Extension file type eg 'CC','AN'
C      INVER   I       Version number to copy, 0=>copy all.
C      OUTVER  I       Version number on output file, if more than one
C                      copied (INVER=0) this will be the # of the first
C                      file.  If OUTVER=0 the EXTINI defaults are used.
C      LUNOLD  I       LUN for old file
C      LUNNEW  I       LUN for new file
C      VOLOLD  I       Disk number for old file.
C      VOLNEW  I       Disk number for new file.
C      CNOOLD  I       Catalog slot number for old file
C      CNONEW  I       Catalog slot number for new file
C      CATNEW  I(256)  Catalog header for new file.
C   In/out:
C      BUFF1   I(>512) Work buffer: 256 words + n * 256 words (enough
C                      to hold at least one logical record)
C      BUFF2   I(>512) Work buffer: as BUFF1
C      BUFF3   I(*)    Buffer large enough to hold one logical record
C   Output:
C      IRET    I     Return error code  0 => ok
C                                       1 => files the same, no copy
C                                       2 => no input files exist
C                                       3 => failed
C                                       4 => no output files created
C-----------------------------------------------------------------------
      CHARACTER TYPE*2
      INTEGER   INVER, OUTVER, LUNOLD, LUNNEW, VOLOLD, VOLNEW, CNOOLD,
     *   CNONEW, CATNEW(256), BUFF1(512), BUFF2(512), BUFF3(256), IRET
C
      INTEGER   IERR, LREC, NREC, FINDO, FINDN, I, IVERI, IVERO, J,
     *   LIM, NVER, COUNT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                        Set initial version numbers.
      COUNT = 0
      IVERI = MAX (1, INVER)
      IVERO = OUTVER
C                                        Check if files the same
      IRET = 1
      IF ((VOLOLD.EQ.VOLNEW) .AND. (CNOOLD.EQ.CNONEW)) GO TO 999
      IRET = 3
C                                        Get old CATBLK in BUFF2.
      CALL CATIO ('READ', VOLOLD, CNOOLD, BUFF2, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 980
         END IF
C                                        Find # TYPE ext. files.
      CALL FNDEXT (TYPE, BUFF2, NVER)
C                                        No old ext. files
      IF (NVER.LE.0) THEN
         IRET = 2
         GO TO 999
         END IF
C                                        Old ext. files exist.
      LIM = MAX (1, NVER)
C                                        Check if only one file to be
C                                        copied.
      IF (INVER.GT.0) LIM = 1
C                                        Loop copying files.
      DO 100 J = 1,LIM
         NREC = 10
         LREC = 1
C                                        Init. old EXT. file.
         CALL EXTINI ('READ', TYPE, VOLOLD, CNOOLD, IVERI, BUFF2,
     *      LUNOLD, FINDO, LREC, NREC, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 75
         NREC = BUFF1(4)
         LREC = MAX (1, LREC)
C                                        Suppress error messages.
         MSGSUP = 32000
C                                        Copy header to new file.
         CALL COPY (256, BUFF1, BUFF2)
C                                        Init. new EXT. file.
         CALL EXTINI ('WRIT', TYPE, VOLNEW, CNONEW, IVERO, CATNEW,
     *      LUNNEW, FINDN, LREC, NREC, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 70
         COUNT = COUNT + 1
         IF (NREC.LE.0) GO TO 60
C                                        Loop copying file.
            DO 50 I = 1,NREC
               CALL EXTIO ('READ', LUNOLD, FINDO, I, BUFF3, BUFF1, IERR)
               IF (IERR.NE.0) GO TO 60
               CALL EXTIO ('WRIT', LUNNEW, FINDN, I, BUFF3, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 60
 50            CONTINUE
C                                        Close up.
 60      CALL EXTIO ('CLOS', LUNNEW, FINDN, J, BUFF3, BUFF2, IERR)
 70      CALL EXTIO ('CLOS', LUNOLD, FINDO, J, BUFF3, BUFF1, IERR)
 75      IVERI = IVERI + 1
         IVERO = IVERO + 1
C                                        Get old CATBLK in BUFF2.
         IF (J.EQ.LIM) GO TO 100
            CALL CATIO ('READ', VOLOLD, CNOOLD, BUFF2, 'REST', BUFF3,
     *         IERR)
            IF (IERR.EQ.0) GO TO 100
               WRITE (MSGTXT,1010) IERR
               GO TO 980
 100     CONTINUE
      IRET = 0
      IF (COUNT.EQ.0) IRET = 4
      GO TO 995
C
 980  CALL MSGWRT (8)
C
 995  MSGSUP = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('EXTCOP: ERROR',I3,' READING OLD CATBLK')
      END
