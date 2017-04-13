      SUBROUTINE TABCOP (TYPE, INVER, OUTVER, LUNOLD, LUNNEW, VOLOLD,
     *   VOLNEW, CNOOLD, CNONEW, CATNEW, BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C! copies one or all tables extension files of specified type
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997
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
C   TABCOP copies Table extension file(s).  The output file must be a
C   new extension - old ones cannot be rewritten.  The output file
C   must be opened WRIT in the catalog and will have its CATBLK
C   updated on disk.
C   Inputs:
C      TYPE     C*2     Extension file type (e.g. 'CC','AN')
C      INVER    I       Version number to copy, 0 => copy all.
C      OUTVER   I       Version number on output file, if more than one
C                       copied (INVER=0) this will be the number of the
C                       first file.  If OUTVER = 0, it will be taken as
C                       1 higher than the previous highest version.
C      LUNOLD   I       LUN for old file
C      LUNNEW   I       LUN for new file
C      VOLOLD   I       Disk number for old file.
C      VOLNEW   I       Disk number for new file.
C      CNOOLD   I       Catalog slot number for old file
C      CNONEW   I       Catalog slot number for new file
C   In/out:
C      CATNEW   I(256)  Catalog header for new file.
C   Output:
C      BUFF1    I(256)  Work buffer
C      BUFF2    I(256)  Work buffer
C      IRET     I       Return error code  0 => ok
C                          1 => files the same, no copy.
C                          2 => no input files exist
C                          3 => failed
C                          4 => no output files created.
C                          5 => failed to update CATNEW
C                          6 => output file exists
C   Inputs from MSG common:
C      MSGSUP   I       If 31990 < MSGSUP < 32000, the file copied
C                       message is suppressed.
C-----------------------------------------------------------------------
      INTEGER   INVER, OUTVER, LUNOLD, LUNNEW, VOLOLD, VOLNEW, CNOOLD,
     *   CNONEW, BUFF1(256), BUFF2(256), CATNEW(256), IRET
      CHARACTER TYPE*2
C
      CHARACTER PHNAME*48, ERRTYP(6)*4, CHTEMP*2
      INTEGER   FINDO, FINDN, COUNT, IVERI, IVERO, OVO, OVN, I, J, II,
     *   LIM, IER, LER, LTYE, NTYE, JERR, IRNO, MRNO, ISIZE, LSIZE,
     *   XRNO
      LOGICAL   T, F, TABLE, EXIST, FITASC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA ERRTYP /' OLD',' NEW','OPEN','READ','WRIT','CREA'/
C-----------------------------------------------------------------------
C                                       Set initial version numbers.
      COUNT = 0
      IVERI = MAX (1, INVER)
      IVERO = OUTVER
C                                       Check if files the same
      IRET = 1
      IF ((VOLOLD.EQ.VOLNEW) .AND. (CNOOLD.EQ.CNONEW) .AND.
     *   (INVER.EQ.OUTVER)) GO TO 999
C                                       See if new file exists
      IRET = 6
      CALL ISTAB (TYPE, VOLNEW, CNONEW, OUTVER, LUNNEW, BUFF1, TABLE,
     *   EXIST, FITASC, JERR)
      IF (EXIST .AND. (JERR.EQ.0)) GO TO 999
C                                       Get old CATBLK in BUFF2.
      IRET = 3
      CALL CATIO ('READ', VOLOLD, CNOOLD, BUFF2, 'REST', BUFF1, IER)
      IF ((IER.LE.0) .OR. (IER.GE.5)) GO TO 10
         WRITE (MSGTXT,1010) IER
         CALL MSGWRT (6)
         GO TO 999
C                                       Find # TYPE ext. files.
 10   OVO = 0
      OVN = 0
      J = 0
      CALL FXHDEX (BUFF2)
      CALL FXHDEX (CATNEW)
      DO 20 I = 1,KIEXTN
         II = KHEXT + I - 1
         CALL H2CHR (2, 1, BUFF2(II), CHTEMP)
         IF (TYPE.EQ.CHTEMP) OVO = I
         CALL H2CHR (2, 1, CATNEW(II), CHTEMP)
         IF (TYPE.EQ.CHTEMP) OVN = I
         IF ((CHTEMP.EQ.' ') .AND. (J.LE.0)) J = I
 20      CONTINUE
C                                       Old table files exist?
      IRET = 2
      IF (OVO.LE.0) GO TO 999
      LIM = BUFF2(KIVER+OVO-1)
      IF (LIM.LE.0) GO TO 999
      IF (INVER.GT.0) LIM = 1
C                                       No room to catalog new type
      IF ((J.GT.0) .OR. (OVN.GT.0)) GO TO 25
         WRITE (MSGTXT,1020)
         CALL MSGWRT (6)
         IRET = 3
         GO TO 999
C                                       Add ext. type to catalog
 25   IF (OVN.GT.0) GO TO 30
         OVN = J
         CALL CHR2H (2, TYPE, 1, CATNEW(KHEXT+OVN-1))
         CATNEW(KIVER+OVN-1) = 0
C                                       Default output version
 30   IF (IVERO.LE.0) IVERO = CATNEW(KIVER+OVN-1) + 1
C                                       Loop copying files.
      DO 100 J = 1,LIM
         FINDN = 0
C                                       Open input file
         CALL ZPHFIL (TYPE, VOLOLD, CNOOLD, IVERI, PHNAME, IER)
         CALL ZOPEN (LUNOLD, FINDO, VOLOLD, PHNAME, F, F, T, IER)
         LTYE = 1
         NTYE = 3
         IF (IER.NE.0) GO TO 90
C                                       Read first record
         IRNO = 1
         CALL ZFIO ('READ', LUNOLD, FINDO, IRNO, BUFF1, IER)
         NTYE = 4
         IF (IER.NE.0) GO TO 85
         MRNO = BUFF1(1)
C                                       Create output file
         BUFF1(32) = VOLNEW
         CALL ZDATE (BUFF1(33))
         CALL ZTIME (BUFF1(36))
         CALL ZPHFIL (TYPE, VOLNEW, CNONEW, IVERO, PHNAME, IER)
         CALL CHR2H (48, PHNAME, 1, BUFF1(17))
         ISIZE = MRNO
         CALL ZCREAT (VOLNEW, PHNAME, ISIZE, F, LSIZE, IER)
         LTYE = 2
         NTYE = 6
         IF (IER.NE.0) GO TO 85
C                                       Open output file
         CALL ZOPEN (LUNNEW, FINDN, VOLNEW, PHNAME, F, T, T, IER)
         NTYE = 3
         IF (IER.NE.0) FINDN = 0
         IF (IER.NE.0) GO TO 80
         ISIZE = LSIZE
C                                       size changed: change limits
         IF (ISIZE.EQ.MRNO) GO TO 40
            BUFF1(1) = ISIZE
            LSIZE = ISIZE - MRNO
            XRNO = BUFF1(3)
            IF (BUFF1(9).GT.0) XRNO = XRNO + LSIZE * BUFF1(9)
            IF (BUFF1(9).LT.0) XRNO = XRNO - LSIZE / BUFF1(9)
            BUFF1(3) = XRNO
C                                       Write/read loop
 40      CONTINUE
            CALL ZFIO ('WRIT', LUNNEW, FINDN, IRNO, BUFF1, IER)
            LTYE = 2
            NTYE = 5
            IF (IER.NE.0) GO TO 80
            IRNO = IRNO + 1
            IF (IRNO.GT.MRNO) GO TO 50
               CALL ZFIO ('READ', LUNOLD, FINDO, IRNO, BUFF1, IER)
               LTYE = 1
               NTYE = 4
               IF (IER.NE.0) GO TO 80
               GO TO 40
 50      COUNT = COUNT + 1
         IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
            WRITE (MSGTXT,1050) TYPE, VOLOLD, CNOOLD, IVERI, VOLNEW,
     *         CNONEW, IVERO
            CALL MSGWRT (3)
            END IF
         GO TO 85
C                                       Error: destroy output
 80      IF (FINDN.GT.0) CALL ZCLOSE (LUNNEW, FINDN, LER)
         CALL ZDESTR (VOLNEW, PHNAME, LER)
         FINDN = 0
C                                       Close files
 85      CALL ZCLOSE (LUNOLD, FINDO, LER)
         IF (FINDN.GT.0) CALL ZCLOSE (LUNNEW, FINDN, LER)
C                                       Report error
 90      IF (IER.GT.0) THEN
            WRITE (MSGTXT,1090) IER, ERRTYP(NTYE), ERRTYP(LTYE), TYPE,
     *         IVERI, IVERO
            CALL MSGWRT (6)
            END IF
         I = KIVER + OVN - 1
         CATNEW(I) = MAX (IVERO, CATNEW(I))
         IVERI = IVERI + 1
         IVERO = IVERO + 1
 100     CONTINUE
      IRET = 0
      CALL CATIO ('UPDT', VOLNEW, CNONEW, CATNEW, 'REST', BUFF1, IER)
      IF (IER.NE.0) IRET = 5
      IF (COUNT.EQ.0) IRET = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('TABCOP: ERROR',I5,' READING OLD CATBLK')
 1020 FORMAT ('TABCOP: NO ROOM IN NEW CATBLK FOR NEW EXTENSION TYPE')
 1050 FORMAT ('Copied ',A2,' file from vol/cno/vers',I3,I5,I4,' to',
     *   I3,I5,I4)
 1090 FORMAT ('TABCOP: ERROR',I5,1X,A4,'ING',A4,1X,A2,' FILE VERS',
     *   I4,' TO',I4)
      END
