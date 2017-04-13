      SUBROUTINE OTBSRT (DISK, CNO, TYPE, INVER, OUTVER, BUFFER, BUFSZ,
     *   TABUFF, CATBLK, KEY, FKEY, ISCR, IERR)
C-----------------------------------------------------------------------
C! Copies sorted table from scratch file to table form
C# Ext-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2003, 2015
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
C   Reads old numbers from the scratch file and writes the new table
C   file. Called by TABSRT.
C   Inputs:
C      DISK     I        Disk number of the file.
C      CNO      I        Catalog slot number.
C      TYPE     C*2      Two character type code (e.g. 'CC')
C      INVER    I        Input version number
C      OUTVER   I        Output version number
C      BUFFER   R        I/O work buffer
C      BUFSZ    I        Size of BUFFER in bytes.
C      TABUFF   I(512)   Buffer large enough to handle I/O to table.
C                        File assumed previously opened and the values
C                        for the input file are in the array and the
C                        input table file should be open.
C      CATBLK   I(256)   Catalog header record.
C      KEY      I(2,2)   Sort keys: may be linear combination of two
C                        numeric value columns.  KEY contains the column
C                        numbers and FKEY contains the factors.  If the
C                        column is a string (bit or char.) then
C                        FKEY(1,n)=first char/bit and FKEY(2,n)=number
C                        of char/bit and KEY(2,n) is ignored.
C                        KEY(2,n)=0 => ignore, <0 => use abs. value.
C                        Column no. is the logical number.
C      FKEY     R(2,2)   Key coefficients, 0 => 1, see above.
C      ISCR     I        /CFILES/ scratch file number of scratch file.
C      LENBU    I        Buffer length in records.
C   Output:
C       IERR    I        Return error code: 0 = OK
C                           8 = I/O error.
C   Usage note: all files will be closed on successful return.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, INVER, OUTVER, BUFSZ, TABUFF(*), CATBLK(256),
     *   KEY(2,2), ISCR, IERR
      CHARACTER TYPE*2
      REAL      BUFFER(*), FKEY(2,2)
C
      CHARACTER PHNAME*48, ERRTYP(7)*4, CHTMP*2, OUTNAM*48
      INTEGER   FINDO, FINDN, IER, OVO, I, J, LIM, LER, LTYE, NTYE, II,
     *   IND, LUN, BIND, NIO, LUNNEW, LUNOLD, NIOUT, NPRPLR, NLRPPR,
     *   IROFF, LRECO, NICOPY, IBOFF, BO, VO, NUMREC, RECNO, IRNO,
     *   RFIRST, OLDRNO, MRNO, NFIRST, ISIZE, LSIZE, OTVER
      LOGICAL   T, F, BIG
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA BO, VO /1, 0/
      DATA ERRTYP /' OLD',' NEW', 'OPEN','READ','WRIT','CREA', 'SCR '/
C-----------------------------------------------------------------------
C                                       Get number of records.
      NUMREC = TABUFF(5)
C                                       Open scratch (index) file.
      LUN = 16
      CALL ZPHFIL ('SC', SCRVOL(ISCR), SCRCNO(ISCR), 1, PHNAME, IERR)
      CALL ZOPEN (LUN, IND, SCRVOL(ISCR), PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         IND = 0
         WRITE (MSGTXT,1000) IERR, 'READ'
         GO TO 995
         END IF
C                                       Init scratch file.
      LRECO = 2 + TABUFF(8)
      NICOPY = TABUFF(8)
      NIOUT = 1
      CALL UVINIT ('READ', LUN, IND, NUMREC, VO, LRECO, NIOUT, BUFSZ,
     *   BUFFER, BO, BIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, 'READ'
         GO TO 995
         END IF
C                                       Find in catalog header
      OVO = 0
      CALL FXHDEX (CATBLK)
      DO 30 I = 1,KIEXTN
         II = KHEXT + I - 1
         CALL H2CHR (2, 1, CATBLK(II), CHTMP)
         IF (TYPE.EQ.CHTMP) OVO = I
 30      CONTINUE
C                                       Set output version number
      LIM = CATBLK(KIVER+OVO-1) + 1
      IF ((OVO.LE.0) .OR. (LIM.LE.0)) THEN
         IERR = 2
         MSGTXT = 'ERROR IN CATALOG HEADER - NO EXTENSION OF TYPE '
     *      // TYPE
         GO TO 995
         END IF
      IF (OUTVER.LE.0) OUTVER = LIM
      IF (OUTVER.GT.LIM) OUTVER = LIM
C                                       Get number of records in file
      MRNO = TABUFF(1)
C                                       Get LUN, FIND for old file
      LUNOLD = TABUFF(81)
      LUNNEW = LUNOLD - 1
      FINDO = TABUFF(82)
C                                       Get control info.
      BIG = TABUFF(9).LT.0
      NPRPLR = 1
      NLRPPR = 1
C                                       No. log. rec / phys. rec
      IF (TABUFF(9).GT.0) NLRPPR = TABUFF(9)
C                                       No. phys. rec / log. rec
      IF (TABUFF(9).LT.0) NPRPLR = -TABUFF(9)
      OLDRNO = -1
C                                       Initialize control record.
C                                       Set sort order
      TABUFF(43) = KEY(1,1)
      IF (TABUFF(43).LT.0) TABUFF(43) = 256 - KEY(1,1)
      TABUFF(44) = KEY(1,2)
      IF (TABUFF(44).LT.0) TABUFF(44) = 256 - KEY(1,2)
      IF (FKEY(1,1).LT.0.0) TABUFF(43) = -TABUFF(43)
      IF (FKEY(1,2).LT.0.0) TABUFF(44) = -TABUFF(44)
C                                       Other control info.
      TABUFF(32) = DISK
      CALL ZDATE (TABUFF(33))
      CALL ZTIME (TABUFF(36))
      CALL ZPHFIL (TYPE, DISK, CNO, OUTVER, OUTNAM, IER)
      CALL CHR2H (48, OUTNAM, 1, TABUFF(17))
      CALL CHR2H (6, TSKNAM, 1, TABUFF(39))
C                                       Go to SC file
      OTVER = 2
      CALL ZPHFIL ('SC', DISK, CNO, OTVER, PHNAME, IER)
      ISIZE = MRNO
C                                       Does it already exist
      CALL ZEXIST (DISK, PHNAME, LSIZE, IERR)
C                                       destroy mystery one
      IF (IERR.NE.1) THEN
         CALL ZDESTR (DISK, PHNAME, IERR)
         WRITE (MSGTXT,1022) IERR
         IF (IERR.GT.2) GO TO 995
         END IF
C                                       Create
      CALL ZCREAT (DISK, PHNAME, ISIZE, F, LSIZE, IERR)
      LTYE = 7
      NTYE = 6
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1020) IERR
         IF (IERR.EQ.3) MSGTXT = 'OTBSRT: INSUFFICIENT SPACE ON DISK'
         IF (IERR.EQ.5) WRITE (MSGTXT,1021)
         GO TO 995
         END IF
C                                       Open output (SC) file
      CALL ZOPEN (LUNNEW, FINDN, DISK, PHNAME, F, T, T, IERR)
      LTYE = 7
      IF (IERR.NE.0) THEN
         NTYE = 3
         FINDN = 0
         GO TO 800
         END IF
C                                       Write first record
      IRNO = 1
      NTYE = 5
      CALL ZFIO ('WRIT', LUNNEW, FINDN, IRNO, TABUFF, IERR)
      IF (IERR.NE.0) GO TO 800
C                                       Copy remaining header records
      NFIRST = TABUFF(50)
      DO 50 IRNO = 2,NFIRST
         LTYE = 1
         NTYE = 4
         CALL ZFIO ('READ', LUNOLD, FINDO, IRNO, TABUFF(257), IERR)
         IF (IERR.NE.0) GO TO 800
         LTYE = 7
         NTYE = 5
         CALL ZFIO ('WRIT', LUNNEW, FINDN, IRNO, TABUFF(257), IERR)
         IF (IERR.NE.0) GO TO 800
 50      CONTINUE
C                                       Begin loop copying records
      DO 300 RECNO = 1,NUMREC
C                                       Read next index
         CALL UVDISK ('READ', LUN, IND, BUFFER, NIO, BIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) 'READ', IERR
            CALL MSGWRT (8)
            GO TO 800
            END IF
C                                       Check if BIG
         IF (.NOT.BIG) THEN
C                                       Get file record number and
C                                       word offset.
            RFIRST = (RECNO-1) / NLRPPR
            IRNO = NFIRST + RFIRST
            IROFF = (RECNO - (RFIRST * NLRPPR) - 1) * TABUFF(8)
C                                       If new record and OLDRNO.gt.0
C                                       write last record.
            IF ((IRNO.NE.OLDRNO) .AND. (OLDRNO.GT.0)) THEN
               NTYE = 5
               LTYE = 7
               CALL ZFIO ('WRIT', LUNNEW, FINDN, OLDRNO, TABUFF(257),
     *            IERR)
               IF (IERR.NE.0) GO TO 800
               END IF
            OLDRNO = IRNO
C                                       Copy record
            CALL COPY (NICOPY, BUFFER(BIND+2), TABUFF(257+IROFF))
C                                       Big records
         ELSE
C                                       Get file record number.
            IRNO = NFIRST + (RECNO-1) * NPRPLR
C                                       Loop
            DO 290 J = 1,NPRPLR
C                                       Set offset
               IBOFF = 2 + (J-1) * 256
               NTYE = 5
               LTYE = 2
               CALL ZFIO ('WRIT', LUNNEW, FINDN, IRNO,
     *            BUFFER(BIND+IBOFF), IERR)
               IF (IERR.NE.0) GO TO 800
C                                       Update record pointers
               IRNO = IRNO + 1
 290           CONTINUE
            END IF
 300     CONTINUE
C                                       Flush output buffer if nec.
      NTYE = 5
      LTYE = 7
      IF (.NOT.BIG) THEN
         CALL ZFIO ('WRIT', LUNNEW, FINDN, OLDRNO, TABUFF(257), IERR)
         IF (IERR.NE.0) GO TO 800
         END IF
      GO TO 950
C                                       Error, destroy file.
 800  IF (FINDN.GT.0) CALL ZCLOSE (LUNNEW, FINDN, LER)
      CALL ZDESTR (DISK, PHNAME, LER)
      FINDN = 0
C                                       Report error
      WRITE (MSGTXT,1900) IERR, ERRTYP(NTYE), ERRTYP(LTYE),
     *   TYPE, INVER, OUTVER
      IF (IERR.GT.0) CALL MSGWRT (6)
C                                       Close files
 950  CALL ZCLOSE (LUN, IND, LER)
      CALL ZCLOSE (LUNOLD, FINDO, LER)
      IF (FINDN.GT.0) CALL ZCLOSE (LUNNEW, FINDN, LER)
C                                       remove any old OUTVER and
C                                       rename SC to OUTVER
      IF (IERR.EQ.0) THEN
         CALL ZEXIST (DISK, OUTNAM, LSIZE, IERR)
         IF (IERR.EQ.0) CALL ZDESTR (DISK, OUTNAM, IERR)
         IF (IERR.GT.2) THEN
            WRITE (MSGTXT,1950) IERR
            CALL MSGWRT (8)
            CALL ZDESTR (DISK, PHNAME, LER)
         ELSE
            CALL ZRENAM (DISK, PHNAME, OUTNAM, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1955) IERR
               CALL MSGWRT (8)
               CALL ZDESTR (DISK, PHNAME, LER)
               END IF
            END IF
         END IF
C                                       No error only:
C                                       Update CATBLK
      IF (IERR.EQ.0) THEN
         I = KIVER + OVO - 1
         CATBLK(I) = MAX (OUTVER, CATBLK(I))
         IF (CATBLK(I).GE.LIM) CALL CATIO ('UPDT', DISK, CNO, CATBLK,
     *      'REST', TABUFF, IERR)
         END IF
      GO TO 999
C                                       Error
 995  CALL MSGWRT (8)
      IF (IND.GT.0) CALL ZCLOSE (LUN, IND, LER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OTBSRT: ERROR',I3,' OPEN FOR ',A4,' INPUT FILE')
 1010 FORMAT ('OTBSRT: ERROR',I3,' INIT FOR ',A4,' INPUT FILE')
 1020 FORMAT ('OTBSRT: ERROR',I3,' CREATING OUTPUT TABLE FILE')
 1021 FORMAT ('OTBSRT: DISK PROHIBITED FOR CREATING OUTPUT TABLE FILE')
 1022 FORMAT ('OTBSRT: ERROR',I5,' DELETING ANTIQUE SCRATCH TABLE')
 1050 FORMAT ('OTBSRT:',A4,' ERROR ',I3,' ON INPUT FILE')
 1900 FORMAT ('OTBSRT: ERROR',I5,1X,A4,'ING ',A4,1X,A2,' FILE VERS',
     *   I4,' TO',I4)
 1950 FORMAT ('OTBSRT: ERROR',I5,' REMOVING OLD COPY OF EXTENSION FILE')
 1955 FORMAT ('OTBSRT: ERROR',I5,' ERROR RENAMING SCR TABLE TO OUTPUT')
      END
