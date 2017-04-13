LOCAL INCLUDE 'QUXTR.INC'
C                                       Local include for QUXTR
      INCLUDE 'INCS:PMAD.INC'
C
      HOLLERITH XNAMEQ(3), XCLASQ(2), XNAMEU(3), XCLASU(2), XOUFIL(12)
      REAL      XSEQQ, XDISKQ, XSEQU, XDISKU, VPARM(2,15)
      REAL      BUFF(MABFSL), QUVALS(2,MAXIMG)
      DOUBLE PRECISION FREQS(MAXIMG)
      INTEGER   SEQI(2), DISKI(2), OLDCNO(2), JBUFSZ, CATOLD(256,2),
     *   SCRTCH(256), XPIX(15), YPIX(15), NPIX, NFREQS, EASY
      CHARACTER NAMEI(2)*12, CLASI(2)*6, OUTFIL*48
      COMMON /INPARM/ XNAMEQ, XCLASQ, XSEQQ, XDISKQ, XNAMEU, XCLASU,
     *   XSEQU, XDISKU, XOUFIL, VPARM
      COMMON /CHRCOM/ NAMEI, CLASI, OUTFIL
      COMMON /PARMS/ CATOLD, SEQI, DISKI, OLDCNO, JBUFSZ, XPIX, YPIX,
     *   EASY, NPIX, NFREQS
      COMMON /BUFRS/ FREQS, QUVALS, BUFF, SCRTCH
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C                                       End QUXTR
LOCAL END
      PROGRAM QUXTR
C-----------------------------------------------------------------------
C! extracts Q,U spectra to text file for input to TARS
C# Map Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 2012
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
C   QUXTR is an AIPS task to modify an image by a model
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEI         Name of Q input image.
C      INCLASS        CLASI         Class of Q input image.
C      INSEQ          SEQI          Seq. of Q input image.
C      INDISK         DISKI         Disk number of Q input image.
C      IN2NAME        NAMEI         Name of U input image.
C      IN2CLASS       CLASI         Class of U input image.
C      IN2SEQ         SEQI          Seq. of U input image.
C      IN2DISK        DISKI         Disk number of U input image.
C      OUTFILE        OUTFIL        Base name of output file(s)
C      VPARM          VPARM(2,15)   List of x,y pixels to send out
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'QUXTR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGM /'QUXTR '/
C-----------------------------------------------------------------------
C                                       Get inputs
      CALL QUXTRI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       do it the easy way
      IF (EASY.LE.1) THEN
         CALL QUXTRE (IRET)
C                                       else poor transposition
      ELSE IF (EASY.EQ.2) THEN
         CALL QUXTR2 (IRET)
C                                       else poor transposition
      ELSE IF (EASY.EQ.3) THEN
         CALL QUXTR3 (IRET)
         END IF
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE QUXTRI (PRGN, IRET)
C-----------------------------------------------------------------------
C   QUXTRI gets input parameters for QUXTR and creates an output file.
C   Inputs:
C      PRGN    C*6       Program name
C   Output:
C      IRET    I         Error code: 0 => ok
C                           4 => user routine detected error.
C                           5 => catalog troubles
C                           8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER STAT*4, MTYPE*2, BNDCOD*8
      INTEGER   IERR, NPARM, IROUND, I, IRA, IDEC, IFREQ, NRA, NDEC,
     *   IFQ, FQBUFF(512), FQKOLS(MAXFQC), FQNUMV(MAXFQC), JRA, JDEC,
     *   IFQRNO, JFQ, NUMREC, NUMIF, IFSIDE, FQID, ILUN, FQVER
      DOUBLE PRECISION IFFREQ, IQFREQ
      REAL      IFCHW, IFTBW
      LOGICAL   ISFQ
      INCLUDE 'QUXTR.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSL
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 56
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEQ, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQI(1) = IROUND (XSEQQ)
      SEQI(2) = IROUND (XSEQU)
      DISKI(1) = IROUND (XDISKQ)
      DISKI(2) = IROUND (XDISKU)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEQ, NAMEI(1))
      CALL H2CHR (6, 1, XCLASQ, CLASI(1))
      CALL H2CHR (12, 1, XNAMEU, NAMEI(2))
      CALL H2CHR (6, 1, XCLASU, CLASI(2))
      CALL H2CHR (48, 1, XOUFIL, OUTFIL)
C                                       Get CATBLK from old file.
      MTYPE = 'MA'
      DO 20 I = 1,2
         OLDCNO(I) = 1
         CALL CATDIR ('SRCH', DISKI(I), OLDCNO(I), NAMEI(I),
     *      CLASI(I), SEQI(I), MTYPE, NLUSER, STAT, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, NAMEI(I), CLASI(I), SEQI(I),
     *         DISKI(I), NLUSER
            GO TO 990
            END IF
C                                       Read CATBLK and mark 'READ'.
         CALL CATIO ('READ', DISKI(I), OLDCNO(I), CATOLD(1,I),
     *      'READ', SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR, I
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKI(I)
         FCNO(NCFILE) = OLDCNO(I)
         FRW(NCFILE) = 0
 20      CONTINUE
      CALL COPY (256, CATOLD, CATBLK)
C                                       find which axis is which
      CALL AXEFND (4, 'RA--', CATBLK(KIDIM), CATH(KHCTP), IRA, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'RA-- AXIS NOT FOUND'
         GO TO 990
         END IF
      CALL AXEFND (4, 'DEC-', CATBLK(KIDIM), CATH(KHCTP), IDEC, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'DEC- AXIS NOT FOUND'
         GO TO 990
         END IF
      CALL AXEFND (4, 'FQID', CATBLK(KIDIM), CATH(KHCTP), IFQ, IRET)
      IF (IRET.EQ.0) THEN
         ISFQ = .TRUE.
         CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATH(KHCTP), IFREQ,
     *      IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'FREQ AXIS NOT FOUND ALTHOUGH FQID ONE FOUND'
            GO TO 990
            END IF
         EASY = IFQ + 1
      ELSE
         ISFQ = .FALSE.
         CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATH(KHCTP), IFREQ,
     *      IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'FREQ/FQID AXIS NOT FOUND'
            GO TO 990
            END IF
         EASY = IFREQ + 1
         END IF
      NRA = CATBLK(KINAX+IRA)
      NDEC = CATBLK(KINAX+IDEC)
      NPIX = 0
      DO 30 I = 1,15
         JRA = IROUND (VPARM(1,I))
         JDEC = IROUND (VPARM(2,I))
         IF ((JRA.LT.1) .OR. (JRA.GT.NRA) .OR. (JDEC.LT.1) .OR.
     *      (JDEC.GT.NDEC)) GO TO 40
         NPIX = NPIX + 1
         IF (IRA.LT.IDEC) THEN
            XPIX(NPIX) = JRA
            YPIX(NPIX) = JDEC
         ELSE
            XPIX(NPIX) = JDEC
            YPIX(NPIX) = JRA
            END IF
 30      CONTINUE
 40   IF (NPIX.LT.1) THEN
         MSGTXT = 'NO PIXEL POSITIONS SPECIFIED'
         IRET = 10
         GO TO 990
         END IF
C                                       get frequencies
      NFREQS = CATBLK(KINAX+IFREQ)
C                                       from header
      IF (.NOT.ISFQ) THEN
         DO 50 I = 1,NFREQS
            FREQS(I) = CATD(KDCRV+IFREQ) + (I - CATR(KRCRP+IFREQ)) *
     *         CATR(KRCIC+IFREQ)
 50         CONTINUE
C                                       from FQ table
      ELSE
         FQVER = 1
         ILUN = 53
         CALL FQINI ('READ', FQBUFF, DISKI(1), OLDCNO(1), FQVER, CATBLK,
     *      ILUN, IFQRNO, FQKOLS, FQNUMV, NUMIF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1050) IRET, 'OPENING FQ TABLE'
            GO TO 990
            END IF
C                                       Get number of records
         NUMREC = FQBUFF(5)
         IF (NUMREC.LT.NFREQS) THEN
            WRITE (MSGTXT,1055) NUMREC, NFREQS
            IRET = 10
            GO TO 990
            END IF
C                                       read FQ table
         CALL DFILL (NFREQS, 0.0D0, FREQS)
         DO 60 IFQRNO = 1,NUMREC
            JFQ = IFQRNO
            CALL TABFQ ('READ', FQBUFF, JFQ, FQKOLS, FQNUMV, NUMIF,
     *         FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1050) IRET, 'READING FQ TABLE'
               GO TO 990
               END IF
            IQFREQ = (FQID - CATD(KDCRV+IFQ)) / CATR(KRCIC+IFQ) +
     *         CATR(KRCRP+IFQ)
            I = IQFREQ + 0.01
            FREQS(I) = IFFREQ + CATD(KDCRV+IFREQ)
 60         CONTINUE
C                                      Close table.
         CALL TABIO ('CLOS', 0, IFQRNO, FQBUFF, FQBUFF, IRET)
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QUXTRI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1020 FORMAT ('QUXTRI: ERROR',I3,' COPYING CATBLK IMAGE',I2)
 1050 FORMAT ('QUXTRI: ERROR',I4,' ON ',A)
 1055 FORMAT ('QUXTRI: FQ TABLE # ROWS',I5,' LESS THAN EXPECTED',I5)
      END
      SUBROUTINE QUXTRE (IRET)
C-----------------------------------------------------------------------
C   QUXTRE finds and writes up to 15 text files with Q/U values taken
C   from the first axis of the images.
C   Output:
C      IRET    I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER IFILE*48, CHARS*16, OUTLIN*80
      INTEGER   LUNI, NYI, NXI, WINI(4), BOI, IDEPTH(5), BOTEMP, IBIND,
     *   INDI, I, J, K, NLIN, JTRIM, TLUN, TIND
      LOGICAL   T, F
      INCLUDE 'QUXTR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUNI /16/
      DATA T, F /.TRUE.,.FALSE./
      DATA CHARS /'123456789ABCDEF'/
C-----------------------------------------------------------------------
C                                       loop over selected pixels
      DO 100 K = 1,NPIX
         WRITE (MSGTXT,1005) XPIX(K), YPIX(K), K
         CALL MSGWRT (2)
         DO 30 J = 1,2
            CALL COPY (256, CATOLD(1,J), CATBLK)
C                                       Open and init for read
            CALL ZPHFIL ('MA', DISKI(J), OLDCNO(J), 1, IFILE, IRET)
            CALL ZOPEN (LUNI, INDI, DISKI(J), IFILE, T, F, T, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPEN INPUT', J
               GO TO 990
               END IF
C                                       Setup for I/O
            NXI = CATBLK(KINAX)
            NYI = CATBLK(KINAX+1)
            WINI(1) = 1
            WINI(2) = XPIX(K)
            WINI(3) = NXI
            WINI(4) = XPIX(K)
C                                       Loop
            CALL FILL (5, 1, IDEPTH)
            IDEPTH(1) = YPIX(K)
            CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, BOTEMP,
     *         IRET)
            BOI = BOTEMP + 1
            CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF,
     *         JBUFSZ, BOI, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'INIT INPUT', J
               GO TO 990
               END IF
C                                       Read.
            CALL MDISK ('READ', LUNI, INDI, BUFF, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ INPUT', J
               GO TO 990
               END IF
            DO 20 I = 1,NXI
               QUVALS(J,I) = BUFF(IBIND+I-1)
 20            CONTINUE
            CALL ZCLOSE (LUNI, INDI, IRET)
 30         CONTINUE
C                                       write text file
         I = JTRIM (OUTFIL)
         IFILE = OUTFIL(:I) // CHARS(K:K)
         TLUN = 11
         CALL ZTXOPN ('WRIT', TLUN, TIND, IFILE, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT TEXT', K
            GO TO 990
            END IF
         WRITE (OUTLIN,1030) K, XPIX(K), YPIX(K)
         I = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', TLUN, TIND, OUTLIN(:I), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT TEXT', K
            GO TO 990
            END IF
         WRITE (OUTLIN,1031)
         I = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', TLUN, TIND, OUTLIN(:I), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT TEXT', K
            GO TO 990
            END IF
         NLIN = 0
         I = 47
         DO 50 J = 1,NFREQS
            IF ((FREQS(J).GT.0.0D0) .AND. (QUVALS(1,J).NE.FBLANK) .AND.
     *         (QUVALS(2,J).NE.FBLANK)) THEN
               NLIN = NLIN + 1
               WRITE (OUTLIN,1035) FREQS(J), QUVALS(1,J), QUVALS(2,J)
               CALL ZTXIO ('WRIT', TLUN, TIND, OUTLIN(:I), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT TEXT', K
                  GO TO 990
                  END IF
               END IF
 50         CONTINUE
         CALL ZTXCLS (TLUN, TIND, IRET)
         IRET = 0
         WRITE (MSGTXT,1050) NLIN, K
         CALL MSGWRT (3)
 100     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QUXTRE: ERROR',I3,' ON ',A,' FILE',I3)
 1005 FORMAT ('Beginning X,Y pixel',2I6,' output file',I3)
 1030 FORMAT ('; QUXTR pixel',I3,' xpix',I6,' ypix',I6)
 1031 FORMAT (';  Frequency',8X,'Q',12X,'U',7X,'weight')
 1035 FORMAT (F13.0,2F13.5,'    1.00')
 1050 FORMAT ('Wrote',I6,' lines in file',I3)
      END
      SUBROUTINE QUXTR2 (IRET)
C-----------------------------------------------------------------------
C   QUXTR2 reads the X-freq-Y cube to extract spectra
C   Output:
C      IRET    I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   iRET
C
      CHARACTER IFILE*48, OUTLIN*80, CHARS*16
      INTEGER   LUNI, NYI, NXI, WINI(4), BOI, BOTEMP, IBIND, INDI,
     *   J, K, IDEPTH(5), I, JTRIM, TLUN, TIND, NLIN
      LOGICAL   T, F
      INCLUDE 'QUXTR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUNI /16/
      DATA T, F /.TRUE.,.FALSE./
      DATA CHARS /'123456789ABCDEF'/
C-----------------------------------------------------------------------
      DO 100 K = 1,NPIX
         WRITE (MSGTXT,1005) XPIX(K), YPIX(K), K
         CALL MSGWRT (2)
         DO 30 J = 1,2
            CALL COPY (256, CATOLD(1,J), CATBLK)
C                                       Open and init for read
            CALL ZPHFIL ('MA', DISKI(J), OLDCNO(J), 1, IFILE, IRET)
            CALL ZOPEN (LUNI, INDI, DISKI(J), IFILE, T, F, T, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPEN INPUT', J
               GO TO 990
               END IF
C                                       Setup for I/O
            NXI = CATBLK(KINAX)
            NYI = CATBLK(KINAX+1)
            WINI(1) = XPIX(K)
            WINI(2) = 1
            WINI(3) = XPIX(K)
            WINI(4) = NYI
            CALL FILL (5, 1, IDEPTH)
            IDEPTH(1) = YPIX(K)
            CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, BOTEMP,
     *         IRET)
            BOI = BOTEMP + 1
            CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF,
     *         JBUFSZ, BOI, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'INIT INPUT', J
               GO TO 990
               END IF
C                                       Read.
            DO 20 I = 1,NYI
               CALL MDISK ('READ', LUNI, INDI, BUFF, IBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READ INPUT', J
                  GO TO 990
                  END IF
               QUVALS(J,I) = BUFF(IBIND)
 20            CONTINUE
            CALL ZCLOSE (LUNI, INDI, IRET)
 30         CONTINUE
C                                       write text file
         I = JTRIM (OUTFIL)
         IFILE = OUTFIL(:I) // CHARS(K:K)
         TLUN = 11
         CALL ZTXOPN ('WRIT', TLUN, TIND, IFILE, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT TEXT', K
            GO TO 990
            END IF
         WRITE (OUTLIN,1030) K, XPIX(K), YPIX(K)
         I = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', TLUN, TIND, OUTLIN(:I), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT TEXT', K
            GO TO 990
            END IF
         WRITE (OUTLIN,1031)
         I = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', TLUN, TIND, OUTLIN(:I), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT TEXT', K
            GO TO 990
            END IF
         NLIN = 0
         I = 47
         DO 50 J = 1,NFREQS
            IF ((FREQS(J).GT.0.0D0) .AND. (QUVALS(1,J).NE.FBLANK) .AND.
     *         (QUVALS(2,J).NE.FBLANK)) THEN
               NLIN = NLIN + 1
               WRITE (OUTLIN,1035) FREQS(J), QUVALS(1,J), QUVALS(2,J)
               CALL ZTXIO ('WRIT', TLUN, TIND, OUTLIN(:I), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT TEXT', K
                  GO TO 990
                  END IF
               END IF
 50         CONTINUE
         CALL ZTXCLS (TLUN, TIND, IRET)
         IRET = 0
         WRITE (MSGTXT,1050) NLIN, K
         CALL MSGWRT (3)
 100     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QUXTR2: ERROR',I3,' ON ',A,' FILE',I3)
 1005 FORMAT ('Beginning X,Y pixel',2I6,' output file',I3)
 1030 FORMAT ('; QUXTR pixel',I3,' xpix',I6,' ypix',I6)
 1031 FORMAT (';  Frequency',8X,'Q',12X,'U',7X,'weight')
 1035 FORMAT (F13.0,2F13.5,'    1.00')
 1050 FORMAT ('Wrote',I6,' lines in file',I3)
      END
      SUBROUTINE QUXTR3 (IRET)
C-----------------------------------------------------------------------
C   QUXTRH reads the X-Y-FREQ cube to extract spectra
C   Output:
C      IRET    I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER IFILE*48, OUTLIN*80, CHARS*16
      INTEGER   LUNI, NYI, NXI, WINI(4), BOI, IDEPTH(5), BOTEMP, IBIND,
     *   INDI, I, J, K, NLIN, TLUN, TIND, JTRIM, NZI
      LOGICAL   T, F
      INCLUDE 'QUXTR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUNI /16/
      DATA T, F /.TRUE.,.FALSE./
      DATA CHARS /'123456789ABCDEF'/
C-----------------------------------------------------------------------
C                                       loop over selected pixels
      DO 100 K = 1,NPIX
         WRITE (MSGTXT,1005) XPIX(K), YPIX(K), K
         CALL MSGWRT (2)
         DO 30 J = 1,2
            CALL COPY (256, CATOLD(1,J), CATBLK)
C                                       Open and init for read
            CALL ZPHFIL ('MA', DISKI(J), OLDCNO(J), 1, IFILE, IRET)
            CALL ZOPEN (LUNI, INDI, DISKI(J), IFILE, T, F, T, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPEN INPUT', J
               GO TO 990
               END IF
C                                       Setup for I/O
            NXI = CATBLK(KINAX)
            NYI = CATBLK(KINAX+1)
            NZI = CATBLK(KINAX+2)
            WINI(1) = XPIX(K)
            WINI(2) = YPIX(K)
            WINI(3) = XPIX(K)
            WINI(4) = YPIX(K)
            CALL FILL (5, 1, IDEPTH)
            DO 20 I = 1,NZI
               IDEPTH(1) = I
               CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH,
     *            BOTEMP, IRET)
               BOI = BOTEMP + 1
               CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF,
     *            JBUFSZ, BOI, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'INIT INPUT', J
                  GO TO 990
                  END IF
C                                       Read.
               CALL MDISK ('READ', LUNI, INDI, BUFF, IBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READ INPUT', J
                  GO TO 990
                  END IF
               QUVALS(J,I) = BUFF(IBIND)
 20            CONTINUE
            CALL ZCLOSE (LUNI, INDI, IRET)
 30         CONTINUE
C                                       write text file
         I = JTRIM (OUTFIL)
         IFILE = OUTFIL(:I) // CHARS(K:K)
         TLUN = 11
         CALL ZTXOPN ('WRIT', TLUN, TIND, IFILE, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT TEXT', K
            GO TO 990
            END IF
         WRITE (OUTLIN,1030) K, XPIX(K), YPIX(K)
         I = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', TLUN, TIND, OUTLIN(:I), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT TEXT', K
            GO TO 990
            END IF
         WRITE (OUTLIN,1031)
         I = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', TLUN, TIND, OUTLIN(:I), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT TEXT', K
            GO TO 990
            END IF
         NLIN = 0
         I = 47
         DO 50 J = 1,NFREQS
            IF ((FREQS(J).GT.0.0D0) .AND. (QUVALS(1,J).NE.FBLANK) .AND.
     *         (QUVALS(2,J).NE.FBLANK)) THEN
               NLIN = NLIN + 1
               WRITE (OUTLIN,1035) FREQS(J), QUVALS(1,J), QUVALS(2,J)
               CALL ZTXIO ('WRIT', TLUN, TIND, OUTLIN(:I), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT TEXT', K
                  GO TO 990
                  END IF
               END IF
 50         CONTINUE
         CALL ZTXCLS (TLUN, TIND, IRET)
         IRET = 0
         WRITE (MSGTXT,1050) NLIN, K
         CALL MSGWRT (3)
 100     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QUXTRD: ERROR',I3,' ON ',A,' FILE',I2)
 1005 FORMAT ('Beginning X,Y pixel',2I6,' output file',I3)
 1030 FORMAT ('; QUXTR pixel',I3,' xpix',I6,' ypix',I6)
 1031 FORMAT (';  Frequency',8X,'Q',12X,'U',7X,'weight')
 1035 FORMAT (F13.0,2F13.5,'    1.00')
 1050 FORMAT ('Wrote',I6,' lines in file',I3)
      END
