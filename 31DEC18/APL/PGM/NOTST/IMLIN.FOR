LOCAL INCLUDE 'IMLIN.INC'
      INCLUDE 'INCS:PMAD.INC'
C                                       Local include for IMLIN
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7), YINC,
     *   ZINC, FCUT, DOCAT, PIXVAL, PORDER, XBOXES, BOXES(2,20),
     *   BADD(10)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, INFILE*48
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), XINFIL(12)
      DOUBLE PRECISION CATD(128), OLDD(128)
      HOLLERITH CATH(256), OLDH(256)
      REAL      CATR(256), OLDR(256), PMIN(22), PMAX(22),
     *   BUFF1(MABFSS), BUFF2(MABFSS)
      INTEGER   CATBLK(256), CATOLD(256), SEQIN, SEQOUT, DISKIN, DISKO,
     *   NEWCNO, OLDCNO, JBUFSZ, NORDER, NBOXES, NBOX(2,20),
     *   SCRTCH(512)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XINFIL, XNAMOU,
     *   XCLAOU, XSEQO, XDISKO, BLC, TRC, YINC, ZINC, DOCAT, PORDER,
     *   XBOXES, BOXES, BADD, FCUT, PIXVAL
      COMMON /BSLCHR/ NAMEIN, CLAIN, NAMOUT, CLAOUT, INFILE
      COMMON /PARMS/ CATOLD, PMAX, PMIN, SEQIN, SEQOUT, DISKIN, DISKO,
     *   NEWCNO, OLDCNO, JBUFSZ, NORDER, NBOXES, NBOX
      COMMON /BUFRS/ BUFF1, BUFF2, SCRTCH
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      EQUIVALENCE (CATOLD, OLDR, OLDD, OLDH)
C                                                          End IMLIN.
LOCAL END
LOCAL INCLUDE 'IMLIN2.INC'
C                                                         Include IMLIN2
C                                       Local include for IMLIN
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   NITTER, ITTER, JJC, MASK(MABFSS), NPTS
      REAL      XBAR, DATA(MABFSS), AARRAY(5,5), CARRAY(5,5), GAMMA(5),
     *   MOMENT(15), POLYFN(5), POLAVG(5), POLXFN(MABFSS,5), IGNORD,
     *   SOLVED
      COMMON /GDATA/ DATA, AARRAY, CARRAY, GAMMA, MOMENT, POLYFN, XBAR,
     *   POLAVG, POLXFN, SOLVED, IGNORD, NPTS, NITTER, ITTER, JJC, MASK
C                                                          End IMLIN2
LOCAL END
      PROGRAM IMLIN
C-----------------------------------------------------------------------
C! Fits polynomial baselines to rows of an image.
C# Map Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2008, 2010, 2015
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
C   IMLIN fits polynomial baselines to rows of an image.
C
C   CONTAINS: 1. Harvey Liszt math fitting methods.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      LOGICAL   ABLANK
      INCLUDE 'IMLIN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'IMLIN '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL IMLIIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL IMLIDO (ABLANK, IRET)
      IF (IRET.EQ.0) CALL IMLIOU (ABLANK, IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE IMLIIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   IMLIIN gets input parameters for IMLIN and creates an output file
C   plus files IF requested for the parameter images.
C   Inputs:
C      PRGN   C*6   Program name
C   Output:
C      IRET   I     Error code: 0 => ok
C                               4 => user routine detected error.
C                               5 => catalog troubles
C                               8 => can't start
C      /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      CHARACTER STAT*4, BLANK*6, CTYPE(19)*4, BUNITS(9)*4, MTYPE*2,
     *   SEQTYP(2,8)*8, CORDER(6)*2, OTYPE*8
      INTEGER   IERR, NPARM, IROUND, NAX, I, ITYP, NTYP, IG, IP, INPSEQ
      INCLUDE 'IMLIN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA BLANK /'      '/
      DATA NTYP, CTYPE /19, 'TIME','FREQ','LAMB','VELO','FELO','    ',
     *   'PIXE','DIST','ANGL','RA  ','RA--','LL  ','DEC ','DEC-',
     *   'MM  ','GLON','GLAT','ELON','ELAT'/
      DATA BUNITS /'/SEC', '/HZ ', '/M  ', '/M/S', '/M/S', '/PIX',
     *   '/PIX', '/DEG', '/UNK'/
      DATA SEQTYP /'CONT    ', 'DCONT   ',
     *             'SLOPE   ', 'DSLOPE  ',
     *             'CURV    ', 'DCURV   ',
     *             'DERIV3  ', 'DDERV3  ',
     *             'DERIV4  ', 'DDERV4  ',
     *             'DERIV5  ', 'DDERV5  ',
     *             'DERIV6  ', 'DDERV6  ',
     *             'DERIV7  ', 'DDERV7  '/
      DATA CORDER /'22','33','44','55','66','77'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 95
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF ((RQUICK) .OR. (IRET.NE.0)) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (48, 1, XINFIL, INFILE)
      DO 20 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 20      CONTINUE
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'READ', SCRTCH,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD, CATBLK)
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATR(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATR(KHIMC))
      CATBLK(KIIMS) = SEQOUT
      INPSEQ = SEQOUT
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATOLD(KIDIM), CATOLD(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get user modification to CATBLK
      IRET = 4
      CALL BASHED (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output file for residual
      IRET = 4
      NEWCNO = 1
      CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 2
C                                       Make names, classes, disks OK.
      SEQOUT = CATBLK(KIIMS)
      CALL RFILL (22, -1.0E15, PMAX)
      CALL RFILL (22, 1.0E15, PMIN)
C                                       save the residual header
      CALL COPY (256, CATBLK, BUFF2)
      FCUT = -1E20
      NORDER = PORDER + 1.01
      IF (NORDER.GT.5) NORDER = 5
C                                       Basic output header: results
      IF (DOCAT.GT.0.0) THEN
         CATBLK(KIDIM) = CATBLK(KIDIM) - 1
         NAX = CATBLK(KIDIM)
         DO 80 I = 1,NAX
            CATBLK(KINAX+I-1) = CATBLK(KINAX+I)
            CATR(KRCRP+I-1) = CATR(KRCRP+I)
            CATR(KRCRT+I-1) = CATR(KRCRT+I)
            CATR(KRCIC+I-1) = CATR(KRCIC+I)
            CATD(KDCRV+I-1) = CATD(KDCRV+I)
            CALL CHCOPY (8, 1, CATH(KHCTP+I*2), 1, CATH(KHCTP+(I-1)*2))
 80        CONTINUE
         DO 85 I = NAX,6
            CATBLK(KINAX+I) = 1
 85         CONTINUE
C                                       Find type of old axis
         CALL H2CHR (8, 1, OLDH(KHCTP), OTYPE)
         DO 90 ITYP = 1,NTYP
            IF (OTYPE(:4).EQ.CTYPE(ITYP)) GO TO 100
 90         CONTINUE
         ITYP = 0
         WRITE (MSGTXT,1090) OTYPE
         CALL MSGWRT (6)
 100     IF (ITYP.GT.7) ITYP = 8
         IF (ITYP.EQ.0) ITYP = 9
C                                       Output polynomial parms
         CATBLK(KIIMS) = INPSEQ
         DO 120 IG = 1,NORDER
            DO 110 IP = 1,2
               CALL CHR2H (6, SEQTYP(IP,IG), KHIMCO, CATR(KHIMC))
               CALL CHCOPY (8, 1, OLDH(KHBUN), 1, CATR(KHBUN))
               IF (IG.NE.1) CALL CHR2H (4, BUNITS(ITYP),
     *            5, CATR(KHBUN))
               IF (IG.GT.2) CALL CHR2H (1, CORDER(IG-2),
     *            8, CATR(KHBUN))
C                                       Create
               DISKO = XDISKO + 0.01
               NEWCNO = 1
               CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1100) IERR, SEQTYP(IP,IG)
                  GO TO 990
                  END IF
C                                       Record the creation
               NCFILE = NCFILE + 1
               FVOL(NCFILE) = DISKO
               FCNO(NCFILE) = NEWCNO
               FRW(NCFILE) = 2
C                                       copy keywords mostly
               CALL KEYPCP (DISKIN, OLDCNO, DISKO, NEWCNO, 0, ' ', IERR)
 110           CONTINUE
 120        CONTINUE
         END IF
C                                       close this one up
      IRET = 0
      NORDER = NORDER - 1
      CALL COPY (256, BUFF2, CATBLK)
      DISKO = FVOL(2)
      NEWCNO = FCNO(2)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMLIIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT RESIDUALS FILE')
 1090 FORMAT ('AXIS TYPE ',A8,' DOES NOT HAVE KNOWN UNITS')
 1100 FORMAT ('ERROR',I5,' CREATING FILE TYPE ',A6)
      END
      SUBROUTINE IMLIDO (TBLNKD, IRET)
C-----------------------------------------------------------------------
C   IMLIDO sends image one row at a time to the baseline fitting
C   routine and then writes the modified data.
C   Output:
C      TBLNKD   L    Answers contain blanked pixels?
C      IRET     I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      LOGICAL   TBLNKD
      INTEGER   IRET
C
      CHARACTER IFILE*48, LINE*256
      INTEGER   IROUND, LUNI, LUNO, NYI, NXI, WINI(4), NXO, NYO, II,
     *   WINO(4), BOI, J, BOO, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1,
     *   I2, I3, I4, I5, I6, I7, IPOS(7), CORN(7), BOTEMP, KOFF, LIMO,
     *   LIMIT, IBIND, OBIND, INDI, INDO, LIM1, IG, WINT(4), LUNT, INDT,
     *   NAXT(7), OBINDT, IBUFF3(1), JBUFS3, IINC2, IINC3, IERR, I,
     *   SIZE, TLUN, TFIND, FITWT, FITCH, KBP, KBPLIM, JTRIM, IU, IL
      REAL      PLTODO, PLDONE, OUTMAX, OUTMIN, BUFF3(512), XPLD
      LOGICAL   T, F, BLNKD, TGOOD
      DOUBLE PRECISION XXX
      INCLUDE 'IMLIN2.INC'
      INCLUDE 'IMLIN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (IBUFF3, BUFF3)
      DATA LUNI, LUNO, LUNT, TLUN /16,17,18,10/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Note: CATOLD & CATBLK must be
C                                       switched in their addresses
C                                       Move the data that way
      CALL COPY (256, CATOLD, BUFF2)
      CALL COPY (256, CATBLK, CATOLD)
      CALL COPY (256, BUFF2, CATBLK)
      JBUFS3 = 2 * 512
      IGNORD = 0.0
      SOLVED = 0.0
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Create scratch files.
C                                       For answers: temp scratch
      CALL COPY (7, CATOLD(KINAX), NAXT)
      NAXT(1) = PORDER + 2.1
      NAXT(1) = NAXT(1) * 2
      IF (DOCAT.GT.0.0) THEN
         CALL MAPSIZ (CATOLD(KIDIM), NAXT, SIZE)
         CALL SCREAT (SIZE, SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
         CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, IFILE, IERR)
         CALL ZOPEN (LUNT, INDT, SCRVOL(NSCR), IFILE, T, T, T, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1015) IRET
            GO TO 990
            END IF
         END IF
C                                       For residual map:
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
C                                       Use actual f.p. output
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Setup for I/O
C                                       remember names switched
      NXI = CATBLK(KINAX)
      NYI = CATBLK(KINAX+1)
      NXO = CATOLD(KINAX)
      NYO = CATOLD(KINAX+1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
      WINT(1) = 1
      WINT(2) = 1
      WINT(3) = NAXT(1)
      WINT(4) = NAXT(2)
      OUTMAX = -1.0E20
      OUTMIN = 1.0E20
      BLNKD = F
      OBINDT = 1
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM4 = TRC(4) - BLC(4) + 1.01
      LIM5 = TRC(5) - BLC(5) + 1.01
      LIM6 = TRC(6) - BLC(6) + 1.01
      LIM7 = TRC(7) - BLC(7) + 1.01
      PLDONE = 0.0
      PLTODO = REAL(LIM7) * REAL(LIM4) * REAL(LIM5) * REAL(LIM6)
      PLTODO = PLTODO * CATOLD(KINAX+2)
      KOFF = 0
      CORN(7) = 1
      LIMO = CATOLD(KINAX) - 1
      IINC2 = YINC + 0.01
      IINC3 = ZINC + 0.01
      TBLNKD = .FALSE.
      TGOOD = .FALSE.
C                                       baseline windows
      JJC = NORDER + 1
      CALL FILL (40, 0, NBOX)
      NBOXES = XBOXES + 0.01
      IG = TRC(1) - BLC(1) + 1.01
      IU = TRC(1) + 0.99
      I1 = BLC(1) - 0.99
      IL = I1 + 1
      IF (INFILE.NE.' ') THEN
         WRITE (MSGTXT,1030) INFILE
         CALL MSGWRT (4)
         CALL ZTXOPN ('READ', TLUN, TFIND, INFILE, .FALSE., IERR)
         IF (IERR.NE.0) THEN
             MSGTXT = 'Cannot open ' // INFILE
             GO TO 990
            END IF
  45     CONTINUE
            CALL ZTXIO ('READ', TLUN, TFIND, LINE, IERR)
            IF (IERR.EQ.2) GO TO 46
            IF (IERR.NE.0) THEN
               MSGTXT = 'Error reading ' // INFILE
               GO TO 990
               END IF
C                                       THIS IS ILLEGAL IN ANSI FORTRAN
C           READ (LINE,*) FITCH, FITWT
C                                       so use AIPS methods
            KBPLIM = JTRIM (LINE)
            KBP = 1
            CALL GETNUM (LINE, 256, KBP, XXX)
            FITCH = 0
            FITWT = 0
            IF (XXX.NE.DBLANK) THEN
               FITCH = XXX + 0.001D0
               IF (KBP.LT.256) THEN
                  CALL GETNUM (LINE, 256, KBP, XXX)
                  IF (XXX.NE.DBLANK) FITWT = XXX + 0.001
                  END IF
               END IF
            FITCH = FITCH - I1
            IF ((FITWT.GT.0) .AND. (FITCH.GT.0)) MASK(FITCH) = 1.0
            GO TO 45
  46     CALL ZTXCLS(TLUN, TFIND, IERR)
      ELSE
         IF (NBOXES.LE.0) THEN
            NBOXES = 2
            BOXES(1,1) = 2.
            BOXES(2,1) = 3. + IG / 10.
            BOXES(1,2) = IG - 1.0 - IG / 10.
            BOXES(2,2) = IG - 1.0
            END IF
         CALL FILL (IG, 0, MASK)
         DO 55 I = 1,NBOXES
            NBOX(1,I) = BOXES(1,I) - I1
            NBOX(2,I) = BOXES(2,I) - I1
            NBOX(1,I) = MAX (NBOX(1,I), 1)
            NBOX(2,I) = MIN (NBOX(2,I), IG)
            J = NBOX(2,I) - NBOX(1,I) + 1
            CALL FILL (J, 1, MASK(NBOX(1,I)))
 55         CONTINUE
         WRITE (MSGTXT,1035) NBOXES
         CALL MSGWRT(4)
         END IF
C
      DO 60 I = 1,IG,10
         II = MIN (IG, I+9)
         I1 = I + IL - 1
         WRITE (MSGTXT,1070) I1, (MASK(J), J = I,II)
         CALL MSGWRT (4)
 60      CONTINUE
C
      NPTS = 0
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = BLC(7) + I7 - 0.9
         CORN(7+KOFF) = I7
         DO 600 I6 = 1,LIM6
            IPOS(6) = BLC(6) + I6 - 0.9
            CORN(6+KOFF) = I6
            DO 500 I5 = 1,LIM5
               IPOS(5) = BLC(5) + I5 - 0.9
               CORN(5+KOFF) = I5
               DO 400 I4 = 1,LIM4
                  IPOS(4) = BLC(4) + I4 - 0.9
                  CORN(4+KOFF) = I4
                  DO 300 I3 = 1,LIM3,IINC3
                     IPOS(3) = BLC(3) + I3 - 0.9
                     CORN(3+KOFF) = (I3 - 1) / IINC3 + 1
                     XPLD = AINT (PLDONE / 10.0 + 0.01)
                     XPLD = 10.0 * XPLD + 1.0
                     PLDONE = PLDONE + 1.
                     IF (ABS(XPLD-PLDONE).LE.0.1) THEN
                        WRITE (MSGTXT,1090) PLDONE, PLTODO
                        CALL MSGWRT (1)
                        END IF
C                                       Init. files, first input.
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3), BOTEMP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1099) IRET
            GO TO 990
            END IF
         BOI = BOTEMP + 1
         CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF1, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'READ', IRET
            GO TO 990
            END IF
C                                       Init answer file.
         IF (DOCAT.GT.0.0) THEN
            CALL COMOFF (CATOLD(KIDIM), NAXT, CORN(3), BOTEMP, IRET)
            BOO = BOTEMP + 1
            CALL MINIT ('WRIT', LUNT, INDT, NAXT, NAXT(2), WINT, BUFF3,
     *         JBUFS3, BOO, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) 'WRIT', IRET
               GO TO 990
               END IF
            END IF
C                                       Init output file.
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), CORN(3), BOTEMP,
     *      IRET)
         BOO = BOTEMP + 1
         CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2, JBUFSZ,
     *      BOO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'WRIT', IRET
            GO TO 990
            END IF
         DO 250 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.9
            IPOS(1) = IROUND (BLC(1))
C                                       Read.
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'READ', IRET
               GO TO 990
               END IF
C                                       Want this row?
            IF (MOD(I2-1,IINC2).NE.0) GO TO 250
C                                       Copy to common.
            CALL RCOPY (LIM1, BUFF1(IBIND), DATA)
C                                       Write.
            IF (DOCAT.GT.0.0) THEN
               CALL MDISK ('WRIT', LUNT, INDT, BUFF3, OBINDT, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1120) 'WRIT', IRET
                  GO TO 990
                  END IF
               END IF
            CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'WRIT', IRET
               GO TO 990
               END IF
C                                       Call DO1BAS
            CALL DO1BAS (IPOS, BUFF3(OBINDT), BUFF2(OBIND), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1180) IRET
               IF (IRET.EQ.99) WRITE (MSGTXT,1181)
               GO TO 990
               END IF
C                                       Check max, min, blanking.
            LIMIT = OBIND + LIMO
            DO 200 I1 = OBIND,LIMIT
               BLNKD = BLNKD .OR. (BUFF2(I1).EQ.FBLANK)
               IF (BUFF2(I1).NE.FBLANK) THEN
                  OUTMAX = MAX (OUTMAX, BUFF2(I1))
                  OUTMIN = MIN (OUTMIN, BUFF2(I1))
                  END IF
 200           CONTINUE
            IF ((TBLNKD) .AND. (TGOOD)) GO TO 250
               LIMIT = OBINDT + WINT(3) - 1
               DO 215 I1 = OBINDT,LIMIT
                  IF (BUFF3(I1).EQ.FBLANK) THEN
                     TBLNKD = .TRUE.
                  ELSE
                     TGOOD = .TRUE.
                     END IF
 215              CONTINUE
 250        CONTINUE
C                                       Flush buffers.
         IF (DOCAT.GT.0.0) THEN
            CALL MDISK ('FINI', LUNT, INDT, BUFF3, OBINDT, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'FINI', IRET
               GO TO 990
               END IF
            END IF
         CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1120) 'FINI', IRET
            GO TO 990
            END IF
C                                       Update CATBLK.
         OLDR(KRDMX) = OUTMAX
         OLDR(KRDMN) = OUTMIN
         CALL CATIO ('UPDT', DISKO, NEWCNO, CATOLD, 'REST', DATA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1260) IRET
            GO TO 990
            END IF
 300     CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Resume AIPS
      IRET = 0
C                                       Mark blanking in CATBLK.
      OLDR(KRBLK) = 0.0
      IF (BLNKD) OLDR(KRBLK) = FBLANK
      OLDR(KRDMN) = OUTMIN
      OLDR(KRDMX) = OUTMAX
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      IF (DOCAT.GT.0.) CALL ZCLOSE (LUNT, INDT, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
C                                       return catblk's to normal
      CALL COPY (256, CATOLD, BUFF2)
      CALL COPY (256, CATBLK, CATOLD)
      CALL COPY (256, BUFF2, CATBLK)
C                                       Was anything done?
      IF (.NOT.TGOOD) THEN
         MSGTXT = 'NO BASELINES WERE FIT - WAS EVERYTHING BLANKED?'
         CALL MSGWRT (8)
         IRET = 10
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMLIDO: ERROR',I3,' OPENING INPUT FILE')
 1010 FORMAT ('IMLIDO: ERROR',I3,' CREATING ANSWER SCRATCH FILE')
 1015 FORMAT ('IMLIDO: ERROR',I3,' OPENING ANSWER SCRATCH FILE')
 1020 FORMAT ('IMLIDO: ERROR',I5,' OPENING OUTPUT FILE')
 1030 FORMAT ('Reading weights file: ',A48)
 1035 FORMAT ('Setting window from ',I2,' boxes')
 1070 FORMAT ('Weights (',I3,') ',10(1X,I2))
 1090 FORMAT ('Begin plane',F8.0,' of',F8.0)
 1099 FORMAT ('IMLIDO: COMOFF ERROR',I3)
 1100 FORMAT ('IMLIDO: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('IMLIDO: ',A4,' ERROR',I3)
 1180 FORMAT ('IMLIDO: DO1BAS ERROR',I3)
 1181 FORMAT ('QUITTING AT USER REQUEST')
 1260 FORMAT ('IMLIDO: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE BASHED (IRET)
C-----------------------------------------------------------------------
C   BASHED modifies the new image header for the subimaging and for
C   replacing the first axis with parameters.
C   Input:
C      CATBLK(256)    I     Output catalog header, also CATR, CATD
C      CATOLD(256)    I     Input catalog header, also OLDR, OLDD
C   Output:
C      CATBLK(256)    I     Modified output catalog header.
C      IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER FCHARS(3)*4, CHTM12*12
      INTEGER   I
      INCLUDE 'IMLIN.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
C                                       Check input axes
      DO 10 I = 1,3
         CALL H2CHR (4, 1, CATR(KHCTP), CHTM12)
         IF (FCHARS(I).EQ.CHTM12) GO TO 20
 10      CONTINUE
      WRITE (MSGTXT,1010)
      CALL MSGWRT (4)
C                                       Set axes in output CATBLK.
 20   I = YINC + 0.01
      IF (I.LE.0) I = 1
      YINC = I
      I = ZINC + 0.01
      IF (I.LE.0) I = 1
      ZINC = I
      CALL SUBHD3 (BLC, TRC, 1.0, YINC, ZINC)
C                                       Check polynomial order
      IF (PORDER.LT.0.0) PORDER = 0.0
      IF (PORDER.GT.4.0) THEN
         WRITE (MSGTXT,1030) PORDER
         CALL MSGWRT (6)
         PORDER = 4.0
         END IF
C                                       Check input size
      IRET = 0
      IF (TRC(1)-BLC(1).GE.MABFSS) THEN
         WRITE (MSGTXT,1040)
         CALL MSGWRT (8)
         IRET = 4
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('WARNING: FIRST AXIS NOT FREQUENCY OR VELOCITY')
 1030 FORMAT ('ORDER=',F7.3,' TOO LARGE: REDUCED TO 4.0')
 1040 FORMAT ('WORKS ONLY ON (SUB)ROWS <= 16384 PIXELS')
      END
      SUBROUTINE DO1BAS (IPOS, RESULT, RESIDS, IRET)
C-----------------------------------------------------------------------
C   DO1BAS fits baselines to a row of an image and returns the answers
C   in RESULT and the residuals in RESIDS.
C   Inputs:
C      IPOS(7)   I    BLC (input image) of first value in DATA
C   Values from commons:
C      DATA(*)   D    Input row, magic value blanked.
C      FBLANK    R    Value of blanked pixel.
C      CATBLK    I    Output catalog header (also CATR, CATD)
C      CATOLD    I    Input catalog header (also OLDR, OLDD)
C   Output:
C      RESULT(*) R    Output row (parameter answers, errors).
C      RESIDS(*) R    Residuals (DATA - model).
C      IRET      I    Return code   0 => OK
C                               >0 => error, terminate.
C   Output in COMMON
C     CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IRET
      REAL      RESULT(*), RESIDS(*)
C
      INTEGER   ING, INPARM, INPTS, I, IERR, OUPARM, NTRY
      REAL      PARMS(9)
      INCLUDE 'IMLIN2.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'IMLIN.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
      NTRY = 0
C                                       Not last call
      IF (IPOS(1).LT.0) GO TO 999
C                                       Get the initial guess
         NTRY = NTRY + 1
         ING = PORDER + 0.1
         INPARM = ING + 1
         OUPARM = 2 * INPARM + 2
         INPTS = TRC(1) - BLC(1) + 1.01
         ITTER = 0
         NITTER = 100
         XBAR = IPOS(1) - 1 - CATR(KRCRP)
         CALL POLYIN (INPTS, IERR)
         IF (IERR.NE.0) GO TO 50
         CALL IMLIGE (NORDER, INPTS, PARMS, IERR)
         IF (IERR.NE.0) GO TO 50
C                                       Compute answers
         CALL XBALMS (INPTS, PARMS)
         IF (PARMS(1).EQ.FBLANK) GO TO 50
C                                       Get errors and nice units
         CALL IMLIFI (INPARM, PARMS, RESULT)
         I = 1
         ITTER  = ITTER - 1
         CALL XBFUNC (INPTS, PARMS, RESIDS)
         SOLVED = SOLVED + 1.0
C                                       Max / Min
         IF (DOCAT.GT.0.) THEN
            DO 45 I = 1,OUPARM
               IF (RESULT(I).NE.FBLANK) THEN
                  PMAX(I) = MAX (PMAX(I), RESULT(I))
                  PMIN(I) = MIN (PMIN(I), RESULT(I))
                  END IF
 45            CONTINUE
            END IF
         GO TO 999
C                                       Failure: Blank outputs
 50      IF (DOCAT.GT.0.) CALL RFILL (OUPARM, FBLANK, RESULT)
         CALL RCOPY (INPTS, DATA, RESIDS)
         IGNORD = IGNORD + 1.0
C
 999  RETURN
      END
      SUBROUTINE XBALMS (INPTS, PARMS)
C-----------------------------------------------------------------------
C   XBALMS computes the answers
C   Inputs:
C      INPTS   I      Number data points
C   Output:
C      PARMS   R(9)   Answers (1 - 8), sigma ** 2 (9)
C-----------------------------------------------------------------------
      INTEGER   INPTS
      REAL      PARMS(9)
C
      INTEGER   I, J, IS
      REAL      YBAR, YBAR2
      INCLUDE 'IMLIN2.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       clear sum variables
      CALL RFILL (9, 0.0, PARMS)
      YBAR = 0.0
      YBAR2 = 0.0
      IS = 0
C                                       sum: data, data**2
C                                       data*polyfunc(j)
      DO 30 I = 1,INPTS
         IF ((MASK(I).EQ.1) .AND. (DATA(I).NE.FBLANK)) THEN
            IS = IS + 1
            YBAR = YBAR + DATA(I)
            YBAR2 = YBAR2 + DATA(I)**2
            DO 20 J = 1,JJC
               PARMS(J) = PARMS(J) + DATA(I) * POLXFN(I,J)
 20            CONTINUE
            END IF
 30      CONTINUE
C                                       average
C                                       sigma**2=ybar2-sum(parms**2)
      IF (IS.GT.0) THEN
         YBAR = YBAR / IS
         YBAR2 = YBAR2 / IS
         PARMS(9) = YBAR2
         DO 40 I = 1,JJC
            PARMS(I) = PARMS(I) / IS
            PARMS(9) = PARMS(9) - PARMS(I)**2
 40         CONTINUE
C                                       no data
      ELSE
         CALL RFILL (9, FBLANK, PARMS)
         END IF
C
 999  RETURN
      END
      SUBROUTINE IMLIGE (NG, ND, PARMS, IERR)
C-----------------------------------------------------------------------
C   IMLIGE obtains an initial guess for the parameters of the baseline.
C   It sets a linear one only.
C   Inputs:
C      NG      I         order of polynomial
C      ND      I         Number of data samples
C   Output:
C      PARMS   R(NG+1)   Guess to use
C      IERR    I         0 => ok, 1 => all data BLANKED
C-----------------------------------------------------------------------
      INTEGER   NG, ND, IERR
      REAL      PARMS(9)
C
      INTEGER   I, NS
      REAL      R, BLS, BLO, SD, SDX, SX, SXX
      INCLUDE 'IMLIN2.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Fit baseline
      BLS = 0.
      BLO = 0.
      CALL RFILL (9, 0.0, PARMS)
      NS = 0
      SD = 0.0
      SX = 0.0
      SXX = 0.0
      SDX = 0.0
      DO 20 I = 1,ND
         IF ((MASK(I).EQ.1) .AND. (DATA(I).NE.FBLANK)) THEN
            NS = NS + 1
            SD = SD + DATA(I)
            IF (NG.GT.0) THEN
               R = I + XBAR
               SX = SX + R
               SXX = SXX + R * R
               SDX = SDX + R * DATA(I)
               END IF
            END IF
 20      CONTINUE
C                                       all blanked
      IF (NS.LE.0) THEN
         IERR = 1
C                                       got some data
      ELSE
         IERR = 0
         R = NS * SXX - SX * SX
         IF (R.NE.0.0) BLS = (NS * SDX - SD * SX) / R
         IF (NS.GT.0) BLO = (SD - BLS * SX) /NS
         PARMS(1) = BLO
         PARMS(2) = BLS
C                                       Model average
         SD = 0.0
         DO 30 I = 1,ND
            SD = SD + BLO + BLS * (I+XBAR)
 30         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE IMLIFI (NP, PARMS, RESULT)
C-----------------------------------------------------------------------
C   IMLIFI determines the errors in the fit and converts the results
C   to normal units for output.
C   Inputs:
C      NP       I       Number of parameters (max) - JJC gives
C                          number fit this time
C      PARMS    R(9)    Answers from XBALMS
C   Output:
C      RESULT   R(18)   Answers then errors in normal units
C-----------------------------------------------------------------------
      INTEGER   NP
      REAL      PARMS(9), RESULT(18)
C
      INTEGER   I, JC, L, J, CATOLD(256)
      DOUBLE PRECISION    OLDD(128)
      REAL      OLDR(256), R, AVER
      INCLUDE 'IMLIN2.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATOLD
      EQUIVALENCE (CATOLD, OLDR, OLDD)
C-----------------------------------------------------------------------
C                                       Convert to output
      IF (PARMS(9).NE.FBLANK) THEN
         IF (PARMS(9).GT.0.0) PARMS(9) = SQRT (PARMS(9))
         IF (OLDR(KRCIC).EQ.0.0) OLDR(KRCIC) = 1.0
         R = 1.0
         JC = NP + 1
         DO 20 I = 1,NP
            L = I + JC
            RESULT(I) = FBLANK
            RESULT(L) = FBLANK
            IF (I.LE.JJC) THEN
               RESULT(I) = 0.0
               RESULT(L) = 0.0
               DO 10 J = I,JJC
                  RESULT(I) = RESULT(I) + PARMS(J) * CARRAY(J,I)
                  RESULT(L) = RESULT(L) + CARRAY(J,I) ** 2
 10               CONTINUE
               RESULT(I) = RESULT(I) * R
               RESULT(L) = PARMS(9) * SQRT (RESULT(L)/NPTS) * ABS(R)
               R = R / OLDR(KRCIC)
               END IF
 20         CONTINUE
C                                       flux
         R = 0.0
         AVER = 0.0
         DO 30 J = 1,JJC
            AVER = AVER + PARMS(J) * POLAVG(J)
            R = R + POLAVG(J) ** 2
 30         CONTINUE
         RESULT(JC) = AVER
         RESULT(2*JC) = PARMS(9) * SQRT (R/NPTS)
C                                       blanked
      ELSE
         JC = 2 * (NP + 1)
         CALL RFILL (JC, FBLANK, RESULT)
         END IF
C
 999  RETURN
      END
      SUBROUTINE XBFUNC (NDATA, PARMS, FVEC)
C-----------------------------------------------------------------------
C   XBFUNC computes the difference between the data and the model
C   Inputs:
C      NDATA   I      Number of data points in row
C      PARMS   R(9)   factors of orthogonal polynomials
C   Common: /GDATA/
C      DATA    R(?)   Original slice data points.
C   Output:
C      FVEC    R(*)   Slice data points minus data points evaluated for
C                     current guess.
C-----------------------------------------------------------------------
      INTEGER   NDATA
      REAL      PARMS(9), FVEC(*)
C
      INTEGER   I, J
      REAL      SUM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'IMLIN2.INC'
C-----------------------------------------------------------------------
C                                       Determine difference between
C                                       data and current fit.
      DO 20 I = 1,NDATA
         IF (DATA(I).NE.FBLANK) THEN
            SUM = 0.0
            DO 10 J = 1,JJC
               SUM = SUM + PARMS(J) * POLXFN(I,J)
 10            CONTINUE
            FVEC(I) = DATA(I) - SUM
         ELSE
            FVEC(I) = FBLANK
            END IF
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE IMLIOU (BLNKD, IRET)
C-----------------------------------------------------------------------
C   IMLIOU, if any, creates and fills (via PSCALE) the individual
C   baseline parameters.  It calls IMLIHI for history info for all.
C   Inputs:
C      BLNKD   L   Are any parameters blanked?
C   Output:
C      IRET    I   0 => ok,  4 => real trouble.
C-----------------------------------------------------------------------
      INTEGER   IRET
      LOGICAL   BLNKD
C
      CHARACTER SEQTYP*6
      INTEGER   NG, NXO, NYO, WINI(4), WINO(4), IERR, IP, NCN, IG, IOFF
      INCLUDE 'IMLIN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      NCN = 2
C                                       Residual map history
      CALL IMLIHI (IRET, NCN)
      IF (DOCAT.LE.0.0) GO TO 999
C                                       loop limits etc.
      NG = PORDER + 2.01
      WINI(1) = 1
      WINI(2) = 1
      WINI(3) = 2 * NG
      WINO(1) = 1
      WINO(2) = 1
C                                       Output all parms
      DO 30 IG = 1,NG-1
         DO 20 IP = 1,2
            IOFF = IG
            IF (IP.EQ.2) IOFF = NG + IG
            NCN = NCN + 1
            NEWCNO = FCNO(NCN)
            DISKO = FVOL(NCN)
            CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'REST', SCRTCH,
     *         IERR)
            IF ((IERR.NE.0) .AND. (IERR.NE.6)) THEN
               WRITE (MSGTXT,1001) IERR, NCN
               GO TO 990
               END IF
            CALL H2CHR (6, KHIMCO, CATH(KHIMC), SEQTYP)
            WRITE (MSGTXT,1010) SEQTYP
            CALL MSGWRT (1)
            SEQOUT = CATBLK(KIIMS)
            CALL H2CHR (12, KHIMNO, CATR(KHIMN), NAMOUT)
            CALL H2CHR (6, KHIMCO, CATR(KHIMC), CLAOUT)
            NXO = CATBLK(KINAX)
            NYO = CATBLK(KINAX+1)
            WINI(4) = NXO
            WINO(3) = NXO
            WINO(4) = NYO
C                                       Fill image
            CALL PSCALE (IOFF, NSCR, WINI, NEWCNO, DISKO, WINO, JBUFSZ,
     *         PMAX, PMIN, BLNKD, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1011) IERR, SEQTYP
               GO TO 990
               END IF
C                                       History, close
            CALL IMLIHI (IOFF, NCN)
 20         CONTINUE
 30      CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IRET = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('ERROR',I5,' RECOVERING FILE HEADER NUMBER',I5)
 1010 FORMAT ('Begin writing file of type ',A6)
 1011 FORMAT ('ERROR',I5,' MOVING DATA TO FILE TYPE ',A)
      END
      SUBROUTINE PSCALE (IOFF, ISCR, WINI, NEWCNO, DISKO, WINO, JBUFSZ,
     *   PMAX, PMIN, BLNKD, IERR)
C-----------------------------------------------------------------------
C   PSCALE reads a floating point map file extracting one point per row
C   and writes a image out.
C   Inputs:
C      IOFF    I       Pixel in row to extract (1-rel)
C      ISCR    I       Scratch file number in CFIL common
C      WINI    I(4)    Input window
C      NEWCNO  I       Output catalog number
C      DISKO   I       Output disk number
C      WINO    I(4)    Output Window
C      JBUFSZ  I       Buffer size in bytes
C      PMAX    R(22)   Max values by columns
C      PMIN    R(22)   Min values by columns
C      BLNKD   L       Image is blanked
C   Output:
C      IERR    I       0 -> ok, else IO error
C      CATBLK in common: change max/min and scaling and blanking
C      Buffers in common
C-----------------------------------------------------------------------
      INTEGER   IOFF, ISCR, WINI(4), NEWCNO, DISKO, WINO(4), JBUFSZ,
     *   IERR
      REAL      PMAX(22), PMIN(22)
C
      CHARACTER PHNAME*48
      LOGICAL   BLNKD, T
      INTEGER   CATBLK(256), NXO, L3, L4, L5, L6, L7, I2, I3, I4, I5,
     *   I6, I7, J, LUNI, LUNO, INDI, INDO, IPOS(8), NAXT(8), INDIM,
     *   BOTEMP, OBIND, IBIND, L, JERR, SCRTCH(512)
      INCLUDE 'INCS:PMAD.INC'
      REAL      CATR(256), BUFF1(MABFSS), BUFF2(MABFSS)
      DOUBLE PRECISION    CATD(128)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      COMMON /MAPHDR/ CATBLK
      COMMON /BUFRS/ BUFF1, BUFF2, SCRTCH
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA T /.TRUE./
      DATA LUNI, LUNO /16, 17/
C-----------------------------------------------------------------------
C                                       Set maxima, clear blanking
      CATR(KRDMX) = PMAX(IOFF)
      CATR(KRDMN) = PMIN(IOFF)
      CATR(KRBLK) = 0.0
C                                       Floating output
      IF (BLNKD) CATR(KRBLK) = FBLANK
C                                       loop limits
      L3 = CATBLK(KINAX+1)
      L4 = CATBLK(KINAX+2)
      L5 = CATBLK(KINAX+3)
      L6 = CATBLK(KINAX+4)
      L7 = CATBLK(KINAX+5)
      NXO = WINO(3)
C                                       Open files
      CALL ZPHFIL ('SC', SCRVOL(ISCR), SCRCNO(ISCR), 1, PHNAME, IERR)
      CALL ZOPEN (LUNI, INDI, SCRVOL(ISCR), PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, PHNAME, IERR)
      CALL ZOPEN (LUNO, INDO, DISKO, PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       Prepare to read
      IPOS(8) = 1
      CALL COPY (7, CATBLK(KINAX), NAXT(2))
      NAXT(1) = WINI(3)
      INDIM = CATBLK(KIDIM) + 1
C                                       loop
      DO 700 I7 = 1,L7
         IPOS(7) = I7
         DO 600 I6 = 1,L6
            IPOS(6) = I6
            DO 500 I5 = 1,L5
               IPOS(5) = I5
               DO 400 I4 = 1,L4
                  IPOS(4) = I4
C                                       Init output
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(4), BOTEMP, IERR)
      BOTEMP = BOTEMP + 1
      CALL MINIT ('WRIT', LUNO, INDO, WINO(3), WINO(4), WINO, BUFF2,
     *   JBUFSZ, BOTEMP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 970
         END IF
      DO 300 I3 = 1,L3
         IPOS(3) = I3
         CALL COMOFF (INDIM, NAXT, IPOS(3), BOTEMP, IERR)
         BOTEMP = BOTEMP + 1
         CALL MINIT ('READ', LUNI, INDI, WINI(3), WINI(4), WINI, BUFF1,
     *      JBUFSZ, BOTEMP, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1110) IERR
            GO TO 970
            END IF
C                                       Init a write
         CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1120) 'WRIT', IERR
            GO TO 970
            END IF
C                                       Loop thru input plane
         DO 200 I2 = 1,NXO
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1120) 'READ', IERR
               GO TO 970
               END IF
            J = IBIND + IOFF - 1
            L = OBIND + I2 - 1
            BUFF2(L) = BUFF1(J)
 200        CONTINUE
 300     CONTINUE
C                                       Flush output plane
      CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1120) 'FINI', IERR
         GO TO 970
         END IF
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
      GO TO 975
C                                       Close down (error)
 970  CALL MSGWRT (8)
C                                       Close files
 975  CALL ZCLOSE (LUNO, INDO, JERR)
 980  CALL ZCLOSE (LUNI, INDI, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('PSCALE: ERROR',I5,' OPENING SCRATCH FILE')
 1035 FORMAT ('PSCALE: ERROR',I5,' OPENING MAP FILE')
 1100 FORMAT ('PSCALE: ERROR',I5,' ON INIT MAP FILE')
 1110 FORMAT ('PSCALE: ERROR',I5,' ON INIT SCRATCH FILE')
 1120 FORMAT ('PSCALE: ',A4,' ERROR',I5)
      END
      SUBROUTINE IMLIHI (ITYP, NCN)
C-----------------------------------------------------------------------
C   IMLIHI copies and updates history file.
C   Inputs:
C      ITYP   I   Output map type: 0 => residual
C                    1 => answers (get 1st axis info also)
C      NCN    I   Position in FILES common on catlgd file
C-----------------------------------------------------------------------
      INTEGER   ITYP, NCN
C
      INTEGER   LUN1, LUN2, IERR, I, IG
      CHARACTER HILINE*72, LABEL*8, NOTTYP*2
      LOGICAL   T
      INCLUDE 'IMLIN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'  '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   SCRTCH(257), SCRTCH, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 50
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) GO TO 50
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       BLC
      WRITE (HILINE,2000) TSKNAM, BLC
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       TRC
      WRITE (HILINE,2001) TSKNAM, TRC
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       other Parms
      WRITE (HILINE,2002) TSKNAM, YINC, ZINC
      IF ((YINC.GE.2.) .OR. (ZINC.GE.2.)) THEN
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         END IF
      IF (IERR.NE.0) GO TO 50
      CALL H2CHR (8, 1, OLDH(KHBUN), LABEL)
      WRITE (HILINE,2003) TSKNAM, FCUT, LABEL
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Baseline limits
      IG = PORDER + 0.01
      WRITE (HILINE,2005) TSKNAM, IG
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       list windows in batch-like
      DO 15 I = 1,NBOXES
         WRITE (HILINE,2006) TSKNAM, NBOX(1,I), NBOX(2,I), I
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
 15      CONTINUE
C                                       Old axis 1
      IF (ITYP.GT.0) THEN
         CALL H2CHR (8, 1, OLDH(KHCTP), LABEL)
         WRITE (HILINE,2020) TSKNAM, LABEL
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2021) TSKNAM, CATOLD(KINAX)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2022) TSKNAM, OLDR(KRCRP)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2023) TSKNAM, OLDR(KRCIC)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2024) TSKNAM, OLDD(KDCRV)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         END IF
C                                       Close HI file
 50   CALL HICLOS (LUN2, T, SCRTCH, IERR)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, SCRTCH(257), SCRTCH, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK and close
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'CLWR', SCRTCH, IERR)
      FRW(NCN) = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMLIHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,' BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,' TRC =',6(F6.0,','),F6.0)
 2002 FORMAT (A6,' YINC =',F6.0,'  ZINC =',F6.0)
 2003 FORMAT (A6,' FLUX =',1PE12.4,14X,'/ Flux cutoff in ',A8)
 2005 FORMAT (A6,' PORDER =',I2,22X,'/ Max polynomial fit')
 2006 FORMAT (A6,' BOXES =',2I6,12X,'/ Window',I2,' wrt BLC(1)')
 2020 FORMAT (A6,' CTYPE1  = ''',A8,'''',12X,'/ old axis 1')
 2021 FORMAT (A6,' NAXIS1  = ',I6,16X,'/ Old axis 1')
 2022 FORMAT (A6,' CRPIX1  = ',F9.3,13X,'/ Old axis 1')
 2023 FORMAT (A6,' CDELT1  = ',1PE13.5,9X,'/ Old axis 1')
 2024 FORMAT (A6,' CRVAL1  = ',1PE18.10,4X,'/ Old axis 1')
      END
      SUBROUTINE SUBHD3 (BLC, TRC, XINC, YINC, ZINC)
C-----------------------------------------------------------------------
C   SUBHD3 corrects the header for subimaging: changes number of points
C   on the axes, the reference pixels, and the alternate axis (freq vs
C   velocity) reference pixel.  It corrects the first 3 axes for use
C   of pixel increments - namely the number of pixels, the reference
C   pixel and the axis increment.
C   Inputs:
C      BLC    R(7)   Bottom left corner to use
C      TRC    R(7)   Top right corner to use
C      XINC   R      Pixel increment on first axis
C      YINC   R      Pixel increment on second axis
C      ZINC   R      PIxel increment on third axis
C   Common /MAPHDR/ CATBLK     map header (in/out)
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7), XINC, YINC, ZINC
C
      CHARACTER FCHARS(3)*4, CHTM12*12
      REAL      CATR(256), AINC(7)
      INTEGER   IPL, IPH, CATBLK(256), NAX, I, J
      LOGICAL   EQUAL
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
C                                       Regular axis parameters
      NAX = CATBLK(KIDIM)
      CALL RFILL (7, 1.0, AINC)
      IF (XINC.GT.0.0) AINC(1) = XINC
      IF (YINC.GT.0.0) AINC(2) = YINC
      IF (ZINC.GT.0.0) AINC(3) = ZINC
      DO 10 I = 1,NAX
         IPL = BLC(I) + 0.01
         IPH = TRC(I) + 0.01
         CATBLK(KINAX+I-1) = (IPH - IPL) / AINC(I) + 1
         CATR(KRCRP+I-1) = (CATR(KRCRP+I-1) - IPL) / AINC(I) + 1.
         CATR(KRCIC+I-1) = CATR(KRCIC+I-1) * AINC(I)
 10      CONTINUE
C                                       Alternate axis
      IF (CATBLK(KIALT).NE.0) THEN
         DO 25 I = 1,NAX
            IPL = KHCTP + (I-1)*2
            DO 20 J = 1,3
               CALL H2CHR (4, 1, CATR(IPL), CHTM12)
               EQUAL = FCHARS(J)(1:4).EQ.CHTM12(1:4)
C                                       Found one
               IF (EQUAL) THEN
                  IPL = BLC(I) + 0.01
                  CATR(KRARP) = (CATR(KRARP) - IPL) / AINC(I) + 1.0
                  GO TO 999
                  END IF
 20            CONTINUE
 25         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE POLYIN (NDATA, IERR)
C-----------------------------------------------------------------------
C   POLYIN prepares the parameters of a set of orthogonal polynomials.
C   All are carried in COMMON /GDATA/.
C   Input:
C      NDATA   I   Number of points in data array
C   Output:
C      IERR    I   0 => okay, 1 => no valid data, 2 => other singularity
C-----------------------------------------------------------------------
      INTEGER   NDATA, IERR
C
      REAL      PP, AL, SUM, TEMP
      INTEGER   MMAX, I, J, K, JJ, KK, N, MM
      INCLUDE 'IMLIN2.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       calculate moments
      MMAX = 2 * JJC - 1
      CALL RFILL (MMAX, 0.0, MOMENT)
      NPTS = 0
      DO 20 I = 1,NDATA
         IF ((MASK(I).EQ.1) .AND. (DATA(I).NE.FBLANK)) THEN
            PP = 1
            AL = I + XBAR
            NPTS = NPTS + 1
            DO 15 J = 2,MMAX
               PP = PP * AL
               MOMENT(J) = MOMENT(J) + PP
 15            CONTINUE
            END IF
 20      CONTINUE
      IERR = 1
      IF (NPTS.LE.0) GO TO 999
      IERR = 2
      IF (NPTS.GT.1) THEN
         J = 6
      ELSE
         J = 3
         END IF
      DO 25 J = 2,MMAX
         MOMENT(J) = MOMENT(J) / NPTS
 25      CONTINUE
      MOMENT(1) = 1.0
C                                       Matrix: P(K) = G(K) * (X**K
C                                        - SUM (A(K,J)*P(J)))
      MMAX = 25
      CALL RFILL (MMAX, 0.0, AARRAY)
      CALL RFILL (MMAX, 0.0, CARRAY)
      GAMMA(1) = 1.0
      DO 50 K = 2,JJC
         SUM = 0.0
         KK = K - 1
         DO 40 J = 1,KK
            AARRAY(K,J) = MOMENT(J+K-1)
            IF (J.NE.1) THEN
               JJ = J - 1
               DO 30 MM = 1,JJ
                  AARRAY(K,J) = AARRAY(K,J) - AARRAY(J,MM)*AARRAY(K,MM)
 30               CONTINUE
               END IF
            AARRAY(K,J) = AARRAY(K,J) * GAMMA(J)
            SUM = SUM + AARRAY(K,J) ** 2
 40         CONTINUE
         TEMP = MOMENT(2*K-1) - SUM
         IF (TEMP.LE.0.0) GO TO 999
         GAMMA(K) = 1.0 / SQRT (TEMP)
 50      CONTINUE
C                                       Matrix: P(K) = SUM (C(K,J) *
C                                                      X**J)
      CARRAY(1,1) = GAMMA(1)
      DO 65 K = 2,JJC
         CARRAY(K,K) = GAMMA(K)
         KK = K - 1
         DO 60 MM = 1,KK
            DO 55 N = MM,KK
               CARRAY(K,MM) = CARRAY(K,MM) - GAMMA(K) * AARRAY(K,N)
     *            * CARRAY(N,MM)
 55            CONTINUE
 60         CONTINUE
 65      CONTINUE
C                                       average of polynomials
      MMAX = 5
      CALL RFILL (MMAX, 0.0, POLAVG)
      DO 75 I = 1,NDATA
         CALL POLYEV (I)
         DO 70 J = 1,MMAX
            POLAVG(J) = POLAVG(J) + POLYFN(J)
            POLXFN(I,J) = POLYFN(J)
 70         CONTINUE
 75      CONTINUE
      DO 80 J = 1,MMAX
         POLAVG(J) = POLAVG(J) / NDATA
 80      CONTINUE
      IERR = 0
C
 999  RETURN
      END
      SUBROUTINE POLYEV (IX)
C-----------------------------------------------------------------------
C   POLYEV evaluates the orthogonal polynomials at the X value given.
C   Inputs:
C      IX      I       X position
C-----------------------------------------------------------------------
      INTEGER   IX
C
      REAL      AX
      INTEGER   J, K, KK
      INCLUDE 'IMLIN2.INC'
C-----------------------------------------------------------------------
      POLYFN(1) = 1.0
      AX = IX + XBAR
      DO 20 K = 2,JJC
         POLYFN(K) = AX**(K-1)
         KK = K-1
         DO 10 J = 1,KK
            POLYFN(K) = POLYFN(K) - AARRAY(K,J) * POLYFN(J)
 10         CONTINUE
         POLYFN(K) = POLYFN(K) * GAMMA(K)
 20      CONTINUE
C
 999  RETURN
      END
