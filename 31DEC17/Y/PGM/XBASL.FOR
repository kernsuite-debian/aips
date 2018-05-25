LOCAL INCLUDE 'XBASL.INC'
C                                                          Include XBASL
      INCLUDE 'INCS:PMAD.INC'
C                                       Local include for XBASL
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7), YINC,
     *   ZINC, FCUT, DOCAT, DOCONT, DOSLIC, DOMODL, DORESI, RANGE(2),
     *   PLTYPE, PIXVAL, PORDER, XBOXES, BOXES(2,20), DOTV, GRCHAN,
     *   BADD(10)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      DOUBLE PRECISION OLDD(128)
      HOLLERITH OLDH(256)
      REAL      OLDR(256), PMIN(22), PMAX(22), BUFF1(MABFSS),
     *    BUFF2(MABFSS)
      INTEGER   CATOLD(256), SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, JBUFSZ, ICODE, NORDER, NBOXES, NBOX(2,20),
     *   IBUFF1(MABFSS), IBUFF2(MABFSS)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOU,
     *   XSEQO, XDISKO, BLC, TRC, YINC, ZINC, FCUT, DOCAT, DOCONT,
     *   DOSLIC, DOMODL, DORESI, RANGE, PLTYPE, PIXVAL, PORDER, XBOXES,
     *   BOXES, DOTV, GRCHAN, BADD
      COMMON /BSLCHR/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /PARMS/ CATOLD, PMAX, PMIN, SEQIN, SEQOUT, DISKIN,
     *   DISKO, NEWCNO, OLDCNO, JBUFSZ, ICODE, NORDER, NBOXES, NBOX
      COMMON /BUFRS/ BUFF1, BUFF2
      EQUIVALENCE (CATOLD, OLDR, OLDD, OLDH)
      EQUIVALENCE (IBUFF1, BUFF1),  (IBUFF2, BUFF2)
      INCLUDE 'INCS:DCAT.INC'
C                                                          End XBASL.
LOCAL END
LOCAL INCLUDE 'XBASL2.INC'
C                                                         Include XBASL2
      INCLUDE 'INCS:PMAD.INC'
C                                       Local include for XBASL
      INTEGER   NITTER, ITTER, JJC, MASK(MABFSS), NPTS, IGR1, IGR2,
     *   IGR3, IGR4, DEVON
      REAL      XBAR, DATA(MABFSS), AARRAY(5,5), CARRAY(5,5), GAMMA(5),
     *   MOMENT(15), POLYFN(5), POLAVG(5), POLXFN(MABFSS,5), IGNORD,
     *   SOLVED, UCHAN
      COMMON /GDATA/ DATA, AARRAY, CARRAY, GAMMA, MOMENT, POLYFN, XBAR,
     *   POLAVG, POLXFN, SOLVED, IGNORD, NPTS, NITTER, ITTER, JJC, MASK,
     *   UCHAN, IGR1, IGR2, IGR3, IGR4, DEVON
C                                                          End XBASL2
LOCAL END
      PROGRAM XBASL
C-----------------------------------------------------------------------
C! Fits polynomial baselines to rows of an image.
C# Map Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2005, 2007, 2009-2010, 2012
C;  Copyright (C) 2014-2015
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
C   XBASL fits n'th order polynomial baselines to rows of an image.
C   It writes out an n-dim cube with the baseline average added and the
C   baseline function subtracted and, if DOOUTPUT > 0, n-1 dimensional
C   images of the polynomial parameters and errors.  It will display
C   the data, model, and residual for each row on the TEK.  After each
C   fit so displayed, it asks for permission to keep the results.
C
C   CONTAINS: 1. Harvey Liszt math fitting methods.
C             2. Compute polynomials to array only at each change
C                of window.
C
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input image.
C      INCLASS        CLAIN         Class of input image.
C      INSEQ          SEQIN         Seq. of input image.
C      INDISK         DISKIN        Disk number of input image.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTCLASS       CLAOUT        Class of the output image.
C                                   Default is task name.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      BLC(7)         BLC           Bottom left corner of subimage
C                                   of input image.
C      TRC(7)         TRC           Top right corner of subimage.
C      YINC           YINC          Pixel increment on 2nd axis
C      ZINC           ZINC          Pixel increment on 3rd axis
C      FLUX           FCUT          Flux cutoff: > 2 consecutive
C                                   points must > FLUX to fit
C                                   Also in initial auto-guesses
C      DOCAT          DOCAT         Catalog the parameter images
C      DOSLICE        DOSLIC        Plot data on TEK
C      DOMODEL        DOMODL        Plot model on TEK
C      DORESID        DORESI        Plot residuals on TEK
C      LTYPE                        Type of labeling: 1 border,
C                                   2 no ticks, 3 standard, 4 rel
C                                   to center, 5 rel to subim cen
C                                   6 map pixels
C      PIXRANGE                     Min,Max of image intensity
C                                   Max <= Min => entire range
C      PIXVAL         PIXVAL        Display only if peak < PIXVAL
C      NITER          XNIT          Limit on iterations in fit
C      BADD(10)       IBAD          Disk numbers to avoid.
C   Programmer Eric W. Greisen based on his XGAUS  April 1984.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      LOGICAL   ABLANK
      INCLUDE 'XBASL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'XBASL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL XBASIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL XBASDO (ABLANK, IRET)
      IF (IRET.EQ.0) CALL XBASOU (ABLANK, IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE XBASIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   XBASIN gets input parameters for XBASL and creates an output file
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
      CHARACTER   STAT*4, BLANK*6, CTYPE(19)*4, BUNITS(9)*4, MTYPE*2,
     *   SEQTYP(2,8)*8, CORDER(6)*2, FLXTYP(2)*8, OTYPE*8
      INTEGER   IERR, NPARM, IROUND, NAX, I, ITYP, NTYP, IG, IP, INPSEQ
      INCLUDE 'XBASL.INC'
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
      DATA SEQTYP /'CONST   ', 'DCONST  ',
     *             'DERIV1  ', 'DDERV1  ',
     *             'DERIV2  ', 'DDERV2  ',
     *             'DERIV3  ', 'DDERV3  ',
     *             'DERIV4  ', 'DDERV4  ',
     *             'DERIV5  ', 'DDERV5  ',
     *             'DERIV6  ', 'DDERV6  ',
     *             'DERIV7  ', 'DDERV7  '/
      DATA CORDER /'22','33','44','55','66','77'/
      DATA FLXTYP /'FLUX    ', 'DFLUX   '/
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
      NPARM = 94
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Using the TEK?
      ICODE = -1
      IF ((DOSLIC.GT.0.) .OR. (DOMODL.GT.0.) .OR. (DORESI.GT.0.))
     *   ICODE = 1
      IF ((NPOPS.GT.NINTRN) .OR. (NTKDEV.LE.0)) ICODE = -1
C                                       Restart AIPS
      IF ((RQUICK) .AND. ((ICODE.LE.0) .OR. (IRET.NE.0))) CALL RELPOP
     *    (IRET, IBUFF1, IERR)
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
      DO 20 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 20      CONTINUE
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, STAT, IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'READ', IBUFF1,
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
      IF (IERR.NE.0) GO TO 995
C                                       Get user modification to CATBLK
      IRET = 4
      CALL BASHED (IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Create output file for residual
      IRET = 4
      NEWCNO = 1
      CALL MCREAT (DISKO, NEWCNO, IBUFF1, IERR)
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
      CALL COPY (256, CATBLK, IBUFF2)
      NORDER = PORDER + 1.01
      IF (NORDER.GT.5) NORDER = 5
C                                       Basic output header: results
      IF (DOCAT.LE.0.0) GO TO 180
         CATBLK(KIDIM) = CATBLK(KIDIM) - 1
         NAX = CATBLK(KIDIM)
         DO 80 I = 1,NAX
            CATBLK(KINAX+I-1) = CATBLK(KINAX+I)
            CATR(KRCRP+I-1) = CATR(KRCRP+I)
            CATR(KRCRT+I-1) = CATR(KRCRT+I)
            CATR(KRCIC+I-1) = CATR(KRCIC+I)
            CATD(KDCRV+I-1) = CATD(KDCRV+I)
            CALL CHCOPY (8, 1, CATH(KHCTP+I*2), 1, CATH(KHCTP+(I-1)*2))
 80         CONTINUE
         DO 85 I = NAX,6
            CATBLK(KINAX+I) = 1
 85         CONTINUE
C                                       Find type of old axis
         CALL H2CHR (8, 1, OLDH(KHCTP), OTYPE)
         DO 90 ITYP = 1,NTYP
            IF (OTYPE.EQ.CTYPE(ITYP)) GO TO 100
 90         CONTINUE
         ITYP = 0
         WRITE (MSGTXT,1090) OTYPE
         CALL MSGWRT (6)
 100     IF (ITYP.GT.7) ITYP = 8
         IF (ITYP.EQ.0) ITYP = 9
C                                       Output polynomial parms
         CATBLK(KIIMS) = INPSEQ
         DO 120 IG = 1,NORDER
            DO 120 IP = 1,2
               CALL CHR2H (6, SEQTYP(IP,IG), KHIMCO, CATR(KHIMC))
               CALL CHCOPY (8, 1, OLDH(KHBUN), 1, CATR(KHBUN))
               IF (IG.NE.1) CALL CHR2H (4, BUNITS(ITYP),
     *            5, CATR(KHBUN))
               IF (IG.GT.2) CALL CHR2H (1, CORDER(IG-2),
     *            8, CATR(KHBUN))
C                                       Create
               DISKO = XDISKO + 0.01
               NEWCNO = 1
               CALL MCREAT (DISKO, NEWCNO, IBUFF1, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1100) IERR, SEQTYP(IP,IG)
                  GO TO 990
                  END IF
C                                       Record the creation
               NCFILE = NCFILE + 1
               FVOL(NCFILE) = DISKO
               FCNO(NCFILE) = NEWCNO
               FRW(NCFILE) = 2
 120           CONTINUE
C                                       Flux maps
         DO 170 IP = 1,2
            CALL CHR2H (6, FLXTYP(IP), KHIMCO, CATR(KHIMC))
            CALL CHCOPY (4, 1, OLDH(KHBUN), 1, CATR(KHBUN))
C                                       Create
            DISKO = XDISKO + 0.01
            NEWCNO = 1
            CALL MCREAT (DISKO, NEWCNO, IBUFF1, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1100) IERR, FLXTYP(IP)
               GO TO 990
               END IF
C                                       Record the creation
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = DISKO
            FCNO(NCFILE) = NEWCNO
            FRW(NCFILE) = 2
 170        CONTINUE
C                                       close this one up
 180  IRET = 0
      NORDER = NORDER - 1
      CALL COPY (256, IBUFF2, CATBLK)
      DISKO = FVOL(2)
      NEWCNO = FCNO(2)
      GO TO 999
C
 990  CALL MSGWRT (8)
C                                       Restart AIPS
 995  IF ((RQUICK) .AND. (IRET.NE.0) .AND. (ICODE.GT.0)) CALL RELPOP
     *   (IRET, IBUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XBASIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT RESIDUALS FILE')
 1090 FORMAT ('AXIS TYPE ',A8,' DOES NOT HAVE KNOWN UNITS')
 1100 FORMAT ('ERROR',I5,' CREATING FILE TYPE ',A6)
      END
      SUBROUTINE XBASDO (TBLNKD, IRET)
C-----------------------------------------------------------------------
C   XBASDO sends image one row at a time to the baseline fitting routine
C   and then writes the modified data.
C   Output:
C      TBLNKD   L   Answers contain blanked pixels?
C      IRET     I   Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      LOGICAL   TBLNKD
      INTEGER   IRET
C
      INCLUDE 'XBASL.INC'
      INCLUDE 'XBASL2.INC'
      CHARACTER IFILE*48, PHNAME*48
      INTEGER   IROUND, LUNI, LUNO, NYI, NXI, WINI(4), NXO, NYO, BOI, J,
     *   WINO(4), BOO, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2, I3,
     *   I4, I5, I6, I7, IPOS(7), CORN(7), BOTEMP, KOFF, LIMO, LIMIT,
     *   IBIND, OBIND, INDI, INDO, LIM1, IG, WINT(4), LUNT, INDT, SIZE,
     *   NAXT(7), OBINDT, IBUFF3(MABFSS), JBUFS3, IINC2, IINC3, IERR, I
      REAL      PLTODO, PLDONE, OUTMAX, OUTMIN, BUFF3(MABFSS), XPLD
      LOGICAL   T, F, BLNKD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (IBUFF3, BUFF3)
      DATA LUNI, LUNO, LUNT /16,17,18/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      OBINDT = 1
C                                       Note: CATOLD & CATBLK must be
C                                       switched in their addresses
C                                       Move the data that way
      CALL COPY (256, CATOLD, IBUFF2)
      CALL COPY (256, CATBLK, CATOLD)
      CALL COPY (256, IBUFF2, CATBLK)
      JBUFS3 = 2 * MABFSS
      IGNORD = 0.0
      SOLVED = 0.0
C                                       pass TV parameter to include 2
      UCHAN = GRCHAN
      DEVON = 0
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Create scratch files.
C                                       For answers: temp scratch
      IF (DOCAT.GT.0.0) THEN
         CALL COPY (7, CATOLD(KINAX), NAXT)
         NAXT(1) = PORDER + 2.1
         NAXT(1) = NAXT(1) * 2
         CALL MAPSIZ (CATOLD(KIDIM), NAXT, SIZE)
         CALL SCREAT (SIZE, IBUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
         CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, PHNAME, IERR)
         CALL ZOPEN (LUNT, INDT, SCRVOL(NSCR), PHNAME, T, T, T, IRET)
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
C                                       baseline windows
      JJC = NORDER + 1
      CALL FILL (40, 0, NBOX)
      NBOXES = XBOXES + 0.01
      IG = TRC(1) - BLC(1) + 1.01
      I1 = BLC(1) - 0.99
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
         IF (NBOX(2,I).LT.NBOX(1,I)) NBOX(2,I) = IG
         J = NBOX(2,I) - NBOX(1,I) + 1
         CALL FILL (J, 1, MASK(NBOX(1,I)))
 55      CONTINUE
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
                     IF ((ICODE.EQ.1) .OR. (ABS(XPLD-PLDONE).LE.0.1))
     *                  THEN
                        WRITE (MSGTXT,1090) PLDONE, PLTODO
                        CALL MSGWRT (1)
                        END IF
C                                       Init. files, first input.
                     CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3),
     *                   BOTEMP, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1099) IRET
                        GO TO 990
                        END IF
                     BOI = BOTEMP + 1
                     CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI,
     *                  BUFF1, JBUFSZ, BOI, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1100) 'READ', IRET
                        GO TO 990
                        END IF
C                                       Init answer file.
                     IF (DOCAT.GT.0.0) THEN
                        CALL COMOFF (CATOLD(KIDIM), NAXT, CORN(3),
     *                     BOTEMP, IRET)
                        BOO = BOTEMP + 1
                        CALL MINIT ('WRIT', LUNT, INDT, NAXT, NAXT(2),
     *                     WINT, BUFF3, JBUFS3, BOO, IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1100) 'WRIT', IRET
                           GO TO 990
                           END IF
                        END IF
C                                       Init output file.
                     CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), CORN(3),
     *                  BOTEMP, IRET)
                     BOO = BOTEMP + 1
                     CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO,
     *                  BUFF2, JBUFSZ, BOO, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1100) 'WRIT', IRET
                        GO TO 990
                        END IF
                     DO 250 I2 = 1,LIM2
                        IPOS(2) = BLC(2) + I2 - 0.9
                        IPOS(1) = IROUND (BLC(1))
C                                       Read.
                        CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1120) 'READ', IRET
                           GO TO 990
                           END IF
C                                       Want this row?
                        IF (MOD(I2-1,IINC2).EQ.0) THEN
C                                       Copy to buffer.
                           DO 160 I1 = 1,LIM1
                              DATA(I1) = BUFF1(IBIND+I1-1)
 160                          CONTINUE
C                                       Write.
                           IF (DOCAT.GT.0.0) THEN
                              CALL MDISK ('WRIT', LUNT, INDT, BUFF3,
     *                           OBINDT, IRET)
                              IF (IRET.NE.0) THEN
                                 WRITE (MSGTXT,1120) 'WRIT', IRET
                                 GO TO 990
                                 END IF
                              END IF
                           CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND,
     *                        IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1120) 'WRIT', IRET
                              GO TO 990
                              END IF
C                                       Call DO1BAS
                           CALL DO1BAS (IPOS, BUFF3(OBINDT),
     *                        BUFF2(OBIND), IRET)
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
 200                          CONTINUE
                           IF ((.NOT.TBLNKD) .AND. (DOCAT.GT.0.0)) THEN
                              LIMIT = OBINDT + WINT(3) - 1
                              DO 210 I1 = OBINDT,LIMIT
                                 TBLNKD = (BUFF3(I1).EQ.FBLANK) .OR.
     *                              (TBLNKD)
 210                             CONTINUE
                              END IF
                           END IF
 250                    CONTINUE
C                                       Flush buffers.
                     IF (DOCAT.GT.0.0) THEN
                        CALL MDISK ('FINI', LUNT, INDT, BUFF3, OBINDT,
     *                     IRET)
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
                     CALL CATIO ('UPDT', DISKO, NEWCNO, CATOLD, 'REST',
     *                  DATA, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1260) IRET
                        GO TO 990
                        END IF
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
      WRITE (MSGTXT,1700) SOLVED, IGNORD
      CALL MSGWRT (7)
C                                       Resume AIPS
      IRET = 0
      IF ((RQUICK) .AND. (ICODE.GT.0)) CALL RELPOP (IRET, BUFF1, IERR)
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
      CALL COPY (256, CATOLD, IBUFF2)
      CALL COPY (256, CATBLK, CATOLD)
      CALL COPY (256, IBUFF2, CATBLK)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C                                       Resume AIPS
      IF ((RQUICK) .AND. (ICODE.GT.0)) CALL RELPOP (IRET, IBUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XBASDO: ERROR',I3,' OPENING INPUT FILE')
 1010 FORMAT ('XBASDO: ERROR',I3,' CREATING ANSWER SCRATCH FILE')
 1015 FORMAT ('XBASDO: ERROR',I3,' OPENING ANSWER SCRATCH FILE')
 1020 FORMAT ('XBASDO: ERROR',I5,' OPENING OUTPUT FILE')
 1090 FORMAT ('Begin plane',F8.0,' of',F8.0)
 1099 FORMAT ('XBASDO: COMOFF ERROR',I3)
 1100 FORMAT ('XBASDO: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('XBASDO: ',A4,' ERROR',I3)
 1180 FORMAT ('XBASDO: DO1BAS ERROR',I3)
 1181 FORMAT ('QUITTING AT USER REQUEST')
 1260 FORMAT ('XBASDO: CATIO ERROR',I3,' UPDATING CATBLK')
 1700 FORMAT ('Solved in',F12.0,' rows, simply copied',F12.0,' rows')
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
      CHARACTER FCHARS(3)*4, CHTM12*12
      INTEGER   I, IRET
      INCLUDE 'XBASL.INC'
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
      IF (FCUT.LE.0.0) FCUT = 0.00005
      IF (PIXVAL.LE.FCUT) PIXVAL = 100. * FCUT
C                                       Check polynomial order
      PORDER = MAX (0.0, PORDER)
      IF (PORDER.GT.4.0) THEN
         WRITE (MSGTXT,1030) PORDER
         CALL MSGWRT (6)
         PORDER = 4.0
         END IF
C                                       Check input size
      IRET = 0
      IF (TRC(1)-BLC(1).GT.MAXIMG) THEN
         WRITE (MSGTXT,1040) MAXIMG
         CALL MSGWRT (8)
         IRET = 4
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('WARNING: FIRST AXIS NOT FREQUENCY OR VELOCITY')
 1030 FORMAT ('ORDER=',F7.3,' TOO LARGE: REDUCED TO 4.0')
 1040 FORMAT ('WORKS ONLY ON (SUB)ROWS <=',I6,' PIXELS')
      END
      SUBROUTINE DO1BAS (IPOS, RESULT, RESIDS, IRET)
C-----------------------------------------------------------------------
C   DO1BAS fits baselines to a row of an image and returns the
C   answers in RESULT and the residuals in RESIDS (if DOCAT > 0).
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
      INCLUDE 'XBASL.INC'
      INCLUDE 'XBASL2.INC'
      INTEGER   ING, INPARM, INPTS, LABEL, TERR, LCODE, I, IERR, OUPARM,
     *    NTRY, LERR, J
       REAL      PARMS(9), FVEC(MABFSS), ORANGE(2), AVER
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVS.INC'
C-----------------------------------------------------------------------
      IRET = 0
      NTRY = 0
      LABEL = ABS (PLTYPE)
      IF (MOD(LABEL,100).LE.0) LABEL = (LABEL/100)*100 + 3
      PLTYPE = LABEL
C                                       Not last call
      IF (IPOS(1).GE.0) THEN
C                                       Get the initial guess
 10      NTRY = NTRY + 1
         LCODE = ICODE
         ING = PORDER + 0.1
         INPARM = ING + 1
         OUPARM = 2 * INPARM + 2
         INPTS = TRC(1) - BLC(1) + 1.01
         ITTER = 0
         NITTER = 100
         XBAR = IPOS(1) - 1 - CATR(KRCRP)
         TERR = 0
         CALL POLYIN (INPTS, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL XBASGE (NORDER, INPTS, FCUT, LCODE, PIXVAL, PARMS, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Plot it
         IF (LCODE.EQ.1) THEN
            IF (DOTV.GT.0.0) THEN
               CALL BTVINI (IPOS, INPTS, NORDER, LABEL, RANGE, DOSLIC,
     *            PARMS, NBOXES, NBOX, PORDER, ORANGE, TERR)
            ELSE
               CALL BTKINI (IPOS, INPTS, NORDER, LABEL, RANGE, DOSLIC,
     *            PARMS, NBOXES, NBOX, PORDER, ORANGE, FVEC, TERR)
               END IF
            JJC = NORDER + 1
            IF ((NTRY.GT.1) .AND. (TERR.LT.100)) TERR = 103
            IF (TERR.EQ.102) THEN
               IRET = 99
               GO TO 990
            ELSE IF (TERR.EQ.101) THEN
               GO TO 900
C                                       Redo guess
            ELSE IF (TERR.GE.100) THEN
               TERR = 0
               IF (DOTV.GT.0.0) THEN
                  CALL BTVGUS (INPTS, ORANGE, NBOXES, NBOX, TERR)
               ELSE
                  CALL BTKGUS (INPTS, ORANGE, NBOXES, NBOX, FVEC, TERR)
                  END IF
               IF (TERR.GT.100) GO TO 900
C                                       reset parms
               CALL FILL (INPTS, 0, MASK)
               DO 15 I = 1,NBOXES
                  IF ((NBOX(1,I).LT.1) .OR. (NBOX(2,I).GT.INPTS))
     *               GO TO 15
                     J = NBOX(2,I) - NBOX(1,I) + 1
                     CALL FILL (J, 1, MASK(NBOX(1,I)))
 15               CONTINUE
               NPTS = 0
               CALL POLYIN (INPTS, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
            END IF
C                                       Fit baselines
         CALL XBALMS (INPTS, PARMS)
C                                       Get errors and nice units
         CALL XBASFI (INPARM, PARMS, RESULT)
         I = 1
         ITTER  = ITTER - 1
         CALL XBFUNC (INPTS, PARMS, FVEC)
         IF ((LCODE.EQ.1) .AND. (TERR.LE.0)) THEN
            IF (DOTV.GT.0.0) THEN
               CALL BTVMOD (DOMODL, DORESI, INPTS, ING, ORANGE, FVEC,
     *            LERR, TERR)
            ELSE
               CALL BTKMOD (DOMODL, DORESI, INPTS, ING, ORANGE, FVEC,
     *            LERR, TERR)
               END IF
            IF (TERR.EQ.101) THEN
               GO TO 900
            ELSE IF (TERR.EQ.102) THEN
               IRET = 99
               GO TO 990
            ELSE IF (TERR.EQ.103) THEN
               GO TO 10
               END IF
            END IF
C                                       Fill residual values
         AVER = RESULT(INPARM+1)
         IF (DOCONT.LE.0.0) AVER = 0.0
         DO 40 I = 1,INPTS
            IF (DATA(I).EQ.FBLANK) THEN
               RESIDS(I) = FBLANK
            ELSE
               RESIDS(I) = FVEC(I) + AVER
               END IF
 40         CONTINUE
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
         GO TO 990
C                                       Blank outputs
 900     IF (DOCAT.GT.0.) CALL RFILL (OUPARM, FBLANK, RESULT)
         DO 910 I = 1,INPTS
            RESIDS(I) = DATA(I)
 910        CONTINUE
         IGNORD = IGNORD + 1.0
         END IF
C                                       close TV on error
 990  IF (DEVON.EQ.2) THEN
         CALL TVCLOS (INBUF, IERR)
      ELSE IF (DEVON.EQ.1) THEN
         CALL ZTKCLS (IERR)
         END IF
      DEVON = 0
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
      INTEGER   I, J
      REAL      YBAR, YBAR2
      INCLUDE 'XBASL2.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       clear sum variables
      DO 10 I = 1,9
         PARMS(I) = 0.0
 10      CONTINUE
      YBAR = 0.0
      YBAR2 = 0.0
C                                       sum: data, data**2
C                                       data*polyfunc(j)
      DO 30 I = 1,INPTS
         IF ((MASK(I).EQ.1) .AND. (DATA(I).NE.FBLANK)) THEN
            YBAR = YBAR + DATA(I)
            YBAR2 = YBAR2 + DATA(I)**2
            DO 20 J = 1,JJC
               PARMS(J) = PARMS(J) + DATA(I) * POLXFN(I,J)
 20            CONTINUE
            END IF
 30      CONTINUE
C                                       average
C                                       sigma**2=ybar2-sum(parms**2)
      YBAR = YBAR / NPTS
      YBAR2 = YBAR2 / NPTS
      PARMS(9) = YBAR2
      DO 40 J = 1,JJC
         PARMS(J) = PARMS(J) / NPTS
         PARMS(9) = PARMS(9) - PARMS(J)**2
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE XBASGE (NG, ND, FC, LCODE, PIXVAL, PARMS, IERR)
C-----------------------------------------------------------------------
C   XBASGE obtains an initial guess for the parameters of the baseline.
C   It sets a linear one only.
C   Inputs:
C      NG       I         order of polynomial
C      ND       I         Number of data samples
C      FC       R         Flux cutoff
C      PIXVAL   R         No plot if peak > PIXVAL
C   In/out:
C      LCODE    I         in: 1 => plot possible, out: 1 => wanted
C   Output:
C      PARMS    R(NG+1)   Guess to use
C      IERR     I         0 => ok, 1 => all data too low
C                         2 => input error
C-----------------------------------------------------------------------
      INTEGER   NG, ND, LCODE, IERR
      REAL      FC, PIXVAL, PARMS(9)
C
      INTEGER   IM, I, NS
      REAL      X, XM, R, BLS, BLO, BL, BLP, BLM, SD, SDX, SX, SXX
      INCLUDE 'XBASL2.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Fit baseline
      BLS = 0.
      BLO = 0.
      DO 10 I = 1,9
         PARMS(I) = 0.0
 10      CONTINUE
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
      IF (NS.LT.NG+1) THEN
         IERR = 3
         GO TO 999
         END IF
      R = NS * SXX - SX * SX
      IF (R.NE.0.0) BLS = (NS * SDX - SD * SX) / R
      IF (NS.GT.0) BLO = (SD - BLS * SX) / NS
      PARMS(1) = BLO
      PARMS(2) = BLS
C                                       Are there 3 big enough
C                                       and do we want to plot
      IM = 0
      XM = 0.
      DO 40 I = 2,ND
         IF ((I.NE.ND) .AND. (DATA(I).NE.FBLANK)) THEN
            R = I + XBAR
            BL = DATA(I) - BLO - BLS * R
            IF ((ABS(BL).GE.FC) .AND. (DATA(I-1).NE.FBLANK) .AND.
     *         (DATA(I+1).NE.FBLANK)) THEN
               BLM = DATA(I-1) - BLO - BLS * (R-1.)
               BLP = DATA(I+1) - BLO - BLS * (R+1.)
               IF (((BL.GT.0.0) .AND. (BLP.GE.FC) .AND. (BLM.GE.FC))
     *            .OR. ((BL.LT.0.0) .AND. (-BLP.GE.FC) .AND.
     *            (-BLM.GE.FC))) THEN
                  X = ABS (BLP + BL + BLM)
                  IF (X.GT.XM) THEN
                     XM = X
                     IM = I
                     END IF
                  END IF
               END IF
            END IF
 40      CONTINUE
C                                       Find anything?
      IF (IM.LT.1) THEN
         IERR = 1
         LCODE = 0
C                                       test desire to plot
      ELSE
         IERR = 0
         IF (XM/3.0.GT.PIXVAL) LCODE = 0
         END IF
C
 999  RETURN
      END
      SUBROUTINE XBASFI (NP, PARMS, RESULT)
C-----------------------------------------------------------------------
C   XBASFI determines the errors in the fit and converts the results
C   to normal units for output.
C   Inputs:
C      NP       I       Number of parameters (max) - JJC gives number
C                          fit this time
C      PARMS    R(9)    Answers from XBALMS
C   Output:
C      RESULT   R(18)   Answers then errors in normal units
C-----------------------------------------------------------------------
      INTEGER   NP
      REAL      PARMS(9), RESULT(18)
C
      INTEGER   I, JC, L, J
      DOUBLE PRECISION    OLDD(128)
      REAL      OLDR(256), R, AVER
      INTEGER   CATOLD(256)
      INCLUDE 'XBASL2.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATOLD
      EQUIVALENCE (CATOLD, OLDR, OLDD)
C-----------------------------------------------------------------------
C                                       Convert to output
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
 10            CONTINUE
            RESULT(I) = RESULT(I) * R
            RESULT(L) = PARMS(9) * SQRT (RESULT(L)/NPTS) * ABS(R)
            R = R / OLDR(KRCIC)
            END IF
 20      CONTINUE
C                                       flux
      R = 0.0
      AVER = 0.0
      DO 30 J = 1,JJC
         AVER = AVER + PARMS(J) * POLAVG(J)
         R = R + POLAVG(J) ** 2
 30      CONTINUE
      RESULT(JC) = AVER
      RESULT(2*JC) = PARMS(9) * SQRT (R/NPTS)
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
      INCLUDE 'XBASL2.INC'
C-----------------------------------------------------------------------
C                                       Determine difference between
C                                       data and current fit.
      DO 20 I = 1,NDATA
         FVEC(I) = FBLANK
         IF (DATA(I).NE.FBLANK) THEN
            SUM = 0.0
            DO 10 J = 1,JJC
               SUM = SUM + PARMS(J) * POLXFN(I,J)
 10            CONTINUE
            FVEC(I) = DATA(I) - SUM
            END IF
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE BTKINI (IPOS, INPTS, NORDER, LABEL, PIXR, DOSLIC,
     *   PARMS, NBOXES, NBOX, PORDER, ORANGE, FVEC, IERR)
C-----------------------------------------------------------------------
C   BTKINI initializes the TEK for a XBASL plot, plots axis labels,
C   and, if requested, plots the data.
C   Inputs:
C      IPOS     I(7)      Position in cube first point in row.
C      INPTS    I         Number of points in row.
C      LABEL    I         Requested label type
C      PIXR     R(2)      Requested plot value range
C      DOSLIC   R         > 0. => plot data
C      PARMS    R(10)     Initial guess
C      NBOXES   I         # windows
C      NBOX     I(2,20)   windows
C      PORDER   R         Max polynomial order
C   Output:
C      NORDER   I         Order of polynomial to use now
C      ORANGE   R(2)      Actual plot range in plot units
C      FVEC     R(*)      Scratch buffer
C      IERR     I         > 0 => plot failed
C                         101 => bad initial guess
C                         102 => DIE
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), INPTS, NORDER, LABEL, NBOXES, NBOX(2,20), IERR
      REAL      PIXR(2), DOSLIC, PARMS(9), PORDER, ORANGE(2), FVEC(*)
C
      CHARACTER TEMP*1, TEXT(2)*80, MSGBUF*132, NTEXT*80
      REAL      XBLC(7), XTRC(7), PBLC(2), PTRC(2), YGAP, CH(4), XYRATO,
     *   X, XX, Y, XFAC, XOFF, DX, DY, Y1, Y2, X1,  X2, Y3, Y4, FQFINC
      DOUBLE PRECISION FQFREQ
      INTEGER   IT, IDROP(2), IX1, IX2, IY1, IY2, ICHL, ICHB, ICHR,
     *   ICHT, NXA, NYA, I, J, JERR, TTYLUN, TTYIND, IPLANE, I4XTRA
      LOGICAL   T, F, BLAST, BNEXT
      INCLUDE 'XBASL2.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTKS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA TTYLUN /5/
C-----------------------------------------------------------------------
C                                       inits, open TEK
      CALL ZTKOPN (IERR)
      IF (IERR.GT.1) GO TO 999
      DEVON = 1
      CALL TKCATL ('INIT', I, I, FVEC, IERR)
      IF (IERR.GT.1) GO TO 999
C                                       "Slice" corners
      IDROP(1) = 0
      IDROP(2) = 0
      DO 10 I = 1,7
         XBLC(I) = IPOS(I)
         XTRC(I) = XBLC(I)
 10      CONTINUE
      XTRC(1) = XBLC(1) + INPTS - 1
C                                       Set PIX ranges
      ORANGE(1) = PIXR(1)
      ORANGE(2) = PIXR(2)
C                                       Default: actual range
      IF (PIXR(2).LE.PIXR(1)) THEN
         ORANGE(1) = 1.0E10
         ORANGE(2) = -ORANGE(1)
         DO 15 I = 1,INPTS
            IF (DATA(I).NE.FBLANK) THEN
               ORANGE(1) = MIN (ORANGE(1), DATA(I))
               ORANGE(2) = MAX (ORANGE(2), DATA(I))
               END IF
 15         CONTINUE
         XFAC = ORANGE(2) - ORANGE(1)
         ORANGE(2) = ORANGE(2) + 0.25 * XFAC
         ORANGE(1) = ORANGE(1) - 0.25 * XFAC
         END IF
      CATR(IRRAN) = ORANGE(1)
      CATR(IRRAN+1) = ORANGE(2)
      XFAC = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      XOFF = 40000.0 - XFAC * CATR(IRRAN+1)
      PBLC(2) = ORANGE(1) * XFAC + XOFF
      PTRC(2) = ORANGE(2) * XFAC + XOFF
C                                       Label inits
      LOCNUM = 1
      FQFINC = 0.0
      FQFREQ = 0.0D0
      CALL SLBINI (IDROP, INPTS, ORANGE, PBLC, PTRC, XBLC, XTRC,
     *   FQFREQ, FQFINC, CATBLK(IIDEP), LABEL, YGAP, CH, TEXT, NTEXT)
      ORANGE(1) = XFAC*ORANGE(1) + XOFF
      ORANGE(2) = XFAC*ORANGE(2) + XOFF
      XYRATO = (PTRC(2) - PBLC(2)) / (PTRC(1) - PBLC(1))
      IX1 = PBLC(1) + .5
      IY1 = PBLC(2) + .5
      IX2 = PTRC(1) + .5
      IY2 = PTRC(2) + .5
      ICHL = CH(1) * CSIZTK(1) + .5
      ICHB = CH(2) * CSIZTK(2) + .5
      ICHR = CH(3) * CSIZTK(1) + .5
      ICHT = CH(4) * CSIZTK(2) + .5
      NYA = MAXXTK(2) - ICHT -ICHB -1
      NXA = MAXXTK(1) - ICHL - ICHR - 1
C                                       compute scaling
      X = IX2
      X = ABS (X - IX1)
      XX = X * XYRATO
      Y = IY2
      Y = ABS (Y - IY1)
      IF ((XX.LE.0.0) .OR. (Y.LE.0.0)) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (8)
         IERR = 1
         GO TO 999
         END IF
      IF ((XX/Y).LE.FLOAT(NXA)/FLOAT(NYA)) THEN
         SCALEY = NYA / Y
         SCALEX = SCALEY * Y / X
      ELSE
         SCALEX = NXA / X
         SCALEY = SCALEX * X / Y
         END IF
C
      NXA = SCALEX * X + ICHL + ICHR
      RX0 = ICHL + MAX (0, MAXXTK(1)-NXA) / 2 + 1
      NYA = SCALEY * Y + ICHB + ICHT
      RY0 = ICHB + MAX (0, MAXXTK(2)-NYA) / 2 + 1
C                                       Put stuff in image catalog.
      CATBLK(IIWIN  ) = IX1
      CATBLK(IIWIN+1) = IY1
      CATBLK(IIWIN+2) = IX2
      CATBLK(IIWIN+3) = IY2
      CATBLK(IICOR  ) = RX0 + 0.5
      CATBLK(IICOR+1) = RY0 + 0.5
      CATBLK(IICOR+2) = RX0 + X * SCALEX + 0.5
      CATBLK(IICOR+3) = RY0 + Y * SCALEY + 0.5
      CATBLK(IIPLT) = 5
      CATBLK(IIOTH) = LABEL
      CATBLK(IIOTH+1) = IDROP(1)
      CATBLK(IIOTH+2) = IDROP(2)
      I4XTRA = IIOTH + 3
      CATR(I4XTRA  ) = XBLC(1)
      CATR(I4XTRA+1) = XBLC(2)
      CATR(I4XTRA+2) = XTRC(1)
      CATR(I4XTRA+3) = XTRC(2)
      CALL TKCATL ('WRIT', IPLANE, IPLANE, CATBLK, IERR)
C                                       set scale for plot routines
      RX0 = RX0 - PBLC(1) * SCALEX + 0.5
      RY0 = RY0 - PBLC(2) * SCALEY + 0.5
C                                       label the plot
      CALL TKLAB (PBLC, PTRC, LABEL, YGAP, TEXT, NTEXT, CH, F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       add pixel coordinates
      DX = -8.5
      DY = -2.0
      CALL TEKVEC (PTRC(1), PTRC(2), 1, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGBUF,1060) IPOS(2)
      J = 7
      CALL TKCHAR (J, 0, DX, DY, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      DY = DY - 1.5
      CALL TEKVEC (PTRC(1), PTRC(2), 1, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGBUF,1061) IPOS(3)
      CALL TKCHAR (J, 0, DX, DY, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      DY = DY - 1.5
      CALL TEKVEC (PTRC(1), PTRC(2), 1, IERR)
      IF (IERR.NE.0) GO TO 999
      I = JJC - 1
      WRITE (MSGBUF,1062) I
      CALL TKCHAR (J, 0, DX, DY, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       plot data
      IF (DOSLIC.GT.0.) THEN
         BLAST = .TRUE.
         DO 70 I = 1,INPTS
            BNEXT = DATA(I).EQ.FBLANK
            IF (.NOT.BNEXT) THEN
               X = I - 0.5
               Y = DATA(I) * XFAC + XOFF
               Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
               J = 2
               IF (BLAST) J = 1
               CALL TEKVEC (X, Y, J, IERR)
               IF (IERR.NE.0) GO TO 999
               X = I + 0.5
               J = 2
               CALL TEKVEC (X, Y, J, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            BLAST = BNEXT
 70         CONTINUE
         END IF
      IF (DOSLIC.GT.1.0) THEN
         DO 90 I = 1,NBOXES
            IT = NBOX(1,I)
 75         IF (DATA(IT).EQ.FBLANK) THEN
               IT = IT + 1
               IF (IT.GT.NBOX(2,I)) GO TO 90
               GO TO 75
               END IF
            X1 = NBOX(1,I)
            Y = DATA(IT) * XFAC + XOFF
            Y1 = Y + 0.15 * (ORANGE(2) - ORANGE(1))
            Y2 = Y - 0.15 * (ORANGE(2) - ORANGE(1))
            Y1 = MIN (ORANGE(2), MAX (ORANGE(1), Y1))
            Y2 = MIN (ORANGE(2), MAX (ORANGE(1), Y2))
            IT = NBOX(2,I)
 80         IF (DATA(IT).EQ.FBLANK) THEN
               IT = IT - 1
               IF (IT.LT.NBOX(1,I)) GO TO 90
               GO TO 80
               END IF
            X2 = NBOX(2,I)
            Y = DATA(IT) * XFAC + XOFF
            Y3 = Y + 0.15 * (ORANGE(2) - ORANGE(1))
            Y4 = Y - 0.15 * (ORANGE(2) - ORANGE(1))
            Y3 = MIN (ORANGE(2), MAX (ORANGE(1), Y3))
            Y4 = MIN (ORANGE(2), MAX (ORANGE(1), Y4))
            CALL TEKVEC (X1, Y1, 1, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TEKVEC (X1, Y2, 2, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TEKVEC (X2, Y4, 2, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TEKVEC (X2, Y3, 2, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TEKVEC (X1, Y1, 2, IERR)
            IF (IERR.NE.0) GO TO 999
 90         CONTINUE
         END IF
C                                       alpha mode
      I = 31
      CALL ZTKBUF (I, 1, IERR)
      CALL TEKFLS (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Talk to user
      IF (DOSLIC.GT.1.0) THEN
         CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, F, T, T, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1900) JERR
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       change order of polynomial
 910     I = PORDER + 0.01
         IF (I.GT.0) THEN
            WRITE (MSGBUF,1910) I
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, JERR)
            IF (JERR.NE.0) GO TO 920
            CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, JERR)
            IF (JERR.NE.0) GO TO 920
            READ (MSGBUF,1911) TEMP, J
            IF ((TEMP.EQ.'C') .OR. (TEMP.EQ.'c')) THEN
               IF ((J.LT.0) .OR. (J.GT.I)) GO TO 910
               NORDER = J
               IF (NORDER.LT.1) PARMS(2) = 0.0D0
               END IF
            END IF
C                                       Quit etc
         WRITE (MSGBUF,1915)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, JERR)
         IF (JERR.NE.0) GO TO 920
         CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, JERR)
         IF (JERR.NE.0) GO TO 920
         READ (MSGBUF,1916) TEMP
         IF ((TEMP.EQ.'B') .OR. (TEMP.EQ.'b')) THEN
            IERR = 101
         ELSE IF ((TEMP.EQ.'Q') .OR. (TEMP.EQ.'q')) THEN
            IERR = 102
         ELSE IF ((TEMP.EQ.'E') .OR. (TEMP.EQ.'e')) THEN
            IERR = 103
            END IF
         GO TO 930
C                                       TTY error
 920     WRITE (MSGTXT,1920) JERR
         CALL MSGWRT (6)
 930     CALL ZCLOSE (TTYLUN, TTYIND, JERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('SCALING ERROR.')
 1060 FORMAT ('Y=',I5)
 1061 FORMAT ('Z=',I5)
 1062 FORMAT ('Order=',I1)
 1900 FORMAT ('OPEN TERMINAL ERROR',I7)
 1910 FORMAT ('??? Type ''C0'' TO ''C',I1,''' to change order of',
     *   ' polynomial')
 1911 FORMAT (A1,I1)
 1915 FORMAT ('??? Type ''E'' to enter windows, ''B'' to skip row,',
     *   ' hit return to procede')
 1916 FORMAT (A1)
 1920 FORMAT ('TERMINAL I/O ERROR',I7)
      END
      SUBROUTINE BTKGUS (INPTS, ORANGE, NBOXES, NBOX, FVEC, IERR)
C-----------------------------------------------------------------------
C   BTKGUS has the user point at the desired baseline windows setting
C   COMMON parameters NBOXES and NBOX.
C   Inputs:
C      INPTS    I         Number data points in row
C      ORANGE   R(2)      Plot range in plot units
C   Output:
C      NBOXES   I         Number windows
C      NBOX     I(2,20)   Window parameters
C      FVEC     R(*)      Work buffer
C      IERR     I         error code: 0 -> ok
C-----------------------------------------------------------------------
      INTEGER   INPTS, IERR, NBOXES, NBOX(2,20)
      REAL      FVEC(*), ORANGE(2)
C
      CHARACTER MSGBUF*80
      REAL      XFAC, XOFF, DY, Y, X1, X2
      INTEGER   I, J, TTYLUN, TTYIND, IXT, IYT, IX, IY, IX0, IY0, JERR
      LOGICAL   T, F
      INCLUDE 'XBASL2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTKS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA TTYLUN /5/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Set scales
      XFAC = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      XOFF = 40000.0 - XFAC * CATR(IRRAN+1)
      IXT = RX0 + SCALEX * INPTS + 0.5
      IYT = RY0 + SCALEY * ORANGE(2) + 0.5
      IX0 = RX0 + SCALEX + 0.5
      IY0 = RY0 + SCALEY * ORANGE(1) + 0.5
C                                       Open terminal
      CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Loop over components.
 20   DO 40 I = 1,20
C                                       read left edge window
         WRITE (MSGBUF,1020) I
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL TKCURS (FVEC, IX, IY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       suppress double hitting
         CALL ZTKCLS (IERR)
         CALL ZTKOPN (IERR)
C                                       Set it
         IF ((IY.LT.IY0) .OR. (IX.GT.IXT) .OR. (IY.GT.IYT)) GO TO 50
         IX = MAX (IX, IX0)
         NBOX(1,I) = (IX - RX0) / SCALEX + 0.5
C                                       read right edge
         WRITE (MSGBUF,1021) I
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL TKCURS (FVEC, IX, IY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       suppress double hitting
         CALL ZTKCLS (IERR)
         CALL ZTKOPN (IERR)
C                                       Set it
         IF ((IX.LT.IX0) .OR. (IY.LT.IY0) .OR. (IY.GT.IYT)) GO TO 50
         IX = MIN (IX, IXT)
         NBOX(2,I) = (IX - RX0) / SCALEX + 0.5
         IF (NBOX(2,I).LT.NBOX(1,I)) THEN
            IX = NBOX(2,I)
            NBOX(2,I) = NBOX(1,I)
            NBOX(1,I) = IX
            END IF
         NBOX(1,I) = MAX (1, NBOX(1,I))
         NBOX(2,I) = MIN (INPTS, NBOX(2,I))
         NBOXES = I
 40      CONTINUE
 50   IF (NBOXES.LE.0) GO TO 20
C                                       Plot new windows
      DY = (ORANGE(2) - ORANGE(1)) / 20.0
      DO 60 I = 1,NBOXES
         J = (NBOX(1,I) + NBOX(2,I)) / 2
         X1 = NBOX(1,I)
         X2 = NBOX(2,I)
         Y = DATA(J) * XFAC + XOFF
         Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
         CALL TEKVEC (X1, Y+DY, 1, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TEKVEC (X1, Y-DY, 2, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TEKVEC (X2, Y-DY, 2, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TEKVEC (X2, Y+DY, 2, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TEKVEC (X1, Y+DY, 2, IERR)
         IF (IERR.NE.0) GO TO 990
 60      CONTINUE
C                                       alpha mode
      I = 31
      CALL ZTKBUF (I, 1, IERR)
      CALL TEKFLS (IERR)
      GO TO 990
C                                       terminal error
 900  WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (6)
C                                       Close TTY anyway
 990  CALL ZCLOSE (TTYLUN, TTYIND, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' OPENING TERMINAL')
 1020 FORMAT ('Position cursor at left edge of window ',I2)
 1021 FORMAT ('Position cursor at right edge of window ',I2)
 1900 FORMAT ('ERROR',I5,' WRITING TO TERMINAL')
      END
      SUBROUTINE BTKMOD (DOMODL, DORESI, INPTS, NG, ORANGE, FVEC, PERR,
     *   IERR)
C-----------------------------------------------------------------------
C   BTKMOD plots the residual and model functions on the TEK.  It asks
C   the user for permission to proceed.
C   Inputs:
C       DOMODL   R      > 0. => plot model
C       DORESI   R      > 0. => plot residuals
C       INPTS    I      Number of data points
C       NG       I      Order of baseline (max)
C       ORANGE   R(2)   Plot intensity range (plot units)
C       FVEC     R(*)   data - model
C       PERR     I      > 0 => probable parameter bad
C  Output:
C       IERR     I      TEK error code
C                          101 => blank this solution
C                          102 => User wants to quit
C                          103 => do a retry
C-----------------------------------------------------------------------
      INTEGER   INPTS, NG, PERR, IERR
      REAL      FVEC(*), DOMODL, DORESI, ORANGE(2)
C
      CHARACTER TEMP*1, MSGBUF*80
      INTEGER   I, J, TTYLUN, TTYIND, NPPL, K, JERR
      REAL      XFAC, XOFF, X, Y, XP, YP, TP, DX, DY
      LOGICAL   BLAST, T, F
      INCLUDE 'XBASL2.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTKS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA TTYLUN /5/
C-----------------------------------------------------------------------
      IERR = 0
      IF ((DOMODL.LE.0.) .AND. (DORESI.LE.0.)) GO TO 900
      XFAC = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      XOFF = 40000.0 - XFAC * CATR(IRRAN+1)
C                                       Plot residuals
      K = 0
      IF (DORESI.GT.0.) THEN
C                                       Do any show?
         DO 10 I = 1,INPTS
            IF (DATA(I).EQ.FBLANK) GO TO 10
               Y = FVEC(I) * XFAC + XOFF
               IF ((Y.LT.ORANGE(2)) .AND. (Y.GT.ORANGE(1))) K = K + 1
 10         CONTINUE
         END IF
      IF (K.GT.1) THEN
         BLAST = .TRUE.
         DO 30 I = 1,INPTS
            IF (DATA(I).EQ.FBLANK) THEN
               BLAST = .TRUE.
            ELSE
               X = I
               Y = FVEC(I) * XFAC + XOFF
               XP = X * SCALEX + RX0 - RXL
               YP = Y * SCALEY + RY0 - RYL
               TP = MAX (ABS(XP), ABS(YP))
               NPPL = (TP + 9) / 16.0 + 0.5
               IF (BLAST) NPPL = 1
               XP = (RXL - RX0) / SCALEX
               YP = (RYL - RY0) / SCALEY
               DX = (X - XP) / NPPL
               DY = (Y - YP) / NPPL
               DO 25 K = 1,NPPL
                  X = XP + (K-0.6) * DX
                  Y = YP + (K-0.6) * DY
                  Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
                  J = 2
                  IF ((BLAST) .OR. (MOD(K,2).EQ.0)) J = 1
                  CALL TEKVEC (X, Y, J, IERR)
                  IF (IERR.NE.0) GO TO 100
                  X = XP + K * DX
                  Y = YP + K * DY
                  Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
                  J = 1
                  CALL TEKVEC (X, Y, J, IERR)
                  IF (IERR.NE.0) GO TO 100
 25               CONTINUE
               BLAST = .FALSE.
               END IF
 30         CONTINUE
         END IF
C                                       Plot model
      IF (DOMODL.GT.0.) THEN
         BLAST = .TRUE.
         DO 60 I = 1,INPTS
            IF (DATA(I).EQ.FBLANK) THEN
               BLAST = .TRUE.
            ELSE
               X = I
               Y = (DATA(I) - FVEC(I)) * XFAC + XOFF
               Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
               J = 2
               IF (BLAST) J = 1
               CALL TEKVEC (X, Y, J, IERR)
               IF (IERR.NE.0) GO TO 100
               BLAST = .FALSE.
               END IF
 60         CONTINUE
         END IF
C                                       Alpha mode
 100  I = 31
      CALL ZTKBUF (I, 1, IERR)
      CALL TEKFLS (IERR)
      BLAST = PERR.GT.0
C                                       talk to user
 900  CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1900) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
      IF (BLAST) THEN
         WRITE (MSGBUF,1910)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      IF (NG.GT.1) THEN
         WRITE (MSGBUF,1915)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      WRITE (MSGBUF,1920)
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      READ (MSGBUF,1921) TEMP
      CALL CHLTOU (4, TEMP)
      IF ((TEMP.EQ.'B') .OR. (TEMP.EQ.'b')) THEN
         IERR = 101
      ELSE IF ((TEMP.EQ.'Q') .OR. (TEMP.EQ.'q')) THEN
         IERR = 102
      ELSE IF ((TEMP.EQ.'R') .OR. (TEMP.EQ.'r')) THEN
         IF (NG.GT.1) IERR = 103
         END IF
      GO TO 990
C                                       TTY error
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (6)
 990  CALL ZCLOSE (TTYLUN, TTYIND, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('OPEN TERMINAL ERROR',I7)
 1910 FORMAT ('>>>> PARAMETERS SEEM OUT OF RANGE.  SOLUTION PROBABLY ',
     *   'BAD! <<<<')
 1915 FORMAT ('**** Enter RETR to repeat the row (must enter a guess)')
 1920 FORMAT ('**** Enter BAD to flag solution, QUIT to die, ',
     *   'Hit return to continue')
 1921 FORMAT (A1)
 1980 FORMAT ('TERMINAL I/O ERROR',I7)
      END
      SUBROUTINE XBASOU (BLNKD, IRET)
C-----------------------------------------------------------------------
C   XBASOU, if any, creates and fills (via PSCALE) the individual
C   baseline parameters.  It calls XBASHI for history info for all.
C   Inputs:
C      BLNKD   L   Are any parameters blanked?
C   Output:
C      IRET    I   0 => ok,  4 => real trouble.
C-----------------------------------------------------------------------
      INTEGER   IRET
      LOGICAL   BLNKD
C
      CHARACTER SEQTYP*6
      INTEGER   NG, NXO, NYO, WINI(4), WINO(4), IERR, IP, NCN,
     *   IG, IOFF
      INCLUDE 'XBASL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      NCN = 2
C                                       Residual map history
      CALL XBASHI (IRET, NCN)
      IF (DOCAT.LE.0.0) GO TO 999
C                                       loop limits etc.
      NG = PORDER + 2.01
      WINI(1) = 1
      WINI(2) = 1
      WINI(3) = 2 * NG
      WINO(1) = 1
      WINO(2) = 1
C                                       Output all parms
      DO 30 IG = 1,NG
         DO 20 IP = 1,2
            IOFF = IG
            IF (IP.EQ.2) IOFF = NG + IP
            NCN = NCN + 1
            NEWCNO = FCNO(NCN)
            DISKO = FVOL(NCN)
            CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'REST', IBUFF1,
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
            CALL XBASHI (IOFF, NCN)
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
C   and writes an image out.
C   Inputs:
C      IOFF     I       Pixel in row to extract (1-rel)
C      ISCR     I       Scratch file number in CFIL common
C      WINI     I(4)    Input window
C      NEWCNO   I       Output catalog number
C      DISKO    I       Output disk number
C      WINO     I(4)    Output Window
C      JBUFSZ   I       Buffer size in bytes
C      PMAX     R(22)   Max values by columns
C      PMIN     R(22)   Min values by columns
C      BLNKD    L       Image is blanked
C   Output:
C      IERR     I       0 -> ok, else IO error
C      CATBLK in common: change max/min and scaling and blanking
C      Buffers in common
C-----------------------------------------------------------------------
      INTEGER   IOFF, ISCR, WINI(4), NEWCNO, DISKO, WINO(4), JBUFSZ,
     *   IERR
      REAL      PMAX(22), PMIN(22)
      LOGICAL   BLNKD
C
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER PHNAME*48
      LOGICAL   T
      INTEGER   NXO, L3, L4, L5, L6, L7, I2, I3, I4, I5, I6, I7, J,
     *   LUNI, LUNO, INDI, INDO, IPOS(8), NAXT(8), INDIM, BOTEMP, OBIND,
     *   IBIND, L, JERR
      REAL      BUFF1(MABFSS), BUFF2(MABFSS)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /BUFRS/ BUFF1, BUFF2
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
                  CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(4),
     *               BOTEMP, IERR)
                  BOTEMP = BOTEMP + 1
                  CALL MINIT ('WRIT', LUNO, INDO, WINO(3), WINO(4),
     *               WINO, BUFF2, JBUFSZ, BOTEMP, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1100) IERR
                     GO TO 970
                     END IF
                  DO 300 I3 = 1,L3
                     IPOS(3) = I3
                     CALL COMOFF (INDIM, NAXT, IPOS(3), BOTEMP, IERR)
                     BOTEMP = BOTEMP + 1
                     CALL MINIT ('READ', LUNI, INDI, WINI(3), WINI(4),
     *                  WINI, BUFF1, JBUFSZ, BOTEMP, IERR)
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
                        CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND,
     *                     IERR)
                        IF (IERR.NE.0) THEN
                           WRITE (MSGTXT,1120) 'READ', IERR
                           GO TO 970
                           END IF
                        J = IBIND + IOFF - 1
                        L = OBIND + I2 - 1
                        BUFF2(L) = BUFF1(J)
 200                    CONTINUE
 300                 CONTINUE
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
      SUBROUTINE XBASHI (ITYP, NCN)
C-----------------------------------------------------------------------
C   XBASHI copies and updates history file.
C   Inputs:
C      ITYP   I   Output map type: 0 => residual
C                 1 => answers (get 1st axis info also)
C      NCN    I   Position in FILES common on catlgd file
C-----------------------------------------------------------------------
      INTEGER   ITYP, NCN
C
      CHARACTER HILINE*72, LABEL*8, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR, I, IG
      LOGICAL   T
      INCLUDE 'XBASL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'  '/
C-----------------------------------------------------------------------
C                                       Copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 50
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, IBUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 50
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       BLC
      WRITE (HILINE,2000) TSKNAM, BLC
      CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       TRC
      WRITE (HILINE,2001) TSKNAM, TRC
      CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       other Parms
      WRITE (HILINE,2002) TSKNAM, YINC, ZINC
      IF ((YINC.GE.2.) .OR. (ZINC.GE.2.)) THEN
         CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         END IF
      CALL H2CHR (8, 1, OLDH(KHBUN), LABEL)
      WRITE (HILINE,2003) TSKNAM, FCUT, LABEL
      CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Baseline limits
      IG = PORDER + 0.01
      WRITE (HILINE,2005) TSKNAM, IG
      CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       list windows in batch-like
      IF (ICODE.NE.1) THEN
         DO 15 I = 1,NBOXES
            WRITE (HILINE,2006) TSKNAM, NBOX(1,I), NBOX(2,I), I
            CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 50
 15         CONTINUE
         END IF
C                                       Old axis 1
      IF (ITYP.GT.0) THEN
         CALL H2CHR (8, 1, OLDH(KHCTP), LABEL)
         WRITE (HILINE,2020) TSKNAM, LABEL
         CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2021) TSKNAM, CATOLD(KINAX)
         CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2022) TSKNAM, OLDR(KRCRP)
         CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2023) TSKNAM, OLDR(KRCIC)
         CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2024) TSKNAM, OLDD(KDCRV)
         CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
         END IF
C                                       Close HI file
 50   CALL HICLOS (LUN2, T, IBUFF2, IERR)
C                                        Update CATBLK and close
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'CLWR', IBUFF1, IERR)
      FRW(NCN) = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XBASHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,'TRC =',6(F6.0,','),F6.0)
 2002 FORMAT (A6,'YINC =',F6.0,'  ZINC =',F6.0)
 2003 FORMAT (A6,'FLUX =',1PE12.4,14X,'/ Flux cutoff in ',A8)
 2005 FORMAT (A6,'PORDER =',I2,22X,'/ Max polynomial fit')
 2006 FORMAT (A6,'BOXES =',2I6,12X,'/ Window',I2,' wrt BLC(1)')
 2020 FORMAT (A6,'CTYPE1  = ''',A8,'''',12X,'/ old axis 1')
 2021 FORMAT (A6,'NAXIS1  = ',I6,16X,'/ Old axis 1')
 2022 FORMAT (A6,'CRPIX1  = ',F9.3,13X,'/ Old axis 1')
 2023 FORMAT (A6,'CDELT1  = ',1PE13.5,9X,'/ Old axis 1')
 2024 FORMAT (A6,'CRVAL1  = ',1PE18.10,4X,'/ Old axis 1')
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
C      ZINC   R      Pxel increment on third axis
C   Common /MAPHDR/ CATBLK     map header (in/out)
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7), XINC, YINC, ZINC
C
      CHARACTER FCHARS(3)*4, CHTM12*12
      REAL      AINC(7)
      INTEGER   IPL, IPH, NAX, I, J
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
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
               IF (FCHARS(J).EQ.CHTM12(:4)) THEN
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
C      IERR    I   0 ok, 1 no good data, 2 other singularity
C-----------------------------------------------------------------------
      INTEGER   NDATA, IERR
C
      REAL      PP, AL, SUM, TEMP
      INTEGER   MMAX, I, J, K, JJ, KK, N, MM
      INCLUDE 'XBASL2.INC'
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
            IF (J.GT.1) THEN
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
C      IX   I   X position
C-----------------------------------------------------------------------
      INTEGER   IX
C
      REAL      AX
      INTEGER   J, K, KK
      INCLUDE 'XBASL2.INC'
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
      SUBROUTINE BTVINI (IPOS, INPTS, NORDER, LABEL, PIXR, DOSLIC,
     *   PARMS, NBOXES, NBOX, PORDER, ORANGE, IERR)
C-----------------------------------------------------------------------
C   BTVINI initializes the TEK for a XBASL plot, plots axis labels,
C   and, if requested, plots the data.
C   Inputs:
C      IPOS     I(7)      Position in cube first point in row.
C      INPTS    I         Number of points in row.
C      LABEL    I         Requested label type
C      PIXR     R(2)      Requested plot value range
C      DOSLIC   R         > 0. => plot data
C      PARMS    R(10)     Initial guess
C      NBOXES   I         # windows
C      NBOX     I(2,20)   windows
C      PORDER   R         Max polynomial order
C   Output:
C      PORDER   I         Order of polynomial to use now
C      ORANGE   R(2)      Actual plot range in plot units
C      IERR     I         > 0 => plot failed
C                         101 => bad initial guess
C                         102 => DIE
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), INPTS, NORDER, LABEL, NBOXES, NBOX(2,20), IERR
      REAL      PIXR(2), DOSLIC, PARMS(9), PORDER, ORANGE(2)
C
      CHARACTER TEMP*1, TEXT(2)*80, MSGBUF*132, NTEXT*80
      REAL      XBLC(7), XTRC(7), PBLC(2), PTRC(2), YGAP, CH(4), XYRATO,
     *   X, XX, Y, XFAC, XOFF, DX, DY, Y1, Y2, X1,  X2, Y3, Y4, FQFINC
      INTEGER   IT, IDROP(2), IX1, IX2, IY1, IY2, ICHL, ICHB, ICHR,
     *   ICHT, NXA, NYA, I, J, JERR, TTYLUN, TTYIND, I4XTRA, IDX, IDY,
     *   TVWIND(4), TVSIZE(2)
      LOGICAL   T, F, BLAST, BNEXT, FIRST
      DOUBLE PRECISION FQFREQ
      INCLUDE 'XBASL2.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVS.INC'
      INCLUDE 'INCS:DCAT.INC'
      SAVE FIRST
      DATA T, F, FIRST /.TRUE.,.FALSE.,.TRUE./
      DATA TTYLUN /5/
C-----------------------------------------------------------------------
C                                       inits, open TEK
      CALL TVOPEN (INBUF, IERR)
      IF (IERR.GT.1) GO TO 999
      DEVON = 2
      CALL YHOLD ('ONNN', IERR)
      IF (FIRST) THEN
         IGR1 = UCHAN + 0.01
         IF ((IGR1.LE.0) .OR. (IGR1.GT.7)) THEN
            IGR1 = 1
            IGR2 = 2
            IGR3 = 3
            IGR4 = 4
         ELSE
            IGR2 = IGR1
            IGR3 = IGR1
            IGR4 = IGR1
            END IF
         IGR1 = IGR1 + NGRAY
         IGR2 = IGR2 + NGRAY
         IGR3 = IGR3 + NGRAY
         IGR4 = IGR4 + NGRAY
         DO 5 I = 1,NGRAY+NGRAPH
            CALL YSLECT ('OFFF', I, 0, INBUF, IERR)
            IF (IERR.NE.0) GO TO 999
 5          CONTINUE
         CALL YSLECT ('ONNN', IGR1, 0, INBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (IGR1.NE.IGR2) THEN
            CALL YSLECT ('ONNN', IGR2, 0, INBUF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL YSLECT ('ONNN', IGR3, 0, INBUF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL YSLECT ('ONNN', IGR4, 0, INBUF, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         FIRST = .FALSE.
         END IF
C                                       "Slice" corners
      IDROP(1) = 0
      IDROP(2) = 0
      DO 10 I = 1,7
         XBLC(I) = IPOS(I)
         XTRC(I) = XBLC(I)
 10      CONTINUE
      XTRC(1) = XBLC(1) + INPTS - 1
C                                       Set PIX ranges
      ORANGE(1) = PIXR(1)
      ORANGE(2) = PIXR(2)
C                                       Default: actual range
      IF (PIXR(2).LE.PIXR(1)) THEN
         ORANGE(1) = 1.0E10
         ORANGE(2) = -ORANGE(1)
         DO 15 I = 1,INPTS
            IF (DATA(I).NE.FBLANK) THEN
               ORANGE(1) = MIN (ORANGE(1), DATA(I))
               ORANGE(2) = MAX (ORANGE(2), DATA(I))
               END IF
 15         CONTINUE
         XFAC = ORANGE(2) - ORANGE(1)
         ORANGE(2) = ORANGE(2) + 0.25 * XFAC
         ORANGE(1) = ORANGE(1) - 0.25 * XFAC
         END IF
      CATR(IRRAN) = ORANGE(1)
      CATR(IRRAN+1) = ORANGE(2)
      XFAC = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      XOFF = 40000.0 - XFAC * CATR(IRRAN+1)
      PBLC(2) = ORANGE(1) * XFAC + XOFF
      PTRC(2) = ORANGE(2) * XFAC + XOFF
C                                       Label inits
      LOCNUM = 1
      FQFINC = 0.0
      FQFREQ = 0.0D0
      CALL SLBINI (IDROP, INPTS, ORANGE, PBLC, PTRC, XBLC, XTRC,
     *   FQFREQ, FQFINC, CATBLK(IIDEP), LABEL, YGAP, CH, TEXT, NTEXT)
      ORANGE(1) = XFAC*ORANGE(1) + XOFF
      ORANGE(2) = XFAC*ORANGE(2) + XOFF
      XYRATO = (PTRC(2) - PBLC(2)) / (PTRC(1) - PBLC(1))
      IX1 = PBLC(1) + .5
      IY1 = PBLC(2) + .5
      IX2 = PTRC(1) + .5
      IY2 = PTRC(2) + .5
      ICHL = CH(1) * CSIZTV(1) + .5
      ICHB = CH(2) * CSIZTV(2) + .5
      ICHR = CH(3) * CSIZTV(1) + .5
      ICHT = CH(4) * CSIZTV(2) + .5
      CALL YWINDO ('READ', TVWIND, IERR)
      IF (IERR.NE.0) THEN
         TVWIND(1) = 1
         TVWIND(2) = 1
         TVWIND(3) = MAXXTV(1)
         TVWIND(4) = MAXXTV(2)
         END IF
      TVSIZE(1) = TVWIND(3) - TVWIND(1) + 1
      TVSIZE(2) = TVWIND(4) - TVWIND(2) + 1
      NYA = TVSIZE(2) - ICHT -ICHB -1
      NXA = TVSIZE(1) - ICHL - ICHR - 1
C                                       compute scaling
      X = IX2
      X = ABS (X - IX1)
      XX = X * XYRATO
      Y = IY2
      Y = ABS (Y - IY1)
      IF ((XX.LE.0.0) .OR. (Y.LE.0.0)) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (8)
         IERR = 1
         GO TO 999
         END IF
      IF ((XX/Y).LE.FLOAT(NXA)/FLOAT(NYA)) THEN
         SCALEY = NYA / Y
         SCALEX = SCALEY * Y / X
      ELSE
         SCALEX = NXA / X
         SCALEY = SCALEX * X / Y
         END IF
C
      NXA = SCALEX * X + ICHL + ICHR
      RX0 = ICHL + MAX (0, TVSIZE(1)-NXA) / 2 + TVWIND(1)
      NYA = SCALEY * Y + ICHB + ICHT
      RY0 = ICHB + MAX (0, TVSIZE(2)-NYA) / 2 + TVWIND(2)
C                                       Put stuff in image catalog.
      CATBLK(IIWIN  ) = IX1
      CATBLK(IIWIN+1) = IY1
      CATBLK(IIWIN+2) = IX2
      CATBLK(IIWIN+3) = IY2
      CATBLK(IICOR  ) = RX0 + 0.5
      CATBLK(IICOR+1) = RY0 + 0.5
      CATBLK(IICOR+2) = RX0 + X * SCALEX + 0.5
      CATBLK(IICOR+3) = RY0 + Y * SCALEY + 0.5
      CATBLK(IIPLT) = 5
      CATBLK(IIOTH) = LABEL
      CATBLK(IIOTH+1) = IDROP(1)
      CATBLK(IIOTH+2) = IDROP(2)
      I4XTRA = IIOTH + 3
      CATR(I4XTRA  ) = XBLC(1)
      CATR(I4XTRA+1) = XBLC(2)
      CATR(I4XTRA+2) = XTRC(1)
      CATR(I4XTRA+3) = XTRC(2)
C                                       Update image catalog
      CALL YCINIT (IGR1, INBUF)
      CALL YCWRIT (IGR1, CATBLK(IICOR), CATBLK, INBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040)
         CALL MSGWRT (6)
         END IF
C                                       clear screen
      CALL YZERO (IGR1, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (IGR2.NE.IGR1) THEN
         CALL YCINIT (IGR2, INBUF)
         CALL YCINIT (IGR3, INBUF)
         CALL YCINIT (IGR4, INBUF)
         CALL YZERO (IGR2, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL YZERO (IGR3, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL YZERO (IGR4, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       set scale for plot routines
      RX0 = RX0 - PBLC(1) * SCALEX + 0.5
      RY0 = RY0 - PBLC(2) * SCALEY + 0.5
C                                       label the plot
      IGR = IGR1
      CALL TVLAB (PBLC, PTRC, LABEL, YGAP, TEXT, NTEXT, CH, F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       add pixel coordinates
      DX = PTRC(1)*SCALEX + RX0 - 8.5 * CSIZTV(1)
      DY = PTRC(2)*SCALEY + RY0 - 2.0 * CSIZTV(2)
      IDX = DX + 0.5
      IDY = DY + 0.5
      WRITE (MSGBUF,1060) IPOS(2)
      CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:7), INBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      DY = DY - 1.5 * CSIZTV(2)
      IDY = DY + 0.5
      WRITE (MSGBUF,1061) IPOS(3)
      CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:7), INBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      DY = DY - 1.5 * CSIZTV(2)
      IDY = DY + 0.5
      I = JJC - 1
      WRITE (MSGBUF,1062) I
      CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:7), INBUF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       plot data
      IF (DOSLIC.GT.0.) THEN
         BLAST = .TRUE.
         DO 70 I = 1,INPTS
            BNEXT = DATA(I).EQ.FBLANK
            IF (.NOT.BNEXT) THEN
               X = I - 0.5
               Y = DATA(I) * XFAC + XOFF
               Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
               J = 2
               IF (BLAST) J = 1
               CALL TVVEC (X, Y, J, IERR)
               IF (IERR.NE.0) GO TO 999
               X = I + 0.5
               J = 2
               CALL TVVEC (X, Y, J, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            BLAST = BNEXT
 70         CONTINUE
         END IF
      IF (DOSLIC.GT.1.0) THEN
         IGR = IGR2
         DO 90 I = 1,NBOXES
            IT = NBOX(1,I)
 75         IF (DATA(IT).EQ.FBLANK) THEN
               IT = IT + 1
               IF (IT.GT.NBOX(2,I)) GO TO 90
               GO TO 75
               END IF
            X1 = NBOX(1,I)
            Y = DATA(IT) * XFAC + XOFF
            Y1 = Y + 0.11 * (ORANGE(2) - ORANGE(1))
            Y2 = Y - 0.11 * (ORANGE(2) - ORANGE(1))
            Y1 = MIN (ORANGE(2), MAX (ORANGE(1), Y1))
            Y2 = MIN (ORANGE(2), MAX (ORANGE(1), Y2))
            IT = NBOX(2,I)
 80         IF (DATA(IT).EQ.FBLANK) THEN
               IT = IT - 1
               IF (IT.LT.NBOX(1,I)) GO TO 90
               GO TO 80
               END IF
            X2 = NBOX(2,I)
            Y = DATA(IT) * XFAC + XOFF
            Y3 = Y + 0.11 * (ORANGE(2) - ORANGE(1))
            Y4 = Y - 0.11 * (ORANGE(2) - ORANGE(1))
            Y3 = MIN (ORANGE(2), MAX (ORANGE(1), Y3))
            Y4 = MIN (ORANGE(2), MAX (ORANGE(1), Y4))
            CALL TVVEC (X1, Y1, 1, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TVVEC (X1, Y2, 2, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TVVEC (X2, Y4, 2, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TVVEC (X2, Y3, 2, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TVVEC (X1, Y1, 2, IERR)
            IF (IERR.NE.0) GO TO 999
 90         CONTINUE
         END IF
      CALL YHOLD ('OFFF', JERR)
C                                       Talk to user
      IF (DOSLIC.GT.1.0) THEN
         CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, F, T, T, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1900) JERR
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       change order of polynomial
 910     I = PORDER + 0.01
         IF (I.GT.0) THEN
            WRITE (MSGBUF,1910) I
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, JERR)
            IF (JERR.NE.0) GO TO 920
            CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, JERR)
            IF (JERR.NE.0) GO TO 920
            READ (MSGBUF,1911) TEMP, J
            IF ((TEMP.EQ.'C') .OR. (TEMP.EQ.'c')) THEN
               IF ((J.LT.0) .OR. (J.GT.I)) GO TO 910
               NORDER = J
               IF (NORDER.LT.1) PARMS(2) = 0.0D0
               END IF
            END IF
C                                       Quit etc
         WRITE (MSGBUF,1915)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, JERR)
         IF (JERR.NE.0) GO TO 920
         CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, JERR)
         IF (JERR.NE.0) GO TO 920
         READ (MSGBUF,1916) TEMP
         IF ((TEMP.EQ.'B') .OR. (TEMP.EQ.'b')) THEN
            IERR = 101
         ELSE IF ((TEMP.EQ.'Q') .OR. (TEMP.EQ.'q')) THEN
            IERR = 102
         ELSE IF ((TEMP.EQ.'E') .OR. (TEMP.EQ.'e')) THEN
            IERR = 103
            END IF
         GO TO 930
C                                       TTY error
 920     WRITE (MSGTXT,1920) JERR
         CALL MSGWRT (6)
 930     CALL ZCLOSE (TTYLUN, TTYIND, JERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('SCALING ERROR.')
 1040 FORMAT ('TV IMAGE CATALOG ERROR.')
 1060 FORMAT ('Y=',I5)
 1061 FORMAT ('Z=',I5)
 1062 FORMAT ('Order=',I1)
 1900 FORMAT ('OPEN TERMINAL ERROR',I7)
 1910 FORMAT ('??? Type ''C0'' TO ''C',I1,''' to change order of',
     *   ' polynomial')
 1911 FORMAT (A1,I1)
 1915 FORMAT ('??? Type ''E'' to enter windows, ''B'' to skip row,',
     *   ' hit return to procede')
 1916 FORMAT (A1)
 1920 FORMAT ('TERMINAL I/O ERROR',I7)
      END
      SUBROUTINE BTVGUS (INPTS, ORANGE, NBOXES, NBOX, IERR)
C-----------------------------------------------------------------------
C   BTVGUS has the user point at the desired baseline windows setting
C   COMMON parameters NBOXES and NBOX.
C   Inputs:
C      INPTS    I         Number data points in row
C      ORANGE   R(2)      Plot range in plot units
C   Output:
C      NBOXES   I         Number windows
C      NBOX     I(2,20)   Window parameters
C      FVEC     R(*)      Work buffer
C      IERR     I         error code: 0 -> ok
C-----------------------------------------------------------------------
      INTEGER   INPTS, IERR, NBOXES, NBOX(2,20)
      REAL      ORANGE(2)
C
      CHARACTER MSGBUF*80
      REAL      XFAC, XOFF, DY, X1, X2, RPOS(2), Y1, Y2, Y3, Y4, Y
      INTEGER   I, TTYLUN, TTYIND, IXT, IYT, IX, IY, IX0, IY0, JERR,
     *   QUAD, IBUT, IT
      LOGICAL   T, F
      INCLUDE 'XBASL2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA TTYLUN /5/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Set scales
      XFAC = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      XOFF = 40000.0 - XFAC * CATR(IRRAN+1)
      IXT = RX0 + SCALEX * INPTS + 0.5
      IYT = RY0 + SCALEY * ORANGE(2) + 0.5
      IX0 = RX0 + SCALEX + 0.5
      IY0 = RY0 + SCALEY * ORANGE(1) + 0.5
C                                       Open terminal
      CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Loop over components.
 20   DO 40 I = 1,20
C                                       read left edge window
         WRITE (MSGBUF,1020) I
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL TVWHER (QUAD, RPOS, IBUT, IERR)
         IF (IERR.NE.0) GO TO 990
         IX = RPOS(1) + 0.5
         IY = RPOS(2) + 0.5
C                                       Set it
         IF ((IY.LT.IY0) .OR. (IX.GT.IXT) .OR. (IY.GT.IYT)) GO TO 50
         IF (IBUT.GE.8) GO TO 50
         IX = MAX (IX, IX0)
         NBOX(1,I) = (IX - RX0) / SCALEX + 0.5
C                                       read right edge
         WRITE (MSGBUF,1021) I
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL TVWHER (QUAD, RPOS, IBUT, IERR)
         IF (IERR.NE.0) GO TO 990
         IX = RPOS(1) + 0.5
         IY = RPOS(2) + 0.5
C                                       Set it
         IF ((IX.LT.IX0) .OR. (IY.LT.IY0) .OR. (IY.GT.IYT)) GO TO 50
         IF (IBUT.GE.8) GO TO 50
         IX = MIN (IX, IXT)
         NBOX(2,I) = (IX - RX0) / SCALEX + 0.5
         IF (NBOX(2,I).LT.NBOX(1,I)) THEN
            IX = NBOX(2,I)
            NBOX(2,I) = NBOX(1,I)
            NBOX(1,I) = IX
            END IF
         NBOX(1,I) = MAX (1, NBOX(1,I))
         NBOX(2,I) = MIN (INPTS, NBOX(2,I))
         NBOXES = I
 40      CONTINUE
 50   IF (NBOXES.LE.0) GO TO 20
C                                       Plot new windows
      CALL YHOLD ('ONNN', JERR)
      CALL YZERO (IGR, IERR)
      IF (IERR.NE.0) GO TO 990
      DY = (ORANGE(2) - ORANGE(1)) / 20.0
      DO 90 I = 1,NBOXES
         IT = NBOX(1,I)
 75      IF (DATA(IT).EQ.FBLANK) THEN
            IT = IT + 1
            IF (IT.GT.NBOX(2,I)) GO TO 90
            GO TO 75
            END IF
         X1 = NBOX(1,I)
         Y = DATA(IT) * XFAC + XOFF
         Y1 = Y + 0.11 * (ORANGE(2) - ORANGE(1))
         Y2 = Y - 0.11 * (ORANGE(2) - ORANGE(1))
         Y1 = MIN (ORANGE(2), MAX (ORANGE(1), Y1))
         Y2 = MIN (ORANGE(2), MAX (ORANGE(1), Y2))
         IT = NBOX(2,I)
 80      IF (DATA(IT).EQ.FBLANK) THEN
            IT = IT - 1
            IF (IT.LT.NBOX(1,I)) GO TO 90
            GO TO 80
            END IF
         X2 = NBOX(2,I)
         Y = DATA(IT) * XFAC + XOFF
         Y3 = Y + 0.11 * (ORANGE(2) - ORANGE(1))
         Y4 = Y - 0.11 * (ORANGE(2) - ORANGE(1))
         Y3 = MIN (ORANGE(2), MAX (ORANGE(1), Y3))
         Y4 = MIN (ORANGE(2), MAX (ORANGE(1), Y4))
         CALL TVVEC (X1, Y1, 1, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TVVEC (X1, Y2, 2, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TVVEC (X2, Y4, 2, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TVVEC (X2, Y3, 2, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TVVEC (X1, Y1, 2, IERR)
         IF (IERR.NE.0) GO TO 990
 90      CONTINUE
      GO TO 990
C                                       terminal error
 900  WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (6)
C                                       Close TTY anyway
 990  CALL ZCLOSE (TTYLUN, TTYIND, JERR)
      CALL YHOLD ('OFFF', JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' OPENING TERMINAL')
 1020 FORMAT ('Position cursor at left edge of window ',I2,
     *   ' hit button')
 1021 FORMAT ('Position cursor at right edge of window ',I2,
     *   ' hit button')
 1900 FORMAT ('ERROR',I5,' WRITING TO TERMINAL')
      END
      SUBROUTINE BTVMOD (DOMODL, DORESI, INPTS, NG, ORANGE, FVEC, PERR,
     *   IERR)
C-----------------------------------------------------------------------
C   BTVMOD plots the residual and model functions on the TEK.  It asks
C   the user for permission to proceed.
C   Inputs:
C       DOMODL   R      > 0. => plot model
C       DORESI   R      > 0. => plot residuals
C       INPTS    I      Number of data points
C       NG       I      Order of baseline (max)
C       ORANGE   R(2)   Plot intensity range (plot units)
C       FVEC     R(*)   data - model
C       PERR     I      > 0 => probable parameter bad
C  Output:
C       IERR     I      TEK error code
C                          101 => blank this solution
C                          102 => User wants to quit
C                          103 => do a retry
C-----------------------------------------------------------------------
      INTEGER   INPTS, NG, PERR, IERR
      REAL      FVEC(*), DOMODL, DORESI, ORANGE(2)
C
      CHARACTER TEMP*1, MSGBUF*80
      INTEGER   I, J, TTYLUN, TTYIND, NPPL, K, JERR
      REAL      XFAC, XOFF, X, Y, XP, YP, TP, DX, DY
      LOGICAL   BLAST, T, F
      INCLUDE 'XBASL2.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA TTYLUN /5/
C-----------------------------------------------------------------------
      IERR = 0
      IF ((DOMODL.LE.0.) .AND. (DORESI.LE.0.)) GO TO 900
      XFAC = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      XOFF = 40000. - XFAC * CATR(IRRAN+1)
C                                       Plot residuals
      K = 0
      IF (DORESI.GT.0.) THEN
C                                       Do any show?
         DO 10 I = 1,INPTS
            IF (DATA(I).EQ.FBLANK) GO TO 10
               Y = FVEC(I) * XFAC + XOFF
               IF ((Y.LT.ORANGE(2)) .AND. (Y.GT.ORANGE(1))) K = K + 1
 10         CONTINUE
         END IF
      IF (K.GT.1) THEN
         CALL YHOLD ('ONNN', I)
         IGR = IGR3
         BLAST = .TRUE.
         DO 30 I = 1,INPTS
            IF (DATA(I).EQ.FBLANK) THEN
               BLAST = .TRUE.
            ELSE
               X = I
               Y = FVEC(I) * XFAC + XOFF
               XP = X * SCALEX + RX0 - RXL
               YP = Y * SCALEY + RY0 - RYL
               TP = MAX (ABS(XP), ABS(YP))
               NPPL = (TP + 9) / 16.0 + 0.5
               IF (BLAST) NPPL = 1
               XP = (RXL - RX0) / SCALEX
               YP = (RYL - RY0) / SCALEY
               DX = (X - XP) / NPPL
               DY = (Y - YP) / NPPL
               DO 25 K = 1,NPPL
                  X = XP + (K-0.6) * DX
                  Y = YP + (K-0.6) * DY
                  Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
                  J = 2
                  IF ((BLAST) .OR. (MOD(K,2).EQ.0)) J = 1
                  CALL TVVEC (X, Y, J, IERR)
                  IF (IERR.NE.0) GO TO 100
                  X = XP + K * DX
                  Y = YP + K * DY
                  Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
                  J = 1
                  CALL TVVEC (X, Y, J, IERR)
                  IF (IERR.NE.0) GO TO 100
 25               CONTINUE
               BLAST = .FALSE.
               END IF
 30         CONTINUE
         CALL YHOLD ('OFFF', I)
         END IF
C                                       Plot model
      IF (DOMODL.GT.0.) THEN
         CALL YHOLD ('ONNN', I)
         IGR = IGR4
         BLAST = .TRUE.
         DO 60 I = 1,INPTS
            IF (DATA(I).EQ.FBLANK) THEN
               BLAST = .TRUE.
            ELSE
               X = I
               Y = (DATA(I) - FVEC(I)) * XFAC + XOFF
               Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
               J = 2
               IF (BLAST) J = 1
               CALL TVVEC (X, Y, J, IERR)
               IF (IERR.NE.0) GO TO 100
               BLAST = .FALSE.
               END IF
 60         CONTINUE
         CALL YHOLD ('OFFF', I)
         END IF
 100  BLAST = PERR.GT.0
C                                       talk to user
 900  CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1900) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
      IF (BLAST) THEN
         WRITE (MSGBUF,1910)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      IF (NG.GT.1) THEN
         WRITE (MSGBUF,1915)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      WRITE (MSGBUF,1920)
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      READ (MSGBUF,1921) TEMP
      CALL CHLTOU (4, TEMP)
      IF ((TEMP.EQ.'B') .OR. (TEMP.EQ.'b')) THEN
         IERR = 101
      ELSE IF ((TEMP.EQ.'Q') .OR. (TEMP.EQ.'q')) THEN
         IERR = 102
      ELSE IF ((TEMP.EQ.'R') .OR. (TEMP.EQ.'r')) THEN
         IF (NG.GT.1) IERR = 103
         END IF
      GO TO 990
C                                       TTY error
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (6)
 990  CALL ZCLOSE (TTYLUN, TTYIND, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('OPEN TERMINAL ERROR',I7)
 1910 FORMAT ('>>>> PARAMETERS SEEM OUT OF RANGE.  SOLUTION PROBABLY ',
     *   'BAD! <<<<')
 1915 FORMAT ('**** Enter RETR to repeat the row (must enter a guess)')
 1920 FORMAT ('**** Enter BAD to flag solution, QUIT to die, ',
     *   'Hit return to continue')
 1921 FORMAT (A1)
 1980 FORMAT ('TERMINAL I/O ERROR',I7)
      END
