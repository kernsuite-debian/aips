LOCAL INCLUDE 'BLANK.INC'
C                                                          Include BLANK
      INCLUDE 'INCS:PMAD.INC'
C                                       Local include for BLANK
      REAL      XSEQIN, XDISKI, XSEQ2N, XDIS2I, XSEQO, XDISKO, BLC(7),
     *   TRC(7), BCHAN, ECHAN, DOCAT, DOALIN, DPARM(10), DOINVR, TVCHAN,
     *   RANGE(2), TXINC, TYINC
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAM2I(3), XCLA2I(2), XNAMOU(3),
     *   XCLAOU(2), XOPCOD, XTVFUN
      CHARACTER NAMEIN*12, CLAIN*6, NAM2IN*12, CLA2IN*6, NAMOUT*12,
     *   CLAOUT*6, OPCODE*4, TVFUNC*4
      DOUBLE PRECISION CATD(128), OLDD(128), NEWD(128), CLPD(128)
      REAL      CATR(256), OLDR(256), NEWR(256), CLPR(256)
      HOLLERITH CATH(256), OLDH(256), NEWH(256), CLPH(256)
      INTEGER   CATBLK(256), CATOLD(256), CATNEW(256), CATCLP(256),
     *   SEQIN, SEQ2IN, SEQOUT, DISKIN, DIS2IN, DISKO, NEWCNO, NE2CNO,
     *   OLDCNO, JBUFSZ, ICODE, NAXSKP, BBLC(7), BTRC(7), NX2LIM(7),
     *   LUNI1, LUNI2, LUNO1, LUNO2, INDI1, INDI2, INDO1, INDO2, TTYLUN,
     *   TTYIND, SBUFF(256), IBUFF1(MABFSS)
      REAL      XBUFF1(MABFSS), XBUFF2(MABFSS), XBUFF3(MABFSS),
     *   XBUFF4(MABFSS)
      EQUIVALENCE (IBUFF1, XBUFF1)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAM2I, XCLA2I,
     *   XSEQ2N, XDIS2I, XNAMOU, XCLAOU, XSEQO, XDISKO, BLC, TRC,
     *   BCHAN, ECHAN, DOCAT, DOALIN, XOPCOD, DPARM, DOINVR, TVCHAN,
     *   RANGE, XTVFUN, TXINC, TYINC
      COMMON /BLKCHR/ NAMEIN, CLAIN, NAM2IN, CLA2IN, NAMOUT, CLAOUT,
     *   OPCODE, TVFUNC
      COMMON /PARMS/ CATOLD, CATNEW, CATCLP, SEQIN, SEQ2IN, SBUFF,
     *   SEQOUT, DISKIN, DIS2IN, DISKO, NEWCNO, NE2CNO, OLDCNO, JBUFSZ,
     *   ICODE, NAXSKP, BBLC, BTRC, NX2LIM, LUNI1, LUNI2, LUNO1, LUNO2,
     *   INDI1, INDI2, INDO1, INDO2, TTYLUN, TTYIND
      COMMON /IMBUF/ XBUFF1, XBUFF2, XBUFF3, XBUFF4
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      EQUIVALENCE (CATNEW, NEWR, NEWD, NEWH)
      EQUIVALENCE (CATOLD, OLDR, OLDD, OLDH)
      EQUIVALENCE (CATCLP, CLPR, CLPD, CLPH)
C                                                          End BLANK.
LOCAL END
      PROGRAM BLANK
C-----------------------------------------------------------------------
C! Blanks regions of an image selected by the user.
C# Map TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2000, 2006, 2008, 2010, 2015
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
C   BLANK blanks out regions of the image purportedly free of real
C   signal using one TV interactive and a variety of batch algorithms.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input image.
C      INCLASS        CLAIN         Class of input image.
C      INSEQ          SEQIN         Seq. of input image.
C      INDISK         DISKIN        Disk number of input image.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTCLASS       CLAOUT        Class of the output image.
C                                   Default is input class.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      BLC(7)         BLC           Bottom left corner of subimage
C                                   of input image.
C      TRC(7)         TRC           Top right corner of subimage.
C      BCHAN          BCHAN         First channel with signal
C      ECHAN          ECHAN         Last channel with signal
C      DOCAT          DOCAT         Catalog avg of blanked pixels
C      OPCODE         OPCODE        Operation type: 'TVCU','SELC',
C                                   'IN2C','FIXW','FLTW','MOMW','FLUW',
C      DPARM          DPARM         (1) window width
C                                   (2) <= 0 -> center 1st moment
C                                       else on peak
C                                   (3) F1 cutoff   (4) F2 cutoff
C                                   (5) > 0: replace blanks by DPARM(6)
C                                   (6) if (5)>0, use instead of blank.
C      TVCHAN                       TV channel to use
C      PIXRANGE                     Min,Max of image intensity
C                                   Max <= Min => entire range
C      TXINC                        X increment in TV loads
C      TYINC                        Y increment in TV loads
C  BCHAN......First x pixel (after application of BLC(1)) which
C             is allowed to be left unblanked in the output
C             image.  This option is useful only if the average
C             of the blanked pixels is written and provides a
C             wider region over which the averaging is performed.
C             The actual number of points on the output image row
C             is given by ECHAN - BCHAN + 1.   (0 => 1).
C  ECHAN......The last x pixel (after application of BLC(1))
C             which is allowed to be left unblanked in the output
C             image.  0 => TRC(1) - BLC(1) + 1.
C  OPCODE.....How the program is to do blanking:
C             'TVCU' : display image planes (any transposition)
C                      on TV and mark signal regions with cursor
C             'FIXW' : fixed window BCHAN through ECHAN
C             'FLTW' : floating window of fixed width
C             'MOMW' : window with width set by 2nd moment
C             'FLUW' : window set by fluxes (from peak through
C                      last pixel > F1 on each side plus "slop")
C             'SELC' : If F1 < F2, keep all F <= 1.0, F >= F2
C                      If F1 > F2, keep all F1 >= F >= F2.
C             'IN2C' : Like 'SELC', except the test is done on a
C                      second image (IN2NAME et al.) rather than
C                      the input image.
C  DPARM......Parameters:
C             (1) 'FLTW' window size in x pixels.
C                 'MOMW' window size = DPARM(1) * 2nd moment
C                 'SELC','IN2C','FLUW' "slop" - extend "good"
C                     areas by DPARM(1) pixels in each direction
C             (2) 'FLTW','MOMW','FLUW' center window on first
C                     moment (<= 0) or on peak intensity (> 0).
C             (3) (Called F1 above for 'SELC','IN2C','FLUW'.)
C                 Also ignore in moment computations all
C                 intensities < F1.  Units are those of image.
C             (4) (Called F2 above for 'SELC','IN2C'.)  Units are
C                 those of image.
C             (5) <= 0. -> retain existing magic-value blanks and
C                 use magic-values for newly blanked pixels.
C                 > 0.  -> change all magic values to DPARM(6) on
C                 output.
C             (6) if (5)>0, use instead of blank.
C   Programmer Eric W. Greisen:  October 1983
C     MRC 86/May/23: In BLNKIN change assignment of NA(1) and NA(2),
C                    In DO1BLK avoid divide by zero.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'BLANK.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'BLANK '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL BLNKIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the blanking routine
      IF (IRET.NE.0) GO TO 990
         IF (ICODE.NE.7) CALL BLNKDO (IRET)
         IF (ICODE.EQ.7) CALL BLNKTV (IRET)
C                                       Finish image(s), do History
         IF (IRET.EQ.0) CALL BLNKOU (IRET)
C                                       Close down files, etc.
 990  CALL DIE (IRET, SBUFF)
C
 999  STOP
      END
      SUBROUTINE BLNKIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   BLNKIN gets input parameters for BLANK and creates an output file
C   IF requested for the residual map.
C   Inputs:  PRGN    C*6       Program name
C   Output:  IRET    I         Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER   STAT*4,  PRGN*6, BLANK*6, SEQTYP(2)*4, CODES(8)*4,
     *   MTYPE*2
      INTEGER  IRET
      INTEGER   IERR, NPARM, IROUND, NAX, I, NCODE, NA(2), J, K, KA,
     *   NX1, NX2, INPSEQ
      REAL      EPS, AXV
      DOUBLE PRECISION DAXV
      LOGICAL   EQUAL
      INCLUDE 'BLANK.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA BLANK /'      '/
      DATA SEQTYP /'AVGB','LK  '/
      DATA NCODE, CODES /8, 'IN2C','SELC','FIXW','FLTW','MOMW','FLUW',
     *   'TVCU','RADI'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      CALL FILL (10, 0, IBAD)
C                                       Get input parameters.
      NPARM = 57
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SBUFF, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Convert characters
 10   CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAM2I, NAM2IN)
      CALL H2CHR (6, 1, XCLA2I, CLA2IN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (2, 1, XTVFUN, TVFUNC)
C                                       Using the TV
      ICODE = 0
      DO 15 I = 1,NCODE
         IF (OPCODE.EQ.CODES(I)) ICODE = I
 15      CONTINUE
      IF (ICODE.LE.0) THEN
         WRITE (MSGTXT,1020) OPCODE
         CALL MSGWRT (8)
         IRET = 4
      ELSE IF (ICODE.EQ.7) THEN
         IF ((NPOPS.GT.NINTRN) .OR. (NTVDEV.LE.0)) THEN
            ICODE = 0
            MSGTXT = 'THERE IS NO TV ASSIGNED TO YOUR AIPS'
            CALL MSGWRT (8)
            IRET = 8
            END IF
         END IF
C                                       Restart AIPS
      IF ((RQUICK) .AND. ((ICODE.NE.7) .OR. (IRET.NE.0))) CALL RELPOP
     *    (IRET, SBUFF, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
      LUNI1 = 16
      LUNI2 = 17
      LUNO1 = 18
      LUNO2 = 19
      TTYLUN = 5
      TTYIND = 0
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, STAT, SBUFF, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
C                                       Read CATBLK and mark 'READ'.
 40   CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'READ', SBUFF, IERR)
      IF (IERR.EQ.0) GO TO 50
         WRITE (MSGTXT,1040) IERR
         GO TO 990
 50   NCFILE = 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATOLD(KIDIM), CATOLD(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Second input file
      NCFILE = 2
      FRW(2) = -1
      NAXSKP = -1
      EPS = 0.2
      CALL FILL (7, 1, BBLC)
      CALL FILL (7, 1, BTRC)
      CALL FILL (7, 1, CATCLP(KINAX))
      IF (ICODE.NE.1) GO TO 100
C                                       Get CATBLK of 2nd old file.
         NEWCNO = 1
         DIS2IN = IROUND (XDIS2I)
         SEQ2IN = IROUND (XSEQ2N)
         MTYPE = 'MA'
         CALL CATDIR ('SRCH', DIS2IN, NEWCNO, NAM2IN, CLA2IN, SEQ2IN,
     *      MTYPE, NLUSER, STAT, SBUFF, IERR)
         IF (IERR.EQ.0) GO TO 55
            WRITE (MSGTXT,1030) IERR, NAM2IN, CLA2IN, SEQ2IN, DIS2IN,
     *         NLUSER
            GO TO 990
C                                       Read CATBLK and mark 'READ'.
 55      CALL CATIO ('READ', DIS2IN, NEWCNO, CATCLP, 'READ', SBUFF,
     *      IERR)
         IF (IERR.EQ.0) GO TO 60
            WRITE (MSGTXT,1055) IERR
            GO TO 990
 60      FVOL(NCFILE) = DIS2IN
         FCNO(NCFILE) = NEWCNO
         FRW(NCFILE) = 0
C                                       Decide IO pattern
         DO 65 I = 1,2
            NA(I) = 0
            K = KHCTP + (I-1) * 2
            DO 65 J = I,7
               IF (NA(I).NE.0) GO TO 65
               CALL CHCOMP (8, 1, CLPH(K), 1, OLDH(KHCTP+(J-1)*2),
     *            EQUAL)
               IF (EQUAL) NA(I) = J
 65         CONTINUE
         NAXSKP = 0
         IF (DOALIN.LT.-1.5) GO TO 75
         IF ((NA(1).EQ.0) .OR. (NA(2).EQ.0)) GO TO 70
         IF ((NA(1).GT.2) .OR. (NA(2).GT.3)) GO TO 70
         IF (NA(2).LE.NA(1)) GO TO 70
            IF (NA(2).GT.2) NAXSKP = 2
            IF (NA(1).EQ.2) NAXSKP = 1
            GO TO 80
C                                       possible non-alignment
 70      IF (DOALIN.LE.-0.1) GO TO 75
            WRITE (MSGTXT,1070)
            GO TO 990
 75      CONTINUE
            NA(1) = 1
            NA(2) = 2
C                                       set corners, check alignment
 80      KA = 0
         IF (NAXSKP.GT.0) KA = 1
         NX1 = CATOLD(KIDIM)
         NX2 = CATCLP(KIDIM)
         DO 90 I = 1,7
            BBLC(I) = 1
            BTRC(I) = 1
            J = I - 1
            IF (CATCLP(KINAX+J).LT.1) CATCLP(KINAX+J) = 1
            IF ((I.GT.NX2) .OR. (I+KA.GT.NX1) .OR. (CATCLP(KINAX+J)
     *         .LE.1)) GO TO 90
               IF (I.LE.2) K = NA(I) - 1
               IF (I.GT.2) K = I + KA - 1
               AXV = CLPR(KRCRP+J) - OLDR(KRCRP+K) + BLC(K+1)
               IF (DOALIN.LT.-1.5) AXV = BLC(K+1)
               IF ((DOALIN.LE.-0.1) .OR. (CLPR(KRCIC+J).EQ.0.0))
     *            GO TO 82
                  DAXV = OLDD(KDCRV+K) + OLDR(KRCIC+K)*(BLC(K+1) -
     *               OLDR(KRCRP+K))
                  AXV = (DAXV - CLPD(KDCRV+J)) / CLPR(KRCIC+J) +
     *               CLPR(KRCRP+J)
 82            BBLC(I) = IROUND (AXV)
               BTRC(I) = BBLC(I) + TRC(K+1) - BLC(K+1) + EPS
               IF (BTRC(I).LT.1) GO TO 95
               IF (BBLC(I).GT.CATCLP(KINAX+J)) GO TO 95
C                                       smaller subim needed?
               IF (BBLC(I).GE.1) GO TO 84
                  BLC(K+1) = BLC(K+1) + 1 - BBLC(I)
                  AXV = AXV + 1 - BBLC(I)
                  BBLC(I) = 1
 84            IF (BTRC(I).LE.CATCLP(KINAX+J)) GO TO 86
                  TRC(K+1) = TRC(K+1) + CATCLP(KINAX+J) - BTRC(I)
                  BTRC(I) = CATCLP(KINAX+J)
 86            IF (BTRC(I).LT.BBLC(I)) GO TO 95
               IF (BLC(K+1).GT.CATOLD(KINAX+K)) GO TO 95
               IF (TRC(K+1).LT.1) GO TO 95
C                                       full alignment check
               IF (DOALIN.LT.0.1) GO TO 90
                  IF (ABS(BBLC(I)-AXV).GT.EPS) GO TO 95
                  CALL CHCOMP (8, 1, CLPH(KHCTP+J*2), 1,
     *               OLDH(KHCTP+K*2), EQUAL)
                  IF (.NOT.EQUAL) GO TO 95
                  AXV = EPS * EPS * ABS(OLDR(KRCIC+K))
                  IF (ABS(OLDR(KRCIC+K)-CLPR(KRCIC+J)).GT.AXV) GO TO 95
                  IF (ABS(OLDR(KRCRT+K)-CLPR(KRCRT+J)).GT.1.0) GO TO 95
 90            CONTINUE
            GO TO 100
C                                       Failure to align
 95      CONTINUE
            WRITE (MSGTXT,1095)
            GO TO 990
C                                       Output file creation
C                                       Copy old CATBLK to new.
 100  CALL COPY (256, CATOLD, CATBLK)
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
      INPSEQ = SEQOUT
C                                       Get user modification to CATBLK
      IRET = 4
      CALL BLKHED (IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Create output file for blanked
      IRET = 4
      NEWCNO = 1
      CALL MCREAT (DISKO, NEWCNO, SBUFF, IERR)
      IF (IERR.EQ.0) GO TO 110
         WRITE (MSGTXT,1100) IERR
         GO TO 990
 110  NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 2
      SEQOUT = CATBLK(KIIMS)
C                                       copy some keywords
      CALL KEYPCP (DISKIN, OLDCNO, DISKO, NEWCNO, 0, ' ', IERR)
C                                       created avg of blanked image
      IF (ICODE.EQ.8) DOCAT = 0.0
      IF (DOCAT.LE.0.0) GO TO 150
C                                       save the blanked header
         CALL COPY (256, CATBLK, XBUFF2)
C                                       Basic output header: results
         CATBLK(KIDIM) = CATBLK(KIDIM) - 1
         NAX = CATBLK(KIDIM)
         DO 120 I = 1,NAX
            CATBLK(KINAX+I-1) = CATBLK(KINAX+I)
            CATR(KRCRP+I-1) = CATR(KRCRP+I)
            CATR(KRCRT+I-1) = CATR(KRCRT+I)
            CATR(KRCIC+I-1) = CATR(KRCIC+I)
            CATD(KDCRV+I-1) = CATD(KDCRV+I)
            CALL CHCOPY (8, 1, CATR(KHCTP+I*2), 1,
     *         CATR(KHCTP+(I-1)*2))
 120        CONTINUE
         DO 125 I = NAX,6
            CATBLK(KINAX+I) = 1
 125        CONTINUE
         CATBLK(KIIMS) = INPSEQ
         CALL CHR2H (6, SEQTYP, KHIMCO, CATR(KHIMC))
C                                       Create
         DISKO = XDISKO + 0.01
         NEWCNO = 1
         CALL MCREAT (DISKO, NEWCNO, SBUFF, IERR)
         IF (IERR.EQ.0) GO TO 130
            WRITE (MSGTXT,1125) IERR, SEQTYP
            GO TO 990
C                                       Record the creation
 130     NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO
         FCNO(NCFILE) = NEWCNO
         FRW(NCFILE) = 2
         CALL COPY (256, CATBLK, CATNEW)
         CALL COPY (256, XBUFF2, CATBLK)
C                                       copy some keywords
         CALL KEYPCP (DISKIN, OLDCNO, DISKO, NEWCNO, 0, ' ', IERR)
C                                       OK ! - clean up
 150  IRET = 0
      DISKO = FVOL(3)
      NEWCNO = FCNO(3)
C                                       check defaults
      IF (DPARM(1).LE.0.0) DPARM(1) = 0.0
      IF ((ICODE.EQ.4) .AND. (DPARM(1).LE.1.0)) DPARM(1) =
     *   (ECHAN - BCHAN + 1.0) / 2.0
      IF ((ICODE.EQ.5) .AND. (DPARM(1).LE.0.1)) DPARM(1) = 3.0
      IF ((ICODE.GT.2) .OR. (DPARM(3).NE.DPARM(4))) GO TO 999
         DPARM(3) = MIN (0.0, 0.1*OLDR(KRDMN))
         DPARM(4) = MAX (0.0, 0.1*OLDR(KRDMX))
         GO TO 999
C
 990  CALL MSGWRT (8)
 995  IF ((RQUICK) .AND. (ICODE.EQ.7)) CALL RELPOP (IRET, SBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLNKIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT ('OPCODE ''',A4,''' IS NOT RECOGNIZED')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' READING INPUT IMAGE CATBLK')
 1055 FORMAT ('ERROR',I3,' READING CLIP IMAGE CATBLK')
 1070 FORMAT ('UNABLE TO ALIGN THE AXES - CHECK TRANSPOSITION')
 1095 FORMAT ('INPUT AND CLIP IMAGES DO NOT OVERLAP: CHECK HEADERS')
 1100 FORMAT ('ERROR',I5,' CREATING OUTPUT FILE')
 1125 FORMAT ('ERROR',I5,' CREATING FILE OF AVG BLANKED PIXELS')
      END
      SUBROUTINE BLKHED (IRET)
C-----------------------------------------------------------------------
C   BLKHED modifies the new image header for the subimaging.
C   Input:
C      CATBLK(256)    I     Output catalog header, also CATR, CATD
C      CATOLD(256)    I     Input catalog header, also OLDR, OLDD
C   Output:
C      CATBLK(256)    I     Modified output catalog header.
C      IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER FCHARS(3)*4, CHTM12*12
      INTEGER   IRET, IROUND, I
      LOGICAL   EQUAL
      REAL      TEMPB, TEMPT
      INCLUDE 'BLANK.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check input axes
      IF ((ICODE.LE.2) .OR. (ICODE.GE.7)) GO TO 20
         DO 10 I = 1,3
            CALL H2CHR (4, 1, CATR(KHCTP), CHTM12)
            EQUAL = FCHARS(I)(1:4).EQ. CHTM12(1:4)
            IF (EQUAL) GO TO 20
 10         CONTINUE
         MSGTXT = 'WARNING: FIRST AXIS NOT FREQUENCY OR VELOCITY'
         CALL MSGWRT (6)
C                                       check input parms
 20   I = TXINC + 0.01
      IF (I.LE.0) I = 1
      TXINC = I
      I = TYINC + 0.01
      IF (I.LE.0) I = 1
      TYINC = I
C                                       Baseline
      IF (ICODE.EQ.8) THEN
         BCHAN = 1
         ECHAN = TRC(1) - BLC(1) + 1.0
      ELSE
         BCHAN = IROUND (BCHAN)
         ECHAN = IROUND (ECHAN)
         IF (BCHAN.LE.0.) BCHAN = 1.0
         IF ((ECHAN.LE.0.) .OR. (ECHAN.GT.TRC(1)-BLC(1)+1)) ECHAN =
     *      TRC(1) - BLC(1) + 1.0
         IF (BCHAN.GE.ECHAN) THEN
            WRITE (MSGTXT,1020) BCHAN, ECHAN
            IRET = 8
            GO TO 990
            END IF
         END IF
      TEMPB = BLC(1)
      TEMPT = TRC(1)
      BLC(1) = BLC(1) + BCHAN - 1.0
      TRC(1) = BLC(1) + ECHAN - BCHAN
      CALL SUBHDR (BLC, TRC, 1.0, 1.0)
      BLC(1) = TEMPB
      TRC(1) = TEMPT
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('BCHAN, ECHAN = ',2F7.0,' IMPROPER')
      END
      SUBROUTINE BLNKDO (IRET)
C-----------------------------------------------------------------------
C   BLNKDO sends image one row at a time to the blanking routine and
C   then writes the modified data.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER IFILE*48
      LOGICAL   TBLNKD
      INTEGER   IRET, IROUND, IBIND1, IBIND2, OBIND1, OBIND2, NYI, NXI,
     *   WINI(4), NXO, NYO, WINO(4), BOI, IINCR, BOO, LIM2, LIM3, LIM4,
     *   LIM5, LIM6, LIM7, I1, I2, I3, I4, I5, I6, I7, IPOS(7), CORN(7),
     *   BOTEMP, KOFF, LIMO, LIMIT, LIM1, LIM1C, LOFF, WINT(4),
     *   CORNOU(8), WINI2(4), IPLDO
      REAL      PLTODO, PLDONE, PMAX, PMIN, OUTMAX, OUTMIN
      LOGICAL   T, F, BLNKD
      INCLUDE 'BLANK.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI1, INDI1, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Init second input map
      IF (NAXSKP.GE.0) THEN
         CALL ZPHFIL ('MA', FVOL(2), FCNO(2), 1, IFILE, IRET)
         CALL ZOPEN (LUNI2, INDI2, FVOL(2), IFILE, T, F, T, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
        END IF
C                                       output blanked
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO1, INDO1, DISKO, IFILE, T, T, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1015) IRET
         GO TO 990
         END IF
C                                       For residual map:
      IF (DOCAT.GT.0.) THEN
         CALL ZPHFIL ('MA', FVOL(4), FCNO(4), 1, IFILE, IRET)
         CALL ZOPEN (LUNO2, INDO2, FVOL(4), IFILE, T, T, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
         END IF
C                                       Setup for I/O
      NXI = CATOLD(KINAX)
      NYI = CATOLD(KINAX+1)
      NXO = CATBLK(KINAX)
      NYO = CATBLK(KINAX+1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
      WINI2(1) = BBLC(1)
      WINI2(2) = BBLC(2)
      WINI2(3) = BTRC(1)
      WINI2(4) = BTRC(2)
      LIM1C = BTRC(1) - BBLC(1) + 1
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
      WINT(1) = 1
      WINT(2) = 1
      WINT(3) = CATNEW(KINAX)
      WINT(4) = CATNEW(KINAX+1)
      OUTMAX = -1.0E20
      OUTMIN = -OUTMAX
      PMAX = OUTMAX
      PMIN = OUTMIN
      BLNKD = F
      TBLNKD = .FALSE.
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM4 = TRC(4) - BLC(4) + 1.01
      LIM5 = TRC(5) - BLC(5) + 1.01
      LIM6 = TRC(6) - BLC(6) + 1.01
      LIM7 = TRC(7) - BLC(7) + 1.01
      IPLDO = 0
      PLDONE = 0.0
      PLTODO = REAL(LIM7) * REAL(LIM4) * REAL(LIM5) * REAL(LIM6)
      PLTODO = PLTODO * CATBLK(KINAX+2)
      KOFF = 0
      IF (NAXSKP.GT.0) KOFF = -1
      CORN(7) = 1
      LIMO = CATBLK(KINAX) - 1
      LOFF = KINAX - 1 + KOFF
      CORNOU(8) = 1
      IINCR = 1
      IF (NAXSKP.EQ.1) IINCR = 0
C                                       Loop
      DO 700 I7 = 1,LIM7
         CORNOU(7) = I7
         IPOS(7) = BLC(7) + I7 - 0.9
         CORN(7+KOFF) = BBLC(7+KOFF) + I7 - 2
         CORN(7+KOFF) = MOD (CORN(7+KOFF), CATCLP(7+LOFF)) + 1
         DO 600 I6 = 1,LIM6
            CORNOU(6) = I6
            IPOS(6) = BLC(6) + I6 - 0.9
            CORN(6+KOFF) = BBLC(6+KOFF) + I6 - 2
            CORN(6+KOFF) = MOD (CORN(6+KOFF), CATCLP(6+LOFF)) + 1
            DO 500 I5 = 1,LIM5
               CORNOU(5) = I5
               IPOS(5) = BLC(5) + I5 - 0.9
               CORN(5+KOFF) = BBLC(5+KOFF) + I5 - 2
               CORN(5+KOFF) = MOD (CORN(5+KOFF), CATCLP(5+LOFF)) + 1
               DO 400 I4 = 1,LIM4
                  CORNOU(4) = I4
                  IPOS(4) = BLC(4) + I4 - 0.9
                  CORN(4+KOFF) = BBLC(4+KOFF) + I4 - 2
                  CORN(4+KOFF) = MOD (CORN(4+KOFF), CATCLP(4+LOFF)) + 1
C                                       Init. files: clip input maybe
                  IF (NAXSKP.GT.0) THEN
                     CALL COMOFF (CATCLP(KIDIM), CATCLP(KINAX),
     *                  CORN(3), BOTEMP, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1050) IRET
                        GO TO 990
                        END IF
                     BOI = BOTEMP + 1
                     CALL MINIT ('READ', LUNI2, INDI2, WINI2(3),
     *                  WINI2(4), WINI2, XBUFF2, JBUFSZ, BOI, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1055) IRET
                        GO TO 990
                        END IF
                     END IF
C                                       Init. file: output average
                  IF (DOCAT.GT.0.0) THEN
                     CALL COMOFF (CATNEW(KIDIM), CATNEW(KINAX),
     *                  CORNOU(4), BOTEMP, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1060) IRET
                        GO TO 990
                        END IF
                     BOI = BOTEMP + 1
                     CALL MINIT ('WRIT', LUNO2, INDO2, WINT(3), WINT(4),
     *                  WINT, XBUFF4, JBUFSZ, BOI, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1065) IRET
                        GO TO 990
                        END IF
                     END IF
                  DO 300 I3 = 1,LIM3
                     CORNOU(3) = I3
                     IPOS(3) = BLC(3) + I3 - 0.9
                     CORN(3+KOFF) = BBLC(3+KOFF) + I3 - 2
                     CORN(3+KOFF) = MOD (CORN(3+KOFF), CATCLP(3+LOFF))
     *                  + 1
                     PLDONE = PLDONE + 1
                     IPLDO = IPLDO + 1
                     WRITE (MSGTXT,1070) PLDONE, PLTODO
                     IF (IPLDO.EQ.1) CALL MSGWRT (1)
                     IF (IPLDO.GE.16) IPLDO = 0
C                                       Init. files, first input.
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3), BOTEMP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1099) IRET
            GO TO 990
            END IF
         BOI = BOTEMP + 1
         CALL MINIT ('READ', LUNI1, INDI1, NXI, NYI, WINI, XBUFF1,
     *      JBUFSZ, BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
C                                       Init blanked file.
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORNOU(3), BOTEMP,
     *      IRET)
         BOO = BOTEMP + 1
         CALL MINIT ('WRIT', LUNO1, INDO1, NXO, NYO, WINO, XBUFF3,
     *      JBUFSZ, BOO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1105) IRET
            GO TO 990
            END IF
C                                       Init. files, first input.
         IF (NAXSKP.EQ.0) THEN
            CALL COMOFF (CATCLP(KIDIM), CATCLP(KINAX), CORN(3), BOTEMP,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1050) IRET
               GO TO 990
               END IF
            BOI = BOTEMP + 1
            CALL MINIT ('READ', LUNI2, INDI2, WINI2(3), WINI2(4), WINI2,
     *         XBUFF2, JBUFSZ, BOI, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1055) IRET
               GO TO 990
               END IF
            END IF
C                                       read clip maybe
         IF (NAXSKP.GT.0) THEN
            CALL MDISK ('READ', LUNI2, INDI2, XBUFF2, IBIND2, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
               END IF
            END IF
C                                       "write" output avg
         OBIND2 = 1
         IF (DOCAT.GT.0.0) THEN
            CALL MDISK ('WRIT', LUNO2, INDO2, XBUFF4, OBIND2, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1145) IRET
               GO TO 990
               END IF
            END IF
         DO 250 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.9
            IPOS(1) = IROUND (BLC(1))
C                                       Read.
            CALL MDISK ('READ', LUNI1, INDI1, XBUFF1, IBIND1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1150) IRET
               GO TO 990
               END IF
C                                       Write.
            CALL MDISK ('WRIT', LUNO1, INDO1, XBUFF3, OBIND1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1175) IRET
               GO TO 990
               END IF
C                                       read clip if needed
            IF (NAXSKP.EQ.0) THEN
               CALL MDISK ('READ', LUNI2, INDI2, XBUFF2, IBIND2, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1120) IRET
                  GO TO 990
                  END IF
               END IF
C                                       Call function: DO1BLK
            IF (ICODE.EQ.1) THEN
               CALL DO1BLK (IPOS, XBUFF1(IBIND1), XBUFF2(IBIND2), IINCR,
     *            XBUFF3(OBIND1), XBUFF4(OBIND2), IRET)
            ELSE IF (ICODE.EQ.8) THEN
               CALL DOPBLK (IPOS, XBUFF1(IBIND1), XBUFF3(OBIND1), IRET)
            ELSE
               CALL DO1BLK (IPOS, XBUFF1(IBIND1), XBUFF1(IBIND1), IINCR,
     *            XBUFF3(OBIND1), XBUFF4(OBIND2), IRET)
               END IF
            IF (DOCAT.GT.0.0) OBIND2 = OBIND2 + 1
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1210) IRET
               GO TO 990
               END IF
C                                       Check max, min, blanking.
            LIMIT = OBIND1 + LIMO
            DO 220 I1 = OBIND1,LIMIT
               IF (XBUFF3(I1).EQ.FBLANK) THEN
                  BLNKD = .TRUE.
               ELSE
                  OUTMAX = MAX (OUTMAX, XBUFF3(I1))
                  OUTMIN = MIN (OUTMIN, XBUFF3(I1))
                  END IF
 220           CONTINUE
            IF (DOCAT.GT.0.0) THEN
               I1 = OBIND2 - 1
               IF (XBUFF4(I1).EQ.FBLANK) THEN
                  TBLNKD = .TRUE.
               ELSE
                  PMAX = MAX (PMAX, XBUFF4(I1))
                  PMIN = MIN (PMIN, XBUFF4(I1))
                  END IF
               END IF
 250        CONTINUE
C                                       Flush buffers for blanked
         CALL MDISK ('FINI', LUNO1, INDO1, XBUFF3, OBIND1, IRET)
         IF (IRET.EQ.0) GO TO 300
            WRITE (MSGTXT,1175) IRET
            GO TO 990
 300     CONTINUE
C                                       Flush buffers for avg.
                  IF (DOCAT.LE.0.0) GO TO 400
                     CALL MDISK ('FINI', LUNO2, INDO2, XBUFF4, OBIND2,
     *                  IRET)
                     IF (IRET.EQ.0) GO TO 400
                        WRITE (MSGTXT,1145) IRET
                        GO TO 990
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Update CATBLK in core only
      CATR(KRDMX) = OUTMAX
      CATR(KRDMN) = OUTMIN
      NEWR(KRDMX) = PMAX
      NEWR(KRDMN) = PMIN
C                                       Mark blanking in CATBLK.
      CATR(KRBLK) = 0.0
      IF (BLNKD) CATR(KRBLK) = FBLANK
      NEWR(KRBLK) = 0.0
      IF (TBLNKD) NEWR(KRBLK) = FBLANK
C                                       Close files
      CALL ZCLOSE (LUNI1, INDI1, IRET)
      IF (ICODE.EQ.1) CALL ZCLOSE (LUNI2, INDI2, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLNKDO: ERROR',I3,' OPENING INPUT FILE')
 1010 FORMAT ('BLNKDO: ERROR',I3,' OPENING CLIPPING FILE')
 1015 FORMAT ('BLNKDO: ERROR',I3,' OPENING AVERAGE BLANKED FILE')
 1020 FORMAT ('BLNKDO: ERROR',I5,' OPENING OUTPUT FILE')
 1050 FORMAT ('BLNKDO: COMOFF (CLIP) ERROR',I3)
 1055 FORMAT ('BLNKDO: MINIT (CLIP) ERROR',I3)
 1060 FORMAT ('BLNKDO: COMOFF (AVG OUT) ERROR',I3)
 1065 FORMAT ('BLNKDO: MINIT (AVG OUT) ERROR',I3)
 1070 FORMAT ('BEGIN PLANE',F8.0,' OF',F8.0)
 1099 FORMAT ('BLNKDO: COMOFF (INPUT) ERROR',I3)
 1100 FORMAT ('BLNKDO: MINIT (INPUT) ERROR',I3)
 1105 FORMAT ('BLNKDO: MINIT (OUTPUT) ERROR',I3)
 1120 FORMAT ('BLNKDO: READ (CLIP) ERROR',I3)
 1145 FORMAT ('BLNKDO: WRITE (AVG OUT) ERROR',I3)
 1150 FORMAT ('BLNKDO: READ (INPUT) ERROR',I3)
 1175 FORMAT ('BLNKDO: WRITE (OUTPUT) ERROR',I3)
 1210 FORMAT ('BLNKDO: DO1BLK ERROR',I3)
      END
      SUBROUTINE DO1BLK (IPOS, IND1, IND2, IINCR, OUTD1, OUTD2, IRET)
C-----------------------------------------------------------------------
C   DO1BLK applies the windowing algorithms to each row - summing and
C   blanking pixels outside the window.
C   Inputs:
C      IPOS(7)   I    BLC (input image) of first value in DATA
C      IND1(*)   R    Input image row
C      IND2(*)   R    Clip image row (ICODE = 1 or 2)
C      IINCR     I    Increment to use in clip image row (0 or 1 only!)
C   Values from commons:
C      FBLANK    R    Value of blanked pixel.
C      BCHAN, ECHAN  R     Channel (x pixel) range of good data
C      BLC, TRC (7)  R     Window corners in original image
C   Output:
C      OUTD1(*)  R    Output row : input partly blanked
C      OUTD2(1)  R    Average of those pixels blanked
C      IRET      I    Return code   0 => OK
C                               >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IINCR, IRET
      REAL      IND1(*), IND2(*), OUTD1(*), OUTD2
      INTEGER   I, J, IDA, ID1, ID2, IW1, IW2, NBL, NBL2, ICX, IBCHAN,
     *   IECHAN, IC
      REAL      SBL, ST, ST1, ST2, TEMP, CC, CM
      LOGICAL   KEEP
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'BLANK.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       sum up edges if any
      IDA = DPARM(1) + 0.01
      NBL = 0
      NBL2 = 0
      SBL = 0.0
      IBCHAN = BCHAN - 0.99
      IECHAN = ECHAN + 1.01
      ICX = TRC(1) - BLC(1) + 1.01
      DO 10 I = 1,IBCHAN
         NBL2 = NBL2 + 1
         IF (IND1(I).ne.FBLANK) then
            NBL = NBL + 1
            SBL = SBL + IND1(I)
            END IF
 10      CONTINUE
      IBCHAN = IBCHAN + 1
      DO 20 I = IECHAN,ICX
         NBL2 = NBL2 + 1
         IF (IND1(I).NE.FBLANK) THEN
            NBL = NBL + 1
            SBL = SBL + IND1(I)
            END IF
 20      CONTINUE
      IECHAN = IECHAN - 1
C                                       Move input to output row
      IC = IECHAN - IBCHAN + 1
      CALL RCOPY (IC, IND1(IBCHAN), OUTD1)
C                                       Fixed window done
      IF (ICODE.EQ.3) GO TO 900
C                                       Clip with some image
      IF (ICODE.LE.2) THEN
C                                       1 pixel clips a row
         IF (IINCR.LE.0) THEN
            I = IPOS(2) - BLC(2) + 1.01
            ICX = TRC(2) - BLC(2) + 1.01
            ID1 = MAX (1, I-IDA)
            ID2 = MIN (ICX, I+IDA)
            IF (ID1.GT.ID2) GO TO 800
C                                       keep if outside F1-F2
            IF (DPARM(3).LE.DPARM(4)) THEN
               DO 30 J = ID1,ID2
                  IF (IND2(J).NE.FBLANK) THEN
                     IF (IND2(J).LE.DPARM(3)) GO TO 900
                     IF (IND2(J).GE.DPARM(4)) GO TO 900
                     END IF
 30               CONTINUE
C                                       keep if inside F1-F2
            ELSE
               DO 40 J = ID1,ID2
                  IF (IND2(J).NE.FBLANK) THEN
                     IF ((IND2(J).LE.DPARM(3)) .AND.
     *                  (IND2(J).GE.DPARM(4))) GO TO 900
                     END IF
 40               CONTINUE
               END IF
C                                       go flag row
            GO TO 800
C                                       Clip with row & extend
C                                       keep if all outside F1-F2
         ELSE IF (DPARM(3).LE.DPARM(4)) THEN
            DO 70 I = IBCHAN,IECHAN
               ID1 = MAX (1, I-IDA)
               ID2 = MIN (ICX, I+IDA)
               KEEP = .FALSE.
               DO 65 J = ID1,ID2
                  IF (IND2(J).NE.FBLANK) THEN
                     IF (IND2(J).LE.DPARM(3)) KEEP = .TRUE.
                     IF (IND2(J).GE.DPARM(4)) KEEP = .TRUE.
                     END IF
 65               CONTINUE
               NBL2 = NBL2 + 1
               IF ((.NOT.KEEP) .AND. (IND1(I).NE.FBLANK)) THEN
                  NBL = NBL + 1
                  SBL = SBL + IND1(I)
                  OUTD1(I-IBCHAN+1) = FBLANK
                  END IF
 70            CONTINUE
            GO TO 900
C                                       keep inside F1-F2
         ELSE
            DO 90 I = IBCHAN,IECHAN
               ID1 = MAX (1, I-IDA)
               ID2 = MIN (ICX, I+IDA)
               KEEP = .FALSE.
               DO 85 J = ID1,ID2
                  IF (IND2(J).NE.FBLANK) THEN
                     IF ((IND2(J).LE.DPARM(3)) .AND.
     *                  (IND2(J).GE.DPARM(4))) KEEP = .TRUE.
                     END IF
 85               CONTINUE
               NBL2 = NBL2 + 1
               IF ((.NOT.KEEP) .AND. (IND1(I).NE.FBLANK)) THEN
                  NBL = NBL + 1
                  SBL = SBL + IND1(I)
                  OUTD1(I-IBCHAN+1) = FBLANK
                  END IF
 90            CONTINUE
            GO TO 900
            END IF
         END IF
C                                       Floating windows
C                                       find peak
      IF (DPARM(2).GT.0.0) THEN
         CC = 0.0
         CM = DPARM(3)
         DO 105 I = IBCHAN,IECHAN
            IF ((IND1(I).NE.FBLANK) .AND. (IND1(I).GE.CM)) THEN
               CC = I
               CM = IND1(I)
               END IF
 105        CONTINUE
         IF (CC.LE.0.0) GO TO 800
         END IF
C                                       Find moments
      IF ((ICODE.EQ.5) .OR. (DPARM(2).LE.0.0)) THEN
         IDA = 0
         ST = 0.0
         ST1 = 0.0
         ST2 = 0.0
         DO 115 I = IBCHAN,IECHAN
            TEMP = IND1(I)
            IF ((TEMP.NE.FBLANK) .AND. (TEMP.GE.DPARM(3))) THEN
               IDA = IDA + 1
               ST = ST + TEMP
               TEMP = TEMP * I
               ST1 = ST1 + TEMP
               ST2 = ST2 + TEMP * I
               END IF
 115        CONTINUE
         IF ((IDA.LE.1) .OR. (ST.EQ.0.0)) GO TO 800
         ST1 = ST1 / ST
         ST2 = ST2 / ST - ST1 * ST1
         IF ((ST2.LT.1.0) .AND. (ICODE.EQ.5)) GO TO 800
         ST2 = SQRT (ABS (ST2))
         IF (DPARM(2).LE.0.0) CC = ST1
         END IF
C                                       Set-width windows
      IF (ICODE.NE.6) THEN
         ST = DPARM(1)
         IF (ICODE.EQ.5) ST = ST * ST2
         IW1 = CC - ST/2.0 + 0.9999
         IW2 = CC + ST/2.0 - 0.0001
C                                       Flux-fixed windows
      ELSE
         IC = CC
         IW1 = 0
         DO 130 I = IBCHAN,IC
            J = IC - I + IBCHAN
            IF ((IND1(J).NE.FBLANK) .AND. (IND1(J).LT.DPARM(3))) THEN
               IW1 = J + 1 - DPARM(1)
               GO TO 135
               END IF
 130        CONTINUE
 135     IW2 = IECHAN + 1
         DO 140 I = IC,IECHAN
            IF ((IND1(I).NE.FBLANK) .AND. (IND1(I).LE.DPARM(3))) THEN
               IW2 = I - 1 + DPARM(1)
               GO TO 150
               END IF
 140        CONTINUE
         END IF
C                                       Blank outside windows
 150  IF (IW1.GT.IBCHAN) THEN
         IW1 = IW1 - 1
         DO 155 I = IBCHAN,IW1
            NBL2 = NBL2 + 1
            IF (IND1(I).NE.FBLANK) THEN
               NBL = NBL + 1
               SBL = SBL + IND1(I)
               OUTD1(I-IBCHAN+1) = FBLANK
               END IF
 155        CONTINUE
         END IF
      IF (IW2.LT.IECHAN) THEN
         IW2 = IW2 + 1
         DO 165 I = IW2,IECHAN
            NBL2 = NBL2 + 1
            IF (IND1(I).NE.FBLANK) THEN
               NBL = NBL + 1
               SBL = SBL + IND1(I)
               OUTD1(I-IBCHAN+1) = FBLANK
               END IF
 165        CONTINUE
         END IF
      GO TO 900
C                                       Blank entire row
 800  DO 810 I = 1,IC
         NBL2 = NBL2 + 1
         IF (OUTD1(I).NE.FBLANK) THEN
            NBL = NBL + 1
            SBL = SBL + OUTD1(I)
            OUTD1(I) = FBLANK
            END IF
 810     CONTINUE
C                                       Output the average too
 900  OUTD2 = 0.0
      IF ((NBL2.GT.0) .AND. (DPARM(5).LE.0.0)) OUTD2 = FBLANK
      IF (NBL.GT.0) OUTD2 = SBL / NBL
C                                       substitute DPARM(6)
      IF (DPARM(5).GT.0.0) THEN
         IC = IECHAN - IBCHAN + 1
         DO 910 I = 1,IC
            IF (OUTD1(I).EQ.FBLANK) OUTD1(I) = DPARM(6)
 910        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE DOPBLK (IPOS, IND1, OUTD1, IRET)
C-----------------------------------------------------------------------
C   DOPBLK blanks the data based on the radius of the pixel from the
C   reference pixel in arc seconds.
C   Inputs:
C      IPOS(7)   I    BLC (input image) of first value in DATA
C      IND1(*)   R    Input image row
C   Output:
C      OUTD1(*)  R    Output row : input partly blanked
C      IRET      I    Return code   0 => OK
C                               >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IRET
      REAL      IND1(*), OUTD1(*)
C
      INTEGER   LPOS3, CATMP(256), I, IX, IY, IERR
      REAL      DX, DY
      DOUBLE PRECISION SKYPOS(3), RA1, DEC1, DD, RA0, DEC0
      INCLUDE 'BLANK.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      SAVE LPOS3, IX, IY, RA0, DEC0
      DATA LPOS3 /1000000/
C-----------------------------------------------------------------------
      IRET = 0
C                                       init coordinates
      IF (IPOS(3).LT.LPOS3) THEN
         LPOS3 = IPOS(3)
         LOCNUM = 1
         CALL COPY (256, CATBLK, CATMP)
         CALL COPY (256, CATOLD, CATBLK)
         CALL SETLOC (IPOS(3), .FALSE.)
         CALL COPY (256, CATMP, CATBLK)
C                                       coord type
         IX = MOD (CORTYP(LOCNUM)-1, 2) + 1
         IF (CORTYP(LOCNUM).GT.3) IX = IX + 1
         IY = 3 - IX
         IY = IY + (CORTYP(LOCNUM)-1) / 2
C                                       ref pixel
         CALL XYVAL (OLDR(KRCRP), OLDR(KRCRP+1), SKYPOS(1), SKYPOS(2),
     *      SKYPOS(3), IERR)
         RA0 = SKYPOS(IX) * DG2RAD
         DEC0 = SKYPOS(IY) * DG2RAD
C                                       default radius
         IF (DPARM(1).LE.0.0) THEN
            DX = CATBLK(KINAX) - CATR(KRCRP)
            DY = CATR(KRCRP) - 1
            DX = MAX (DX, DY)
            DPARM(1) = ABS (DX * CATR(KRCIC)) * 3600.0
            END IF
         END IF
      DY = IPOS(2)
      DO 100 I = 1,CATBLK(KINAX)
         OUTD1(I) = IND1(I)
         DX = IPOS(1) + I - 1
         CALL XYVAL (DX, DY, SKYPOS(1), SKYPOS(2), SKYPOS(3), IERR)
         IF (IERR.NE.0) THEN
            OUTD1(I) = FBLANK
         ELSE
            RA1 = SKYPOS(IX) * DG2RAD
            DEC1 = SKYPOS(IY) * DG2RAD
            DD = SIN(DEC1) * SIN(DEC0) + COS(DEC1) * COS(DEC0) *
     *         COS(RA1-RA0)
            DD = MAX (-1.0D0, MIN (1.0D0, DD))
            DD = ACOS (DD) * RAD2AS
            IF (DD.GT.DPARM(1)) OUTD1(I) = FBLANK
            END IF
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE BLNKTV (IRET)
C-----------------------------------------------------------------------
C   BLNKTV sends image one row at a time to the blanking routine and
C   then writes the modified data.  For the TV opcode only.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER    OFF*4, ON*4, IFILE*48
      INTEGER   IRET, NYI, NXI, WINI(4), NXO, NYO, WINO(4), BOI, I, J,
     *   BOO, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2, K, I3, I4, I5,
     *   I6, I7, IPOS(7), BOTEMP, KOFF, NPOLY, IG, LZOOM(3), LIMO, IERR,
     *   LIMIT, IBIND1, OBIND1, OBIND2, LIM1, NVERT(40), NF, NXF, ICHAN,
     *   WINO2(4), NYF, NXT, NYT, CORNOU(8), LINC(2), WINT(4,64),
     *   WINX(4), IROUND, TVPLAN, XV(400), YV(400), ZOR, ITEMP
      REAL      PLTODO, PLDONE, XP, YP, OUTMAX, OUTMIN, PMAX, PMIN
      LOGICAL   T, F, BLNKD, TBLNKD
      INCLUDE 'BLANK.INC'
      INTEGER   BUFF1(MABFSS)
      EQUIVALENCE (XBUFF1, BUFF1)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      DATA OFF, ON /'OFFF', 'ONNN'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open TV
      CALL TVOPEN (BUFF1, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Open terminal
      CALL ZOPEN (TTYLUN, TTYIND, 1, MSGTXT, F, T, T, IRET)
      IF (IRET.NE.0) THEN
         TTYIND = 0
         WRITE (MSGTXT,1000) IRET
         GO TO 980
         END IF
C                                       move CATBLK to CATCLP to save
C                                       get CATOLD into /MAPHDR/
      CALL COPY (256, CATBLK, CATCLP)
      CALL COPY (256, CATOLD, CATBLK)
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI1, INDI1, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 980
         END IF
C                                       output blanked
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO1, INDO1, DISKO, IFILE, T, T, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1015) IRET
         GO TO 980
         END IF
C                                       For residual map:
      IF (DOCAT.GT.0.) THEN
         CALL ZPHFIL ('MA', FVOL(4), FCNO(4), 1, IFILE, IRET)
         CALL ZOPEN (LUNO2, INDO2, FVOL(4), IFILE, T, T, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 980
            END IF
         END IF
C                                       Setup for I/O
C                                       remember names switched
      NXI = CATOLD(KINAX)
      NYI = CATOLD(KINAX+1)
      NXO = CATCLP(KINAX)
      NYO = CATCLP(KINAX+1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
      WINO2(1) = 1
      WINO2(2) = 1
      WINO2(3) = CATNEW(KINAX)
      WINO2(4) = CATNEW(KINAX+1)
C                                       TV windows
      LINC(1) = TXINC + 0.01
      LINC(2) = TYINC + 0.01
      NXT = (ECHAN - BCHAN) / TXINC + 1.0001
      NYT = (TRC(2) - BLC(2)) / TYINC + 1.0001
      NXF = (NXT - 1) / MAXXTV(1) + 1
      NYF = (NYT - 1) / MAXXTV(2) + 1
      NF = NXF * NYF
      XP = 0.0
      IF (NXF.GT.1) XP = ((NXT-1)*TXINC + 1 - TXINC*MAXXTV(1)) /
     *   (NXF - 1.0)
      YP = 0.0
      IF (NYF.GT.1) YP = ((NYT-1)*TYINC + 1 - TYINC*MAXXTV(2)) /
     *   (NYF - 1.0)
      K = 0
      DO 35 I = 1,NYF
         DO 30 J = 1,NXF
            K = K + 1
            WINT(1,K) = BLC(1) + BCHAN - 0.99 + XP * (J-1)
            WINT(2,K) = BLC(2) + YP * (I-1) + 0.01
            IF (NXF.GT.1) WINT(3,K) = WINT(1,K) + MAXXTV(1)*LINC(1) - 1
            IF (NXF.EQ.1) WINT(3,K) = WINT(1,K) + ECHAN - BCHAN + 0.01
            IF (NYF.GT.1) WINT(4,K) = WINT(2,K) + MAXXTV(2)*LINC(2) - 1
            IF (NYF.EQ.1) WINT(4,K) = TRC(2) + 0.01
 30         CONTINUE
 35      CONTINUE
      WINX(1) = (MAXXTV(1) - (WINT(3,1)-WINT(1,1)) / LINC(1) + 1) / 2
      WINX(2) = (MAXXTV(2) - (WINT(4,1)-WINT(2,1)) / LINC(2) + 1) / 2
      WINX(3) = WINX(1) + (WINT(3,1) - WINT(1,1)) / LINC(1)
      WINX(4) = WINX(2) + (WINT(4,1) - WINT(2,1)) / LINC(2)
C                                       other TV parms
      ICHAN = TVCHAN + 0.01
      IF ((ICHAN.LT.1) .OR. (ICHAN.GT.NGRAY)) ICHAN = 1
      TVPLAN = 2 ** (ICHAN-1)
      IG = MIN (3, NGRAPH)
      CALL RNGSET (RANGE, OLDR(KRDMX), OLDR(KRDMN), CATR(IRRAN))
      CALL CHR2H (2, TVFUNC(1:2), 1, CATH(IITRA))
      CATBLK(IICNO) = FCNO(1)
      CATBLK(IIVOL) = FVOL(1)
C                                       Init the TV
      I = IG + NGRAY
      CALL MOVIST (OFF, TVPLAN, 0, 0, 0, IERR)
      CALL YCINIT (ICHAN, BUFF1)
      CALL YCINIT (I, BUFF1)
      CALL YZERO (ICHAN, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL YZERO (I, IERR)
      IF (IERR.NE.0) GO TO 900
      J = NGRAY + NGRAPH
      DO 40 K = 1,J
         CALL YSLECT (OFF, K, 0, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 900
 40      CONTINUE
      CALL YSLECT (ON, I, 0, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL YSLECT (ON, ICHAN, 0, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 900
      ITEMP = 2 ** NGRAY
      I = ZOR (ITEMP, TVPLAN)
      CALL YSCROL (I, 0, 0, F, IERR)
      IF (IERR.NE.0) GO TO 900
      TVZOOM(1) = 0
      TVZOOM(2) = MAXXTV(1) / 2
      TVZOOM(3) = MAXXTV(2) / 2
      CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), F, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL COPY (3, TVZOOM, LZOOM)
C                                       Other IO initializations
      OUTMAX = -1.0E20
      OUTMIN = -OUTMAX
      PMAX = OUTMAX
      PMIN = OUTMIN
      BLNKD = F
      TBLNKD = .FALSE.
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
      PLTODO = PLTODO * CATCLP(KINAX+2)
      KOFF = 0
      LIMO = CATCLP(KINAX) - 1
      CORNOU(8) = 1
C                                       Loop
      DO 700 I7 = 1,LIM7
         CORNOU(7) = I7
         IPOS(7) = BLC(7) + I7 - 0.9
         DO 600 I6 = 1,LIM6
            CORNOU(6) = I6
            IPOS(6) = BLC(6) + I6 - 0.9
            DO 500 I5 = 1,LIM5
               CORNOU(5) = I5
               IPOS(5) = BLC(5) + I5 - 0.9
               DO 400 I4 = 1,LIM4
                  CORNOU(4) = I4
                  IPOS(4) = BLC(4) + I4 - 0.9
C                                       Init. file: output average
                  IF (DOCAT.GT.0.0) THEN
                     CALL COMOFF (CATNEW(KIDIM), CATNEW(KINAX),
     *                  CORNOU(4), BOTEMP, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1060) IRET
                        GO TO 980
                        END IF
                     BOI = BOTEMP + 1
                     CALL MINIT ('WRIT', LUNO2, INDO2, WINO2(3),
     *                  WINO2(4), WINO2, XBUFF3, JBUFSZ, BOI, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1065) IRET
                        GO TO 980
                        END IF
                     END IF
                  DO 300 I3 = 1,LIM3
                     CORNOU(3) = I3
                     IPOS(3) = BLC(3) + I3 - 0.9
                     PLDONE = PLDONE + 1
                     WRITE (MSGTXT,1070) PLDONE, PLTODO
                     CALL MSGWRT (1)
C                                       Find polygons
         NPOLY = 0
         CALL FILL (40, 0, NVERT)
         CALL COPY (5, IPOS(3), CATBLK(IIDEP))
         DO 80 K = 1,NF
            WRITE (MSGTXT,1071) WINT(1,K), WINT(3,K), WINT(2,K),
     *         WINT(4,K)
            IF (NF.GT.1) CALL MSGWRT (1)
            CALL TVLOAD (LUNI1, INDI1, ICHAN, LINC, WINX, WINT(1,K),
     *         JBUFSZ, XBUFF1, IRET)
            IF (IRET.EQ.0) GO TO 75
               WRITE (MSGTXT,1070) IRET
               GO TO 980
 75         CALL BLKTVF (IG, TVPLAN, LINC, TTYLUN, TTYIND, LZOOM, NPOLY,
     *         NVERT, XV, YV, IBUFF1, IRET)
            IF (IRET.NE.0) GO TO 990
 80         CONTINUE
C                                       Init. files, first input.
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3), BOTEMP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET
            GO TO 980
            END IF
         BOI = BOTEMP + 1
         CALL MINIT ('READ', LUNI1, INDI1, NXI, NYI, WINI, XBUFF1,
     *      JBUFSZ, BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 980
            END IF
C                                       Init blanked file.
         CALL COMOFF (CATCLP(KIDIM), CATCLP(KINAX), CORNOU(3), BOTEMP,
     *      IRET)
         BOO = BOTEMP + 1
         CALL MINIT ('WRIT', LUNO1, INDO1, NXO, NYO, WINO, XBUFF2,
     *      JBUFSZ, BOO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1105) IRET
            GO TO 980
            END IF
C                                       "write" output avg
         OBIND2 = 1
         IF (DOCAT.GT.0.0) THEN
            CALL MDISK ('WRIT', LUNO2, INDO2, XBUFF3, OBIND2, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1145) IRET
               GO TO 980
               END IF
            END IF
         DO 250 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.9
            IPOS(1) = IROUND (BLC(1))
C                                       Read.
            CALL MDISK ('READ', LUNI1, INDI1, XBUFF1, IBIND1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1150) IRET
               GO TO 980
               END IF
            DO 170 I1 = 1,LIM1
               XBUFF4(I1) = XBUFF1(IBIND1+I1-1)
 170           CONTINUE
C                                       Write.
            CALL MDISK ('WRIT', LUNO1, INDO1, XBUFF2, OBIND1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1175) IRET
               GO TO 980
               END IF
C                                       Call DO1TVB
            CALL DO1TVB (IPOS, NPOLY, NVERT, XV, YV, XBUFF4,
     *         XBUFF2(OBIND1), XBUFF3(OBIND2), IRET)
            IF (DOCAT.GT.0.0) OBIND2 = OBIND2 + 1
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1210) IRET
               GO TO 980
               END IF
C                                       Check max, min, blanking.
            LIMIT = OBIND1 + LIMO
            DO 220 I1 = OBIND1,LIMIT
               BLNKD = BLNKD .OR. (XBUFF2(I1).EQ.FBLANK)
               IF (XBUFF2(I1).NE.FBLANK) THEN
                  OUTMAX = MAX (OUTMAX, XBUFF2(I1))
                  OUTMIN = MIN (OUTMIN, XBUFF2(I1))
                  END IF
 220           CONTINUE
            IF (DOCAT.GT.0.0) THEN
               I1 = OBIND2 - 1
               TBLNKD = (TBLNKD) .OR. (XBUFF3(I1).EQ.FBLANK)
               IF (XBUFF3(I1).NE.FBLANK) THEN
                  PMAX = MAX (PMAX, XBUFF3(I1))
                  PMIN = MIN (PMIN, XBUFF3(I1))
                  END IF
               END IF
 250        CONTINUE
C                                       Flush buffers for blanked
         CALL MDISK ('FINI', LUNO1, INDO1, XBUFF2, OBIND1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1175) IRET
            GO TO 980
            END IF
 300     CONTINUE
C                                       Flush buffers for avg.
                  IF (DOCAT.GT.0.0) THEN
                     CALL MDISK ('FINI', LUNO2, INDO2, XBUFF3, OBIND2,
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1145) IRET
                        GO TO 980
                        END IF
                     END IF
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       restore CATBLK
      CALL COPY (256, CATCLP, CATBLK)
C                                       Update CATBLK in core only
      CATR(KRDMX) = OUTMAX
      CATR(KRDMN) = OUTMIN
      NEWR(KRDMX) = PMAX
      NEWR(KRDMN) = PMIN
C                                       Mark blanking in CATBLK.
      CATR(KRBLK) = 0.0
      IF (BLNKD) CATR(KRBLK) = FBLANK
      NEWR(KRBLK) = 0.0
      IF (TBLNKD) NEWR(KRBLK) = FBLANK
C                                       Close files
      CALL ZCLOSE (LUNI1, INDI1, IRET)
      IRET = 0
      GO TO 990
C                                       TV error
 900  IRET = 8
      WRITE (MSGTXT,1900) IERR
C                                       Error
 980  CALL MSGWRT (8)
C                                       close downs
 990  IF (TTYIND.GT.0) CALL ZCLOSE (TTYLUN, TTYIND, IERR)
      CALL TVCLOS (BUFF1, IERR)
C                                       resume AIPS on error
 995  IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLNKTV: ERROR',I3,' OPENING USER TERMINAL')
 1010 FORMAT ('BLNKTV: ERROR',I3,' OPENING INPUT FILE')
 1015 FORMAT ('BLNKTV: ERROR',I3,' OPENING AVERAGE BLANKED FILE')
 1020 FORMAT ('BLNKTV: ERROR',I5,' OPENING OUTPUT FILE')
 1060 FORMAT ('BLNKTV: COMOFF (AVG OUT) ERROR',I3)
 1065 FORMAT ('BLNKTV: MINIT (AVG OUT) ERROR',I3)
 1070 FORMAT ('BEGIN PLANE',F8.0,' OF',F8.0)
 1071 FORMAT ('BEGIN SUBIMAGE XPIX =',I5,' -',I5,'  YPIX =',I5,' -',I5)
 1080 FORMAT ('BLNKTV: COMOFF (INPUT) ERROR',I3)
 1100 FORMAT ('BLNKTV: MINIT (INPUT) ERROR',I3)
 1105 FORMAT ('BLNKTV: MINIT (OUTPUT) ERROR',I3)
 1145 FORMAT ('BLNKTV: WRITE (AVG OUT) ERROR',I3)
 1150 FORMAT ('BLNKTV: READ (INPUT) ERROR',I3)
 1175 FORMAT ('BLNKTV: WRITE (OUTPUT) ERROR',I3)
 1210 FORMAT ('BLNKTV: DO1BLK ERROR',I3)
 1900 FORMAT ('BLNKTV: TV ERROR CODE',I7)
      END
      SUBROUTINE BLKTVF (IGR, ICHAN, LINC, TTYLUN, TTYIND, LZOOM, NPY,
     *   NV, XV, YV, SCRTCH, IRET)
C-----------------------------------------------------------------------
C   BLKTVF does the finding of the desired polygons
C   Inputs: IGR     I          Graphics plane to use: 1 - NGRAPH
C           ICHAN   I          Image plane (bit mask)
C           LINC    I(2)       TV load pixel increments (x,y)
C           TTYLUN  I          LUN of terminal opened for user comm.
C           TTYIND  I          FTAB pointer. for terminal
C   In/out: LZOOM   I(3)       Previous zoom parameters
C           NPY     I          Number of polygons
C           NV      I(30)      Number of vertices in each polygon
C           XV      I(400)     X pixel position of each vertex wrt the
C                              original image
C           YV      I(400)     Y pixel pos of vertices wrt input image
C   Output: SCRTCH  I(>520)    scrtach buffer
C           IRET    I          error code 0 ok, 1 TV, 2 TTY, 99 user
C-----------------------------------------------------------------------
      CHARACTER TEMP*4, ON*4, OFF*4, REDO*4, QUIT*4, LINE*80
      INTEGER   IGR, ICHAN, LINC(2), TTYLUN, TTYIND, LZOOM(3), NPY,
     *   NV(30), XV(400), YV(400), SCRTCH(*), IRET
      INTEGER   IB, IE, IE0, NPY0, I, J, CATBLK(256), IERR, JERR, IBO,
     *   ITW(3), QUAD, IBUT, ICH, LB, LE, IX(3), IY(3)
      REAL      PPOS(2), RPOS(2), DLIM
      LOGICAL   T, F, DOIT
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /MAPHDR/ CATBLK
      DATA T, F /.TRUE.,.FALSE./
      DATA ON, OFF, REDO, QUIT /'ONNN','OFFF','REDO','QUIT'/
C-----------------------------------------------------------------------
      IRET = 1
      IE0 = 0
      NPY0 = NPY + 1
      IE = 0
C                                       Correct old ones to this subimg
      IF (NPY.LE.0) GO TO 15
         DO 10 I = 1,NPY
            IB = IE + 1
            IE = IB + NV(I) - 1
            DO 10 J = IB,IE
               XV(J) = (XV(J)-CATBLK(IIWIN)) / LINC(1) + CATBLK(IICOR)
               YV(J) = (YV(J)-CATBLK(IIWIN+1)) / LINC(2) +
     *            CATBLK(IICOR+1)
 10         CONTINUE
         IE0 = IE
C                                       clear graphics
 15   ICH = IGR + NGRAY
      CALL YZERO (ICH, IERR)
      IF (IERR.NE.0) GO TO 900
      NPY = NPY0 - 1
      IE = IE0
C                                       interactive zoom, enhance
      CALL YWINDO ('READ', WINDTV, IERR)
      IF (IERR.NE.0) GO TO 900
      I = LUTOUT + 1
      CALL COPY (3, LZOOM, TVZOOM)
      CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), F, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL TVFIDL (ICHAN, I, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL COPY (3, TVZOOM, LZOOM)
C                                       fill current polygons
      IF (NPY.GT.0) CALL BLTFIL (NPY, NV, XV, YV, IGR, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL ZTIME (ITW)
C                                       Start new polygon
 40   IF (NPY.GE.30) GO TO 800
      NPY = NPY + 1
      IB = IE + 1
      IE = IB
      NV(NPY) = 0
      WRITE (MSGTXT,1040) NPY
      CALL MSGWRT (1)
      WRITE (MSGTXT,1041)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1042)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1043)
      CALL MSGWRT (1)
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
C                                       No scroll correction
      QUAD = -1
C                                       ON cursor at desired position
      CALL YCURSE (ON, F, T, RPOS, QUAD, IBUT, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 900
      IF (IERR.EQ.2) CALL YCURSE (ON, F, F, RPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Cursor read loop
 50   CALL YCURSE ('READ', T, T, RPOS, QUAD, IBUT, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF (.NOT.DOIT) GO TO 50
C                                       No button -> no action
         IF (IBUT.EQ.0) GO TO 50
C                                       Time to quit marking
         IF ((IBUT.LT.4) .OR. (IB.LT.IE)) GO TO 60
            NPY = NPY - 1
            GO TO 200
C                                       Mark new vertex
 60      NV(NPY) = NV(NPY) + 1
         XV(IE) = RPOS(1) + 0.01
         YV(IE) = RPOS(2) + 0.01
         IF (IB.EQ.IE) XV(IE+1) = XV(IE)
         IF (IB.EQ.IE) YV(IE+1) = YV(IE)
         I = MAX (IB, IE-1)
         IE = IE + 1
C                                       draw line
         CALL IMVECT (ON, ICH, 2, XV(I), YV(I), SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       done with polygon
         IF ((IBUT.LT.2) .AND. (IE.LT.400)) GO TO 50
            NV(NPY) = NV(NPY) + 1
            XV(IE) = XV(IB)
            YV(IE) = YV(IB)
C                                       draw line
            CALL IMVECT (ON, ICH, 2, XV(IE-1), YV(IE-1), SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
C                                       Respond further to buttons
         IF (IE.GE.400) GO TO 810
         IF (IBUT.LT.4) GO TO 40
C                                       Vertex correction area
         IF (IBUT.GE.8) GO TO 200
C                                       some already set box
C                                       wait for indication which
 100  WRITE (MSGTXT,1100)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1101)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1102)
      CALL MSGWRT (1)
      CALL YCURSE ('READ', T, T, RPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       check button
      IF (IBUT.GE.8) GO TO 200
C                                       redraw polygons to be safe
 105  IF (IBUT.LT.4) GO TO 120
         LB = 1 + IE0
         DO 110 I = NPY0,NPY
            CALL IMVECT (ON, ICH, NV(I), XV(LB), YV(LB), SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            LB = LB + NV(I)
 110        CONTINUE
         GO TO 40
C                                       Find nearest corner
 120  DLIM = 0.5
 125  LB = 1 + IE0
      DO 135 I = NPY0,NPY
         LE = LB + NV(I) - 1
         DO 130 J = LB,LE
            IF ((ABS(RPOS(1)-XV(J)).LE.DLIM) .AND. (ABS(RPOS(2)
     *         -YV(J)).LE.DLIM)) GO TO 140
 130        CONTINUE
         LB = LE + 1
 135     CONTINUE
      DLIM = DLIM + 1.5
      IF (DLIM.LE.4.0) GO TO 125
      GO TO 100
C                                       Got one
 140  IF (J.EQ.LE) J = LB
      IBO = J
      RPOS(1) = XV(IBO)
      RPOS(2) = YV(IBO)
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      IX(2) = XV(IBO)
      IX(1) = XV(IBO-1)
      IX(3) = XV(IBO+1)
      IF (IBO.EQ.LB) IX(1) = XV(LE-1)
      IY(2) = YV(IBO)
      IY(1) = YV(IBO-1)
      IY(3) = YV(IBO+1)
      IF (IBO.EQ.LB) IY(1) = YV(LE-1)
C                                       Cursor read loop
 150  CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF (.NOT.DOIT) GO TO 150
C                                       draw new lines
         CALL IMVECT (OFF, ICH, 3, IX, IY, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         IX(2) = RPOS(1) + 0.5
         IY(2) = RPOS(2) + 0.5
         CALL IMVECT (ON, ICH, 3, IX, IY, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       mark this one
         IF (IBUT.EQ.0) GO TO 150
            XV(IBO) = IX(2)
            YV(IBO) = IY(2)
            IF (IBUT.LT.4) GO TO 100
            IF (IBUT.LT.8) GO TO 105
C                                       DONE: off zoom
 200  TVZOOM(1) = 0
      TVZOOM(2) = MAXXTV(1) / 2
      TVZOOM(3) = MAXXTV(2) / 2
      CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), F, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       fill current polygons
      IF (NPY.GT.0) CALL BLTFIL (NPY, NV, XV, YV, IGR, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       talk to user
      IRET = 2
      WRITE (LINE,1200)
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, LINE, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, LINE, IERR)
      IF (IERR.NE.0) GO TO 900
      READ (LINE,1201) TEMP
      CALL CHLTOU (4, TEMP)
      IRET = 1
      IF (TEMP.EQ.REDO) GO TO 15
      IRET = 0
      IF (TEMP.EQ.QUIT) IRET = 99
C                                       Correct to image pixels
      IF (NPY.LE.0) GO TO 900
         IE = 0
         DO 210 I = 1,NPY
            IB = IE + 1
            IE = IB + NV(I) - 1
            DO 210 J = IB,IE
               XV(J) = (XV(J)-CATBLK(IICOR)) * LINC(1) + CATBLK(IIWIN)
               YV(J) = (YV(J)-CATBLK(IICOR+1)) * LINC(2) +
     *            CATBLK(IIWIN+1)
 210        CONTINUE
         GO TO 900
C                                       Overflow problems
 800  WRITE (MSGTXT,1800)
      CALL MSGWRT (7)
      GO TO 900
 810  WRITE (MSGTXT,1810)
      CALL MSGWRT (7)
      GO TO 200
C                                       close down
 900  CALL YCURSE (OFF, F, T, RPOS, QUAD, IBUT, JERR)
      IF (IRET.EQ.0) GO TO 999
         IF (IRET.EQ.1) WRITE (MSGTXT,1900) IERR
         IF (IRET.EQ.2) WRITE (MSGTXT,1901) IERR
         IF (IRET.EQ.99) WRITE (MSGTXT,1902)
         CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('Begin setting region number',I3)
 1041 FORMAT ('Press button A to set intermediate vertex')
 1042 FORMAT ('Press buttons B, C, or D to set final vertex')
 1043 FORMAT ('C => then reset a vertex,  D => then exit')
 1100 FORMAT ('*************  move cursor to vertex to be reset')
 1101 FORMAT ('Then push button A or B to do resetting -- or')
 1102 FORMAT ('Push C to go to next region  or D to exit')
 1200 FORMAT ('Type REDO to do this image over, QUIT to stop,',
     *   ' hit return to go on')
 1201 FORMAT (A4)
 1800 FORMAT ('REACHED LIMIT OF 30 POLYGONS')
 1810 FORMAT ('REACHED LIMIT OF 400 VERTICES')
 1900 FORMAT ('TV ROUTINE RETURNS ERROR CODE',I7)
 1901 FORMAT ('TERMINAL IO ERROR CODE',I7)
 1902 FORMAT ('Terminating AT USER''S REQUEST')
      END
      SUBROUTINE DO1TVB (IPOS, NPOLY, NVERT, XV, YV, IND1, OUTD1, OUTD2,
     *   IRET)
C-----------------------------------------------------------------------
C   DO1TVB applies the polygon blanking to each row - summing and
C   blanking pixels outside the windows.
C   Inputs:
C      IPOS(7)   I    BLC (input image) of first value in DATA
C      NPOLY     I    Number of polygons
C      NVERT(*)  I    Number of vertices in each polygon
C      XV(*)     I    X pixel coords of vertices (wrt original input)
C      YV(*)     I    Y pixel coords of vertices (wrt original input)
C      IND1(*)   R    Input image row
C   Values from commons:
C      FBLANK    R    Value of blanked pixel.
C      BCHAN, ECHAN  R     Channel (x pixel) range of good data
C      BLC, TRC (7)  R     Window corners in original image
C   Output:
C      OUTD1(*)  R    Output row : input partly blanked
C      OUTD2(1)  R    Average of those pixels blanked
C      IRET      I    Return code   0 => OK
C                               >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), NPOLY, NVERT(*), XV(*), YV(*), IRET
      REAL      IND1(*), OUTD1(*), OUTD2
      INTEGER   I, J, NBL, NBL2, ICX, IBCHAN, IECHAN, IC, IX, LP,
     *   IXL(40), IXU(40), LNX
      REAL      SBL
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'BLANK.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       sum up edges if any
      NBL = 0
      NBL2 = 0
      SBL = 0.0
      IBCHAN = BCHAN - 0.99
      IECHAN = ECHAN + 1.01
      ICX = TRC(1) - BLC(1) + 1.01
      IF (IBCHAN.GE.1) THEN
         DO 10 I = 1,IBCHAN
            NBL2 = NBL2 + 1
            IF (IND1(I).NE.FBLANK) THEN
               NBL = NBL + 1
               SBL = SBL + IND1(I)
               END IF
 10         CONTINUE
         END IF
      IBCHAN = IBCHAN + 1
      IF (IECHAN.LE.ICX) THEN
         DO 20 I = IECHAN,ICX
            NBL2 = NBL2 + 1
            IF (IND1(I).NE.FBLANK) THEN
               NBL = NBL + 1
               SBL = SBL + IND1(I)
               END IF
 20         CONTINUE
         END IF
      IECHAN = IECHAN - 1
C                                       Move input to output row
      IC = IECHAN - IBCHAN + 1
      CALL RCOPY (IC, IND1(IBCHAN), OUTD1)
C                                       Init polygon search
      LNX = 0
      IF (NPOLY.GT.0) CALL BLTLIS (1, 1, NPOLY, NVERT, XV, YV,
     *      IPOS(2), LNX, IXL, IXU)
C                                       blank inside blotch regions
      IF (DOINVR.LE.0.0) GO TO 70
         IF (LNX.LE.0) GO TO 900
            LP = BLC(1) + BCHAN - 1.99
            DO 40 J = 1,IC
               IX = J + LP
               DO 30 I = 1,LNX
                  IF ((IX.GE.IXL(I)) .AND. (IX.LE.IXU(I))) GO TO 35
 30               CONTINUE
               GO TO 40
C                                       blank the 1 pixel inside
 35            NBL2 = NBL2 + 1
               IF (OUTD1(J).EQ.FBLANK) GO TO 40
                  NBL = NBL + 1
                  SBL = SBL + OUTD1(J)
                  OUTD1(J) = FBLANK
 40            CONTINUE
            GO TO 900
C                                       blank outside blotch regions
 70   CONTINUE
         IF (LNX.LE.0) GO TO 85
            LP = BLC(1) + BCHAN - 1.99
            DO 80 J = 1,IC
               IX = J + LP
               DO 75 I = 1,LNX
                  IF ((IX.GE.IXL(I)) .AND. (IX.LE.IXU(I))) GO TO 80
 75               CONTINUE
C                                       blank the 1 pixel (outside)
               NBL2 = NBL2 + 1
               IF (OUTD1(J).EQ.FBLANK) GO TO 80
                  NBL = NBL + 1
                  SBL = SBL + OUTD1(J)
                  OUTD1(J) = FBLANK
 80            CONTINUE
            GO TO 900
C                                       Blank whole row
 85      CONTINUE
            DO 90 J = 1,IC
               NBL2 = NBL2 + 1
               IF (OUTD1(J).EQ.FBLANK) GO TO 90
                  NBL = NBL + 1
                  SBL = SBL + OUTD1(J)
                  OUTD1(J) = FBLANK
 90            CONTINUE
C                                       Output the average too
 900  OUTD2 = 0.0
      IF ((NBL2.GT.0) .AND. (DPARM(5).LE.0.0)) OUTD2 = FBLANK
      IF (NBL.GT.0) OUTD2 = SBL / NBL
C                                       substitute DPARM(6)
      IF (DPARM(5).LE.0.0) GO TO 999
         DO 910 I = 1,IC
            IF (OUTD1(I).EQ.FBLANK) OUTD1(I) = DPARM(6)
 910        CONTINUE
C
 999  RETURN
      END
      SUBROUTINE BLNKOU (IRET)
C-----------------------------------------------------------------------
C   BLNKOU completes the writing of the residual map (if any) and then
C   creates and fills (via PSCALE) the individual Gaussian parameters.
C   It calls BLNKHI for history info for all images.
C   Output: IRET    I      0 => ok,  4 => real trouble.
C-----------------------------------------------------------------------
      INTEGER   IRET
      INTEGER   IERR
      INCLUDE 'BLANK.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Blanked (normal) output
      WRITE (MSGTXT,1000)
      CALL MSGWRT (1)
      CALL ZCLOSE (LUNO1, INDO1, IERR)
C                                       Blanked history
      CALL BLNKHI (0, 3)
C                                       Avg of blanked pixels image
      IF (DOCAT.GT.0.0) THEN
         WRITE (MSGTXT,1050)
         CALL MSGWRT (1)
         NEWCNO = FCNO(4)
         DISKO = FVOL(4)
         CALL COPY (256, CATNEW, CATBLK)
         SEQOUT = CATBLK(KIIMS)
         CALL H2CHR (6, KHIMCO, CATR(KHIMC), CLAOUT)
         CALL ZCLOSE (LUNO2, INDO2, IERR)
C                                       Avg history
         CALL BLNKHI (1, 4)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Begin final output stage for blanked image')
 1050 FORMAT ('Begin final output stage for image of average of',
     *   ' blanked pixels')
      END
      SUBROUTINE BLNKHI (ITYP, NCN)
C-----------------------------------------------------------------------
C   BLNKHI copies and updates history file.
C   Inputs: ITYP   I      Output map type: 0 => first axis intact
C                         >0 => answers (get 1st axis info also)
C           NCN    I      Position in FILES common on catlgd file
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, OPEXP(7)*16, LABEL*8, NOTTYP*2
      INTEGER   ITYP, NCN, LUN1, LUN2, IERR, BUFF1(256)
      LOGICAL   T
      INCLUDE 'BLANK.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'  '/
      DATA OPEXP /'IMAGE 2 CLIP    ',
     *   'SELF CLIP       ', 'FIXED WINDOW    ',
     *   'FLOATING WINDOW ', 'MOMENT WINDOW   ',
     *   'FLUX WINDOW     ', 'TV CURSOR SET   '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   BUFF1, SBUFF, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 50
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, SBUFF,
     *   IERR)
      IF (IERR.NE.0) GO TO 50
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   SBUFF, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       BLC
      WRITE (HILINE,2000) TSKNAM, BLC
      CALL HIADD (LUN2, HILINE, SBUFF, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       TRC
      WRITE (HILINE,2001) TSKNAM, TRC
      CALL HIADD (LUN2, HILINE, SBUFF, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       other Parms
      WRITE (HILINE,2002) TSKNAM, BCHAN, ECHAN
      IF ((BCHAN.GE.2.) .OR. (ECHAN.LE.TRC(1)-BLC(1))) THEN
         CALL HIADD (LUN2, HILINE, SBUFF, IERR)
         END IF
      IF (IERR.NE.0) GO TO 50
      WRITE (HILINE,2003) TSKNAM, OPCODE, OPEXP(ICODE)
      CALL HIADD (LUN2, HILINE, SBUFF, IERR)
      IF (IERR.NE.0) GO TO 50
      IF (ICODE.EQ.1) CALL HENCO2 (TSKNAM, NAM2IN, CLA2IN, SEQ2IN,
     *   DIS2IN, LUN2, SBUFF, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       parameters
      IF ((ICODE.NE.3) .AND. (ICODE.LT.7)) THEN
         WRITE (HILINE,2004) TSKNAM, DPARM(1)
         IF (ICODE.EQ.4) WRITE (HILINE,2005) TSKNAM, DPARM(1)
         IF (ICODE.EQ.5) WRITE (HILINE,2006) TSKNAM, DPARM(1)
         IF (DPARM(1).GT.0.0) THEN
            CALL HIADD (LUN2, HILINE, SBUFF, IERR)
            END IF
         IF (IERR.NE.0) GO TO 50
         CALL H2CHR (8, 1, OLDH(KHBUN), LABEL)
         IF (ICODE.GT.2) THEN
            WRITE (HILINE,2007) TSKNAM, DPARM(2)
            IF (DPARM(2).GT.0.0) WRITE (HILINE,2008) TSKNAM,
     *         DPARM(2)
            CALL HIADD (LUN2, HILINE, SBUFF, IERR)
            IF (IERR.NE.0) GO TO 50
            WRITE (HILINE,2009) TSKNAM, DPARM(3), LABEL
            CALL HIADD (LUN2, HILINE, SBUFF, IERR)
            IF (IERR.NE.0) GO TO 50
            IF (ICODE.EQ.6) THEN
               WRITE (HILINE,2010) TSKNAM
               CALL HIADD (LUN2, HILINE, SBUFF, IERR)
               IF (IERR.NE.0) GO TO 50
               END IF
            GO TO 30
            END IF
C                                       clipping ones
         WRITE (HILINE,2015) TSKNAM, DPARM(3), LABEL
         CALL HIADD (LUN2, HILINE, SBUFF, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2016) TSKNAM, DPARM(4), LABEL
         CALL HIADD (LUN2, HILINE, SBUFF, IERR)
         IF (IERR.NE.0) GO TO 50
C                                       explain F in messages
         WRITE (HILINE,2020) TSKNAM
         IF (ICODE.EQ.2) WRITE (HILINE,2021) TSKNAM
         CALL HIADD (LUN2, HILINE, SBUFF, IERR)
         IF (IERR.NE.0) GO TO 50
         END IF
C                                       TVCU: inverse or no
      IF (ICODE.EQ.7) THEN
         IF (DOINVR.GT.0.0) WRITE (HILINE,2025) TSKNAM
         IF (DOINVR.LE.0.0) WRITE (HILINE,2026) TSKNAM
         CALL HIADD (LUN2, HILINE, SBUFF, IERR)
         IF (IERR.NE.0) GO TO 50
      ELSE IF (ICODE.EQ.8) THEN
         WRITE (HILINE,2027) TSKNAM, DPARM(1)
         CALL HIADD (LUN2, HILINE, SBUFF, IERR)
         IF (IERR.NE.0) GO TO 50
         END IF
C                                       blanking
 30   IF (DPARM(5).GT.0.0) THEN
         WRITE (HILINE,2030) TSKNAM
      ELSE
         WRITE (HILINE,2031) TSKNAM
         END IF
      CALL HIADD (LUN2, HILINE, SBUFF, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Old axis 1
      IF (ITYP.GT.0) THEN
         CALL H2CHR (8, 1, OLDH(KHCTP), LABEL)
         WRITE (HILINE,2040) TSKNAM, LABEL
         CALL HIADD (LUN2, HILINE, SBUFF, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2041) TSKNAM, CATOLD(KINAX)
         CALL HIADD (LUN2, HILINE, SBUFF, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2042) TSKNAM, OLDR(KRCRP)
         CALL HIADD (LUN2, HILINE, SBUFF, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2043) TSKNAM, OLDR(KRCIC)
         CALL HIADD (LUN2, HILINE, SBUFF, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2044) TSKNAM, OLDD(KDCRV)
         CALL HIADD (LUN2, HILINE, SBUFF, IERR)
         END IF
C                                       Close HI file
 50   CALL HICLOS (LUN2, T, SBUFF, IERR)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, BUFF1, SBUFF, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK and close
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'CLWR', BUFF1, IERR)
      FRW(NCN) = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLNKHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,'TRC =',6(F6.0,','),F6.0)
 2002 FORMAT (A6,'BCHAN =',F5.0,'  ECHAN =',F5.0,6X,'/ signal ',
     *   'channels in window')
 2003 FORMAT (A6,'OPCODE = ''',A4,'''',17X,'/ operation ',A)
 2004 FORMAT (A6,'DPARM(1) =',F6.0,16X,'/ Extend signal area by ',
     *   'channels')
 2005 FORMAT (A6,'DPARM(1) =',F6.0,16X,'/ Window width in channels')
 2006 FORMAT (A6,'DPARM(1) =',F6.2,16X,'/ Window width = 2nd mom * ',
     *   'D(1)')
 2007 FORMAT (A6,'DPARM(2) =',F6.1,16X,'/ Window center at 1st',
     *   ' moment')
 2008 FORMAT (A6,'DPARM(2) =',F6.1,16X,'/ Window center at peak val')
 2009 FORMAT (A6,'DPARM(3) =',1PE13.5,9X,'/ Moms ignore < D(3) ',A8)
 2010 FORMAT (A6,'/ Also stop window at first pixel < D(3)')
 2015 FORMAT (A6,'DPARM(3) =',1PE13.5,9X,'/ Keep F <= D(3) in ',A8)
 2016 FORMAT (A6,'DPARM(4) =',1PE13.5,9X,'/ Keep F >= D(4) in ',A8)
 2020 FORMAT (A6,'/ Where F is from the second image')
 2021 FORMAT (A6,'/ Where F is from the input image itself')
 2025 FORMAT (A6,'/ Blank pixels inside blotch regions')
 2026 FORMAT (A6,'/ Blank pixels outside blotch regions')
 2027 FORMAT (A6,'/ Blank pixels outside',F9.3,' asec')
 2030 FORMAT (A6,'/ Blanked pixels set to 0.0 - as if they are valid')
 2031 FORMAT (A6,'/ Blanked pixels set to magic value blanking')
 2040 FORMAT (A6,'CTYPE1  = ''',A8,'''',12X,'/ Old axis 1')
 2041 FORMAT (A6,'NAXIS1  = ',I6,16X,'/ Old axis 1')
 2042 FORMAT (A6,'CRPIX1  = ',F9.3,13X,'/ Old axis 1')
 2043 FORMAT (A6,'CDELT1  = ',1PE13.5,9X,'/ Old axis 1')
 2044 FORMAT (A6,'CRVAL1  = ',1PE18.10,4X,'/ Old axis 1')
      END
