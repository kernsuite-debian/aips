LOCAL INCLUDE 'BLSUM.INC'
C                                                          Include BLSUM
      INCLUDE 'INCS:PMAD.INC'
C                                       Local include for BLSUM
      REAL      XSEQIN, XDISKI, XSEQ2N, XDIS2I, BLC(7), TRC(7), BCHAN,
     *   DOALIN, DOINVR, PRTLEV, DOCRT, DOSLIC, TVCHAN, RANGE(2), TXINC,
     *   TYINC, XNPLOT, FACTOR, XSYMB, XLTYPE, XYRATO, APARM(10),
     *   BADD(10)
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAM2I(3), XCLA2I(2), XTVFUN,
     *   XLPNAM(12), XOUFIL(12), XOPTYP
      CHARACTER NAMEIN*12, CLAIN*6, NAM2IN*12, CLA2IN*6, TVFUNC*4,
     *   LPNAME*48, OUTFIL*48, AUNITS(2)*8, OPTYPE*4
      DOUBLE PRECISION OLDD(128), CLPD(128), FRQS(MAXIMG)
      REAL      OLDR(256), CLPR(256), XBUFF1(MABFSS)
      HOLLERITH OLDH(256), CLPH(256)
      INTEGER   BUFF1(MABFSS), CATOLD(256), CATCLP(256), SEQIN, SEQ2IN,
     *   DISKIN, DIS2IN, NEWCNO, OLDCNO, JBUFSZ, NAXSKP, BBLC(7),
     *   BTRC(7), LUNI1, LUNI2, INDI1, INDI2, TTYLUN, TTYIND, XCHRN(7),
     *   PRTLUN, PRTIND, LZOOM(3), PLTLUN, PLTIND, LINMAX, ITYPE,
     *   NPLOTS, LABEL, NPARM, NCTOT, NCPER, ISYM, OFFCH
      LOGICAL   ISFQID
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAM2I, XCLA2I,
     *   XSEQ2N, XDIS2I, BLC, TRC, BCHAN, DOALIN, DOINVR, PRTLEV,
     *   DOCRT, XLPNAM, XOUFIL, DOSLIC, TVCHAN, RANGE, XTVFUN, TXINC,
     *   TYINC, XNPLOT, FACTOR, XSYMB, XLTYPE, XYRATO, XOPTYP, APARM,
     *   BADD
      COMMON /BLSCHR/ NAMEIN, CLAIN, NAM2IN, CLA2IN, TVFUNC, LPNAME,
     *   OUTFIL, AUNITS, OPTYPE
      COMMON /PARMS/ FRQS, CATOLD, SEQIN, SEQ2IN, DISKIN, DIS2IN,
     *   NEWCNO, OLDCNO, JBUFSZ, NAXSKP, BBLC, BTRC, LUNI1, LUNI2,
     *   INDI1, INDI2, TTYLUN, TTYIND, XCHRN, PRTLUN, PRTIND, LZOOM,
     *   PLTLUN, PLTIND, LINMAX, ITYPE, ISFQID, NPLOTS, LABEL, NPARM,
     *   NCTOT, NCPER, ISYM, OFFCH
      EQUIVALENCE (XBUFF1, BUFF1)
      COMMON /IMBUF/ BUFF1
      COMMON /MAPHDR/ CATCLP
      EQUIVALENCE (CATOLD, OLDH, OLDR, OLDD)
      EQUIVALENCE (CATCLP, CLPH, CLPR, CLPD)
C                                                          End BLSUM.
LOCAL END
      PROGRAM BLSUM
C-----------------------------------------------------------------------
C! Sums images over blotch regions
C# Map TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998-2000, 2004, 2007-2010, 2012-2015
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
C   BLSUM sums images over blotch regions.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input image.
C      INCLASS        CLAIN         Class of input image.
C      INSEQ          SEQIN         Seq. of input image.
C      INDISK         DISKIN        Disk number of input image.
C      IN2NAME                      Name of blotch image: ' ' => #1
C      IN2CLASS
C      IN2SEQ
C      IN2DISK
C      BLC(7)         BLC           Bottom left corner of subimage
C                                   of input image.
C      TRC(7)         TRC           Top right corner of subimage.
C      BCHAN          BCHAN         First channel with signal, Plane in
C                                   INNAME to use for blotch iff IN2NAME
C                                   blank.
C      DOALIGN                      How to line up 2 images
C      PRTLEV                       > 0 => print pixel locations
C                                   >= 2 printer plot
C      DOCRT                        >0 => use CRT not line printer
C      OUTPRINT       LPNAME        File to save printer output in
C      TVCHAN                       TV channel to use
C      PIXRANGE                     Min,Max of image intensity
C                                   Max <= Min => entire range
C      FUNCTYPE                     TV load transfer function type
C      TXINC                        X increment in TV loads
C      TYINC                        Y increment in TV loads
C      BADDISK                      Disks to avoid for scratch.
C   Programmer Eric W. Greisen:  March 1984
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, NWORDS, BLITCH(2)
      LONGINT   PBLOT, PSPEC, PWT
      REAL      BLOTCH(2), SPEC(2), WEIGHT(2)
      EQUIVALENCE (BLOTCH, BLITCH)
      INCLUDE 'BLSUM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'BLSUM '/
C-----------------------------------------------------------------------
C                                       Get input parameters, open up
      CALL BLSMIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       get memory for blotch plane
      NWORDS = (XCHRN(1) * XCHRN(2) - 1) / 1024 + 4
      CALL ZMEMRY ('GET ', PRGM, NWORDS, BLOTCH, PBLOT, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'CANNOT GET DYNAMIC MEMORY FOR BLOTCH PLANE'
         CALL MSGWRT (8)
         GO TO 990
         END IF
      CALL ZMEMRY ('GET ', PRGM, NWORDS, WEIGHT, PWT, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'CANNOT GET DYNAMIC MEMORY FOR WEIGHT PLANE'
         CALL MSGWRT (8)
         GO TO 990
         END IF
      NWORDS = (5 * NCTOT - 1) / 1024 + 4
      CALL ZMEMRY ('GET ', PRGM, NWORDS, SPEC, PSPEC, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'CANNOT GET DYNAMIC MEMORY FOR SPECTRA'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Call routine that sends data
C                                       to the summing routine
      CALL BLSMTV (XCHRN(1), XCHRN(2), BLITCH(1+PBLOT), NCTOT,
     *   WEIGHT(1+PWT), SPEC(1+PSPEC), SPEC(1+NCTOT+PSPEC), IRET)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE BLSMIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   BLSMIN gets input parameters for BLSUM and creates an output file
C   IF requested for the residual map.
C   Inputs:  PRGN    C*6      Program name
C   Output:  IRET    I         Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      CHARACTER IFILE*48, STAT*4, MTYPE*2, CTEMP*8
      INTEGER   IERR, IROUND, I, NA(2), NPL, J, K, KA, NX1, NX2, NFQ,
     *   ORDER, NZI
      REAL      EPS, AXV
      DOUBLE PRECISION DAXV
      LOGICAL   T, F, EQUAL
      INCLUDE 'BLSUM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
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
      NPARM = 90
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR, 'OBTAINING INPUT PARAMETERS'
         CALL MSGWRT (8)
         END IF
C                                       Using the TV
      IF ((NPOPS.GT.NINTRN) .OR. (NTVDEV.LE.0)) THEN
         MSGTXT = 'THERE IS NO TV ASSIGNED TO YOUR AIPS'
         CALL MSGWRT (8)
         IRET = 8
         END IF
C                                       Restart AIPS
      RQUICK = RQUICK .AND. (DOCRT.LT.0.0)
      IF ((RQUICK) .AND. (IRET.NE.0)) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSEQIN)
      DISKIN = IROUND (XDISKI)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAM2I, NAM2IN)
      CALL H2CHR (6, 1, XCLA2I, CLA2IN)
      CALL H2CHR (4, 1, XTVFUN, TVFUNC)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      CALL H2CHR (48, 1, XOUFIL, OUTFIL)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      DO 25 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 25      CONTINUE
      NPLOTS = ABS (XNPLOT) + 0.1
      NPLOTS = MAX (0, MIN (4, NPLOTS))
      LABEL = IROUND (XLTYPE)
      IF (MOD (ABS(LABEL),100).EQ.0) THEN
         IF (LABEL.GE.0) THEN
            LABEL = (LABEL/100)*100  + 3
         ELSE
            LABEL = (LABEL/100)*100  - 3
            END IF
         END IF
      IF (FACTOR.EQ.0.0) FACTOR = 1.0
      IF (XYRATO.EQ.0.0) XYRATO = 1.414
      XLTYPE = LABEL
      ISYM = XSYMB + 0.1
      ISYM = MAX (1, MIN (24, ISYM))
      XSYMB = ISYM
      LUNI1 = 16
      LUNI2 = 17
      TTYLUN = 5
      TTYIND = 0
C                                       Get CATBLK from old file.
      OLDCNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      STAT = 'READ'
      IF (DOSLIC.GT.0.0) STAT = 'WRIT'
      IF (NPLOTS.GT.0) STAT = 'WRIT'
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READING INPUT IMAGE CATBLK'
         GO TO 990
         END IF
      NCFILE = 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      IF (STAT.EQ.'WRIT') FRW(NCFILE) = 1
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATOLD(KIDIM), CATOLD(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Second input file
      IF (NAM2IN.EQ.' ') THEN
         XDIS2I = XDISKI
         XSEQ2N = XSEQIN
         NAM2IN = NAMEIN
         CLA2IN = CLAIN
         END IF
      FRW(2) = -1
      NAXSKP = -1
      EPS = 0.2
      CALL FILL (7, 1, BBLC)
      CALL FILL (7, 1, BTRC)
C                                       Get CATBLK of 2nd old file.
      NEWCNO = 1
      DIS2IN = IROUND (XDIS2I)
      SEQ2IN = IROUND (XSEQ2N)
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DIS2IN, NEWCNO, NAM2IN, CLA2IN, SEQ2IN,
     *   MTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR, NAM2IN, CLA2IN, SEQ2IN, DIS2IN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DIS2IN, NEWCNO, CATCLP, 'READ', BUFF1, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.6) .AND. (IERR.NE.7)) THEN
         WRITE (MSGTXT,1000) IERR, 'READING BLOTCH IMAGE CATBLK'
         GO TO 990
         END IF
      NCFILE = 2
      FVOL(NCFILE) = DIS2IN
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 0
C                                       Decide IO pattern
      DO 65 I = 1,2
         NA(I) = 0
         K = KHCTP + (I-1) * 2
         DO 65 J = I,7
            CALL CHCOMP (8, 1, CLPH(K), 1, OLDH(KHCTP+(J-1)*2), EQUAL)
            IF (EQUAL) NA(I) = J
 65      CONTINUE
      NAXSKP = 0
      IF (DOALIN.LT.-1.5) GO TO 75
      IF ((NA(1).EQ.0) .OR. (NA(2).EQ.0)) GO TO 70
      IF ((NA(1).GT.2) .OR. (NA(2).GT.3)) GO TO 70
      IF (NA(2).LE.NA(1)) GO TO 70
         IF (NA(2).GT.2) NAXSKP = 2
         IF (NA(1).EQ.2) NAXSKP = 1
         GO TO 80
C                                       possible non-alignment
 70   IF (DOALIN.GT.-0.1) THEN
         MSGTXT = 'UNABLE TO ALIGN THE AXES - CHECK TRANSPOSITION'
         GO TO 990
         END IF
 75   NA(1) = 1
      NA(2) = 2
C                                       set corners, check alignment
 80   KA = 0
      IF (NAXSKP.GT.0) KA = 1
      NX1 = CATOLD(KIDIM)
      NX2 = CATCLP(KIDIM)
      DO 90 I = 1,7
         BBLC(I) = 1
         BTRC(I) = 1
         J = I - 1
         IF (CATCLP(KINAX+J).LT.1) CATCLP(KINAX+J) = 1
         IF ((I.LE.NX2) .AND. (I+KA.LE.NX1) .AND.
     *      (CATCLP(KINAX+J).GT.1)) THEN
            IF (I.LE.2) THEN
               K = NA(I) - 1
            ELSE
               K = I + KA - 1
               END IF
            AXV = CLPR(KRCRP+J) - OLDR(KRCRP+K) + BLC(K+1)
            IF (DOALIN.LT.-1.5) AXV = BLC(K+1)
            IF ((DOALIN.GT.-0.1) .AND. (CLPR(KRCIC+J).NE.0.0)) THEN
               DAXV = OLDD(KDCRV+K) + OLDR(KRCIC+K)*(BLC(K+1) -
     *            OLDR(KRCRP+K))
               AXV = (DAXV - CLPD(KDCRV+J)) / CLPR(KRCIC+J) +
     *            CLPR(KRCRP+J)
               END IF
            BBLC(I) = IROUND(AXV)
            BTRC(I) = BBLC(I) + TRC(K+1) - BLC(K+1) + EPS
            IF (BTRC(I).LT.1) GO TO 95
            IF (BBLC(I).GT.CATCLP(KINAX+J)) GO TO 95
C                                       smaller subim needed?
            IF (BBLC(I).LT.1) THEN
               BLC(K+1) = BLC(K+1) + 1 - BBLC(I)
               AXV = AXV + 1 - BBLC(I)
               BBLC(I) = 1
               END IF
            IF (BTRC(I).GT.CATCLP(KINAX+J)) THEN
               TRC(K+1) = TRC(K+1) + CATCLP(KINAX+J) - BTRC(I)
               BTRC(I) = CATCLP(KINAX+J)
               END IF
            IF (BTRC(I).LT.BBLC(I)) GO TO 95
            IF (BLC(K+1).GT.CATOLD(KINAX+K)) GO TO 95
            IF (TRC(K+1).LT.1) GO TO 95
C                                       full alignment check
            IF (DOALIN.GE.0.1) THEN
               IF (ABS(BBLC(I)-AXV).GT.EPS) GO TO 95
               CALL CHCOMP (8, 1, CLPH(KHCTP+J*2), 1, OLDH(KHCTP+K*2),
     *            EQUAL)
               IF (.NOT.EQUAL) GO TO 95
               AXV = EPS * EPS * ABS(OLDR(KRCIC+K))
               IF (ABS(OLDR(KRCIC+K)-CLPR(KRCIC+J)).GT.AXV) GO TO 95
               IF (ABS(OLDR(KRCRT+K)-CLPR(KRCRT+J)).GT.1.0) GO TO 95
               END IF
            END IF
 90      CONTINUE
      GO TO 100
C                                       Failure to align
 95   CONTINUE
         MSGTXT = 'INPUT AND BLOTCH IMAGES DO NOT OVERLAP:' //
     *   ' CHECK HEADERS'
         GO TO 990
C                                       OK : which plane for blotch
 100  NPL = 1
      DO 105 I = 3,NX2
         NPL = NPL * CATCLP(KINAX+I-1)
 105     CONTINUE
      IF (BCHAN.LT.1) BCHAN = 1
      IF (BCHAN.GT.NPL) BCHAN = NPL
      J = BCHAN + 0.01
      CALL FILL (7, 1, XCHRN)
      XCHRN(1) = CATCLP(KINAX)
      XCHRN(2) = CATCLP(KINAX+1)
      IF (J.GT.1) THEN
         DO 120 I = 3,NX2
            XCHRN(I) = MOD (J-1, CATCLP(KINAX+I-1)) + 1
            J = (J - XCHRN(I)) / CATCLP(KINAX+I-1) + 1
 120        CONTINUE
         END IF
C                                       Open files: main image
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI1, INDI1, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT IMAGE FILE'
         GO TO 990
         END IF
C                                       Open blotch image
      CALL ZPHFIL ('MA', DIS2IN, FCNO(2), 1, IFILE, IRET)
      CALL ZOPEN (LUNI2, INDI2, DIS2IN, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT BLOTCH FILE'
         GO TO 990
         END IF
C                                       Open plot file
      IF (OUTFIL.NE.' ') THEN
         PLTLUN = 11
         CALL ZTXOPN ('WRIT', PLTLUN, PLTIND, OUTFIL, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'UNABLE TO OPEN PLOT OUTPUT: TURNING IT OFF'
            CALL MSGWRT (8)
            OUTFIL = ' '
            END IF
         END IF
C                                       Open line printer
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      IF (DOCRT.NE.0.0) THEN
         CALL LPOPEN (LPNAME, DOCRT, PRTLUN, PRTIND, LINMAX, BUFF1,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING LINE PRINTER'
            CALL MSGWRT (8)
            DOCRT = 1.0
            IRET = 0
            END IF
         END IF
C                                       FQID axis?
      I = NAXSKP
      IF (I.EQ.0) I = 3
      CALL H2CHR (8, 1, OLDH(KHCTP+2*(I-1)), CTEMP)
      ISFQID = CTEMP.EQ.'FQID'
      IF (ISFQID) THEN
         CALL FRQGET (DISKIN, OLDCNO, NFQ, ORDER, FRQS, IRET)
         IF ((IRET.NE.0) .OR. (ABS(ORDER).NE.1)) THEN
            MSGTXT = 'FQID AXIS NOT CHANGED TO FREQUENCIES'
            CALL MSGWRT (7)
            ISFQID = .FALSE.
         ELSE
            NZI = TRC(I) - BLC(I) + 1.1
            J = BLC(I) - 0.5
            DO 210 I = 1,NZI
               FRQS(I) = FRQS(I+J)
 210           CONTINUE
            END IF
         END IF
C                                       total number plotted pixels
      IF (NAXSKP.GT.0) THEN
         NCPER = TRC(NAXSKP) - BLC(NAXSKP) + 1.01
      ELSE
         NCPER = TRC(3) - BLC(3) + 1.01
         END IF
      NCTOT = NCPER
      DO 220 I = 4,7
         J = TRC(I) - BLC(I) + 1.01
         NCTOT = NCTOT * MAX (J, 1)
 220     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLSMIN: ERROR',I3,1X,A)
 1020 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
      END
      SUBROUTINE BLSMTV (NX, NY, BLOTCH, NC, WEIGHT, SPCTRM, SPCTRA,
     *   IRET)
C-----------------------------------------------------------------------
C   BLSMTV sends image one row at a time to the blanking routine and
C   then writes the modified data.  For the TV opcode only.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NX, NY, BLOTCH(NX,NY), NC, IRET
      REAL      SPCTRM(*), WEIGHT(NX,NY), SPCTRA(NC,*)
C
      CHARACTER OFF*4, TITL1*132, TITL2*132, ALINE*132, PSCR*132, ON*4,
     *   CEYE*2, CPLUS*2, CAST*2, JYB*8, LLCHAR(5)*4, MMCHAR(5)*4,
     *   BUNITS*8, CCHAR*4, PLINE*132, CTEMP*18, ATIME*8, ADATE*12,
     *   CUNITS*8, CUNITU*8, BUNITU*8
      INTEGER   ZOR, IBLVAL, OFF3, WINI(4), I, J, K, LIM2, LIM3, LIM4,
     *   LIM5, BOI, LIM6, LIM7, NPOLY, IG, IBIND, MV, LIM1, NVERT(40),
     *   NF, NXF, NYF, NXT, NYT, JBUFS4, IERR, XLO, XHI, YLO, LINC(2),
     *   WINT(4,64), WINX(4), IROUND, TVPLAN, ICHAN, XV(400), YV(400),
     *   WINI2(4), IXL(15), IXU(15), WINBL(4), LLV, LNX, LPOLY, IX, IY,
     *   J1, J2, JG1, JG2, JJ, I1L, I2L, I1U, I2U, IG2, NXI2, NYI2, IS,
     *   I1, I2, I3, I4, I5, I6, I7, IV, NXI, NYI, CORN(7), IS0, BLNUM,
     *   LINCNT, NM, IPAGE, XPIX, NCHAR, ID(3), IT(3), PLAX, ITRIM, YHI,
     *   JX, JY, IPLOTS, BLNUMS(4), WCOUNT
      REAL      XP, YP, XXCHAR, AREAT, CSUM, V, SMIN, SMAX, AREAB,
     *   WSUM, W2SUM, WT
      LOGICAL   T, F
      INCLUDE 'BLSUM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      DATA CEYE, CPLUS, CAST /'II', '++', '**'/
      DATA JYB /'JY/BEAM '/
      DATA OFF, ON /'OFFF', 'ONNN'/
      DATA LLCHAR, MMCHAR /'LL  ','RA  ','RA--','GLON','ELON',
     *   'MM  ','DEC ','DEC-','GLAT','ELAT'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open TV
      CALL TVOPEN (BUFF1, IRET)
      IF (IRET.NE.0) GO TO 995
      IPLOTS = 0
C                                       init display
      LINCNT = 999
      IPAGE = 0
      WRITE (TITL1,1010) NAMEIN, CLAIN, SEQIN
      WRITE (TITL2,1011) NAM2IN, CLA2IN, SEQ2IN, (XCHRN(I), I = 3,7)
      IF (DOCRT.LE.-2.5) THEN
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, LINMAX, TITL1, TITL2,
     *      TITL1, LINCNT, IPAGE, PSCR, IRET)
         IF (IRET.NE.0) GO TO 990
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, LINMAX, TITL1, TITL2,
     *      TITL2, LINCNT, IPAGE, PSCR, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      IF (NGRAPH.LE.0) THEN
         IRET = 1
         MSGTXT = 'BLSUM REQUIRES THE TV TO HAVE A GRAPHICS PLANE'
         GO TO 980
         END IF
C                                       Open terminal
      IF (DOCRT.LE.0) THEN
         CALL ZOPEN (TTYLUN, TTYIND, 1, MSGTXT, F, T, T, IRET)
         IF (IRET.NE.0) THEN
            TTYIND = 0
            WRITE (MSGTXT,1000) IRET, 'OPENING USER TERMINAL'
            GO TO 980
            END IF
      ELSE
         TTYLUN = PRTLUN
         TTYIND = PRTIND
         END IF
      JBUFS4 = JBUFSZ
      IBLVAL = 0
      IF (DOINVR.GT.0.0) IBLVAL = 1
C                                       Setup for I/O
      NXI2 = CATCLP(KINAX)
      NYI2 = CATCLP(KINAX+1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
      WINI2(1) = BBLC(1)
      WINI2(2) = BBLC(2)
      WINI2(3) = BTRC(1)
      WINI2(4) = BTRC(2)
C                                       TV windows
      LINC(1) = TXINC + 0.01
      LINC(2) = TYINC + 0.01
      LINC(1) = MAX (1, LINC(1))
      LINC(2) = MAX (1, LINC(2))
      TXINC = LINC(1)
      TYINC = LINC(2)
      NXT = (BTRC(1) - BBLC(1)) / TXINC + 1.0001
      NYT = (BTRC(2) - BBLC(2)) / TYINC + 1.0001
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
      DO 15 I = 1,NYF
         DO 10 J = 1,NXF
            K = K + 1
            WINT(1,K) = BBLC(1) + XP * (J-1)
            WINT(2,K) = BBLC(2) + YP * (I-1)
            IF (NXF.GT.1) WINT(3,K) = WINT(1,K) + MAXXTV(1)*LINC(1) - 1
            IF (NXF.EQ.1) WINT(3,K) = WINT(1,K) + BTRC(1) - BBLC(1)
     *         + 0.01
            IF (NYF.GT.1) WINT(4,K) = WINT(2,K) + MAXXTV(2)*LINC(2) - 1
            IF (NYF.EQ.1) WINT(4,K) = BTRC(2) + 0.01
 10         CONTINUE
 15      CONTINUE
      WINX(1) = (MAXXTV(1) - (WINT(3,1)-WINT(1,1)) / LINC(1) + 1) / 2
      WINX(2) = (MAXXTV(2) - (WINT(4,1)-WINT(2,1)) / LINC(2) + 1) / 2
      WINX(3) = WINX(1) + (WINT(3,1) - WINT(1,1)) / LINC(1)
      WINX(4) = WINX(2) + (WINT(4,1) - WINT(2,1)) / LINC(2)
C                                       other TV parms
      ICHAN = TVCHAN + 0.01
      IF ((ICHAN.LT.1) .OR. (ICHAN.GT.NGRAY)) ICHAN = 1
      TVPLAN = 2 ** (ICHAN-1)
      IG = MIN (3, NGRAPH)
      IG2 = MAX (IG-1, 1)
      CALL RNGSET (RANGE, CLPR(KRDMX), CLPR(KRDMN), CLPR(IRRAN))
      CALL CHR2H (2, TVFUNC(1:2), 1, CATCLP(IITRA))
      CATCLP(IICNO) = FCNO(2)
      CATCLP(IIVOL) = FVOL(2)
C                                       Init the TV
      JG1 = IG + NGRAY
      JG2 = IG2 + NGRAY
      CALL MOVIST (OFF, TVPLAN, 0, 0, 0, IERR)
      CALL YCINIT (ICHAN, BUFF1)
      CALL YCINIT (JG1, BUFF1)
      CALL YZERO (ICHAN, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL YZERO (JG1, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (JG2.NE.JG1) CALL YZERO (JG2, IERR)
      IF (IERR.NE.0) GO TO 900
      J = NGRAY + NGRAPH
      DO 20 K = 1,J
         CALL YSLECT (OFF, K, 0, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 900
 20      CONTINUE
      CALL YSLECT (ON, JG1, 0, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (JG1.NE.JG2) CALL YSLECT (ON, JG2, 0, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL YSLECT (ON, ICHAN, 0, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 900
      I = 2 ** NGRAY
      I = ZOR (I, TVPLAN)
      CALL YSCROL (I, 0, 0, F, IERR)
      IF (IERR.NE.0) GO TO 900
      TVZOOM(1) = 0
      TVZOOM(2) = MAXXTV(1) / 2
      TVZOOM(3) = MAXXTV(2) / 2
      CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), F, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL COPY (3, TVZOOM, LZOOM)
C                                       Setup for looping
      LIM4 = TRC(4) - BLC(4) + 1.01
      LIM5 = TRC(5) - BLC(5) + 1.01
      LIM6 = TRC(6) - BLC(6) + 1.01
      LIM7 = TRC(7) - BLC(7) + 1.01
      NPOLY = 0
      MV = 0
      CALL FILL (40, 0, NVERT)
      CALL COPY (5, XCHRN(3), CATCLP(IIDEP))
      BLNUM = 0
      APARM(3) = MAX (0.0, APARM(3))
C                                       read blotch image into ram
      WINBL(1) = 1
      WINBL(2) = 1
      WINBL(3) = NXI2
      WINBL(4) = NYI2
      CALL COMOFF (CATCLP(KIDIM), CATCLP(KINAX), XCHRN(3), BOI, IRET)
      BOI = BOI + 1
      CALL MINIT ('READ', LUNI2, INDI2, NXI2, NYI2, WINBL, BUFF1,
     *   JBUFSZ, BOI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1175) IRET
         GO TO 980
         END IF
C                                       loop over rows
      DO 30 IY = 1,NYI2
         CALL MDISK ('READ', LUNI2, INDI2, BUFF1, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1185) IRET
            GO TO 980
            END IF
         CALL RCOPY (NXI2, BUFF1(IBIND), WEIGHT(1,IY))
 30      CONTINUE
C                                       SUM loop point:
C                                       Find polygons
 50   CALL RFILL (NC, 0.0, SPCTRM)
      K = NX * NY
      CALL FILL (K, IBLVAL, BLOTCH)
      LINCNT = 999
      BLNUM = BLNUM + 1
      LPOLY = NPOLY
      LLV = MV
      NPOLY = 0
      DO 60 K = 1,NF
         IF (NF.GT.1) THEN
            WRITE (MSGTXT,1051) WINT(1,K), WINT(3,K), WINT(2,K),
     *         WINT(4,K)
            CALL MSGWRT (1)
            END IF
         IF ((NF.GT.1) .OR. (BLNUM.LE.1)) THEN
            CALL TVLOAD (LUNI2, INDI2, ICHAN, LINC, WINX, WINT(1,K),
     *         JBUFS4, XBUFF1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'LOADING IMAGE ON TV'
               GO TO 980
               END IF
            END IF
         CALL BLSTVF (IG, IG2, TVPLAN, LINC, TTYLUN, TTYIND, LPOLY,
     *      NPOLY, NVERT, XV, YV, LZOOM, BUFF1, IRET)
         IF ((IRET.EQ.99) .AND. (BLNUM.GT.1)) GO TO 800
         IF (IRET.NE.0) GO TO 990
 60      CONTINUE
C                                       new polygons
      IF (NPOLY.GT.0) THEN
C                                       set limits
         NPOLY = NPOLY + LPOLY
         LPOLY = LPOLY + 1
         LLV = LLV + 1
         MV = 0
         DO 65 I = 1,NPOLY
            MV = MV + NVERT(I)
 65         CONTINUE
         I1L = MABFSS
         I2L = MABFSS
         I1U = 1
         I2U = 1
         DO 70 I = LLV,MV
            I1L = MIN (I1L, XV(I))
            I1U = MAX (I1U, XV(I))
            I2L = MIN (I2L, YV(I))
            I2U = MAX (I2U, YV(I))
 70         CONTINUE
         DO 165 IY = 1,NYI2
            LNX = 0
            IF (IY.LE.I2U) CALL BLTLIS (LPOLY, LLV, NPOLY, NVERT, XV,
     *         YV, IY, LNX,IXL, IXU)
            IF ((IY.GE.I2L) .AND. (IY.LE.I2U) .AND. (LNX.GT.0)) THEN
C                                       loop over row
               IF ((PRTLEV.GT.1.05) .AND. (DOCRT.NE.0.0)) THEN
                  J2 = 0
 146              J1 = J2 + 1
                  IF (J1.LE.LNX) THEN
                     J2 = (LINMAX - 21) / 14 - 1
                     J2 = MIN (LNX, J1+J2)
                     IF (DOINVR.LE.0.0) WRITE (ALINE,1146) IY,
     *                  (IXL(I), IXU(I), I=J1,J2)
                     IF (DOINVR.GT.0.0) WRITE (ALINE,1147) IY,
     *                  (IXL(I), IXU(I), I=J1,J2)
                     CALL PRTLIN (PRTLUN, PRTIND, DOCRT, LINMAX, TITL1,
     *                  TITL2, ALINE, LINCNT, IPAGE, PSCR, IRET)
                     IF (IRET.NE.0) GO TO 990
                     GO TO 146
                     END IF
                  END IF
C                                       Inside a blotch?
               DO 160 IX = I1L,I1U
                  DO 150 I = 1,LNX
C                                       Add it in
                     IF ((IX.GE.IXL(I)). AND. (IX.LE.IXU(I))) THEN
                        BLOTCH(IX,IY) = 1 - IBLVAL
                        GO TO 160
                        END IF
 150                 CONTINUE
 160              CONTINUE
               END IF
 165        CONTINUE
C                                       Apply to clip image
C                                       Range sum outside:
         IF (DOINVR.GT.0.0) THEN
            I1L = BBLC(1)
            I2L = BBLC(2)
            I1U = BTRC(1)
            I2U = BTRC(2)
            END IF
         WINBL(1) = I1L
         WINBL(2) = I2L
         WINBL(3) = I1U
         WINBL(4) = I2U
         XPIX = 0
         CSUM = 0.0
         WSUM = 0.0
         W2SUM = 0.0
         WCOUNT = 0
C                                       loop over rows
         DO 210 IY = I2L,I2U
            DO 205 IX = I1L,I1U
               IF (BLOTCH(IX,IY).NE.0) THEN
                  IF (WEIGHT(IX,IY).NE.FBLANK) THEN
                     V = WEIGHT(IX,IY)
                     CSUM = CSUM + V
                     XPIX = XPIX + 1
                     IF (V.GE.APARM(3)) THEN
                        W2SUM = W2SUM + V * V
                        WSUM = WSUM + V
                        WCOUNT = WCOUNT + 1
                        END IF
                     END IF
                  END IF
 205           CONTINUE
 210        CONTINUE
         IF ((OPTYPE.NE.'WEIG') .AND. (OPTYPE.NE.'WEI2')) THEN
            WSUM = 1.0
            W2SUM = 1.0
         ELSE IF (WCOUNT.LE.0) THEN
            MSGTXT = 'NO BLOTCH PIXELS ABOVE WEIGHT CUTOFF'
            CALL MSGWRT (6)
            GO TO 50
         ELSE
            WSUM = WSUM / WCOUNT
            W2SUM = W2SUM / WCOUNT
            END IF
C                                       Output remarks: -> Jy
         CALL H2CHR (8, 1, OLDH(KHBUN), BUNITS)
         CALL H2CHR (8, 1, CLPH(KHBUN), CUNITS)
         CUNITU = CUNITS
         BUNITU = BUNITS
         CALL CHLTOU (8, BUNITU)
         CALL CHLTOU (8, CUNITU)
         J = CATOLD(KIDIM)
         J1 = 0
         J2 = 0
         DO 215 I = 1,J
            CALL H2CHR (4, 1, OLDH(KHCTP+(I-1)*2), CCHAR)
            DO 214 JJ = 1,5
               IF (CCHAR.EQ.LLCHAR(JJ)) J1 = I
               IF (CCHAR.EQ.MMCHAR(JJ)) J2 = I
 214           CONTINUE
 215        CONTINUE
         XXCHAR = 1.0
         IF ((J1.GT.0) .AND. (J2.GT.0)) XXCHAR = ABS (CLPR(KRCIC+J1-1)
     *      * CLPR(KRCIC+J2-1))
         AREAB = 0.0
         IF ((XXCHAR.NE.1.0) .AND. (CUNITU.EQ.JYB) .AND.
     *      (CATCLP(KINIT).GT.0)) AREAB =
     *      1.1331 * CLPR(KRBMJ) * CLPR(KRBMN)
         IF (AREAB.GT.0.0) THEN
            ITYPE = 1
         ELSE
            ITYPE = 2
            AREAB = 1.0
            END IF
         AREAB = AREAB / XXCHAR
         WRITE (MSGTXT,1215) 'Blotch', XPIX
         ALINE = MSGTXT
         IF (DOCRT.LE.0.0) CALL MSGWRT (5)
         IF (DOCRT.NE.0.0) THEN
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, LINMAX, TITL1, TITL2,
     *         ALINE, LINCNT, IPAGE, PSCR, IRET)
            IF (IRET.GT.0) GO TO 990
            END IF
         IF ((ITYPE.EQ.1) .AND. (CUNITU.EQ.BUNITU)) THEN
            CSUM = CSUM / AREAB
            WRITE (MSGTXT,1216) 'Blotch', CSUM, AREAB
            CUNITS = 'Jy'
            CUNITU = 'JY'
         ELSE
            WRITE (MSGTXT,1217) 'Blotch', CSUM, CUNITS
            END IF
         ALINE = MSGTXT
         IF (DOCRT.LE.0.0) CALL MSGWRT (5)
         IF (DOCRT.NE.0.0) THEN
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, LINMAX, TITL1, TITL2,
     *         ALINE, LINCNT, IPAGE, PSCR, IRET)
            IF (IRET.GT.0) GO TO 990
            END IF
         IF (OPTYPE(:3).EQ.'WEI') THEN
            IF (OPTYPE.EQ.'WEI2') THEN
               WRITE (MSGTXT,1214) 'Weight', WCOUNT
            ELSE
               WRITE (MSGTXT,1215) 'Weight', WCOUNT
               END IF
            ALINE = MSGTXT
            IF (DOCRT.LE.0.0) CALL MSGWRT (5)
            IF (DOCRT.NE.0.0) THEN
               CALL PRTLIN (PRTLUN, PRTIND, DOCRT, LINMAX, TITL1, TITL2,
     *            ALINE, LINCNT, IPAGE, PSCR, IRET)
               IF (IRET.GT.0) GO TO 990
               END IF
            CSUM = WSUM * WCOUNT
            IF ((ITYPE.EQ.1) .AND. (CUNITU.EQ.BUNITU)) THEN
               CSUM = CSUM / AREAB
               WRITE (MSGTXT,1216) 'Weight', CSUM, AREAB
               CUNITS = 'Jy'
               CUNITU = 'JY'
            ELSE
               WRITE (MSGTXT,1217) 'Weight', CSUM, CUNITS
               END IF
            ALINE = MSGTXT
            IF (DOCRT.LE.0.0) CALL MSGWRT (5)
            IF (DOCRT.NE.0.0) THEN
               CALL PRTLIN (PRTLUN, PRTIND, DOCRT, LINMAX, TITL1, TITL2,
     *            ALINE, LINCNT, IPAGE, PSCR, IRET)
               IF (IRET.GT.0) GO TO 990
               END IF
            END IF
C                                       units
         XXCHAR = 1.0
         IF ((J1.GT.0) .AND. (J2.GT.0)) XXCHAR = ABS (OLDR(KRCIC+J1-1)
     *      * OLDR(KRCIC+J2-1))
         AREAT = 0.0
         IF ((XXCHAR.NE.1.0) .AND. (BUNITU.EQ.JYB) .AND.
     *      (CATOLD(KINIT).GT.0)) AREAT =
     *      1.1331 * OLDR(KRBMJ) * OLDR(KRBMN)
         IF (AREAT.GT.0.0) THEN
            ITYPE = 1
            CUNITS = 'Jy'
            CUNITU = 'JY'
         ELSE
            ITYPE = 2
            AREAT = 1.0
            CUNITS = BUNITS
            CUNITU = BUNITU
            END IF
         AREAT = AREAT / XXCHAR
C                                       PLOTR ouput file
C                                       Title
         IF ((OUTFIL.NE.' ') .AND. (BLNUM.EQ.1)) THEN
            CALL H2CHR (18, 1, OLDH(KHIMN), CTEMP)
            ALINE = 'BLSUM ON'
            CALL NAMEST (CTEMP, CATOLD(KIIMS), ALINE(10:), NCHAR)
            NCHAR = NCHAR + 12
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, ATIME, ADATE)
            ALINE(NCHAR:) = ADATE // ' ' // ATIME
            NCHAR = ITRIM (ALINE)
            CALL ZTXIO ('WRIT', PLTLUN, PLTIND, ALINE(:NCHAR), IRET)
            IF (IRET.NE.0) OUTFIL = ' '
            END IF
C                                       Bottom line 1
         IF ((OUTFIL.NE.' ') .AND. (BLNUM.EQ.1)) THEN
            IF (ITYPE.EQ.1) THEN
               ALINE = 'Flux over selected area(s)'
            ELSE
               ALINE = 'Average brightness over selected area(s)'
               END IF
            NCHAR = ITRIM (ALINE)
            CALL ZTXIO ('WRIT', PLTLUN, PLTIND, ALINE(:NCHAR), IRET)
            IF (IRET.NE.0) OUTFIL = ' '
C                                       Bottom line 2
            ALINE = ' '
            NCHAR = 1
            CALL ZTXIO ('WRIT', PLTLUN, PLTIND, ALINE(:NCHAR), IRET)
            IF (IRET.NE.0) OUTFIL = ' '
C                                       Xaxis
            IF (NAXSKP.EQ.0) THEN
               PLAX = 2
            ELSE
               PLAX = NAXSKP-1
               END IF
            CALL H2CHR (8, 1, OLDH(KHCTP+2*PLAX), ALINE)
            NCHAR = ITRIM (ALINE)
            CALL ZTXIO ('WRIT', PLTLUN, PLTIND, ALINE(:NCHAR), IRET)
            IF (IRET.NE.0) OUTFIL = ' '
C                                       Yaxis
            ALINE = CUNITS
            NCHAR = ITRIM (ALINE)
            CALL ZTXIO ('WRIT', PLTLUN, PLTIND, ALINE(:NCHAR), IRET)
            IF (IRET.NE.0) OUTFIL = ' '
C                                       5 plot type labels
            DO 218 I = 1,5
               WRITE (ALINE,1218) I
               NCHAR = 15
               CALL ZTXIO ('WRIT', PLTLUN, PLTIND, ALINE(:NCHAR), IRET)
               IF (IRET.NE.0) OUTFIL = ' '
 218           CONTINUE
            END IF
C                                       prepare parms for main image
         IF (NAXSKP.GT.0) THEN
            OFF3 = BLC(3) + 0.01
            LIM3 = I2U - I2L + 1
            OFF3 = I2L + OFF3 - BBLC(2)
            IV = TRC(NAXSKP) - BLC(NAXSKP) + 1.01
            OFFCH = BLC(NAXSKP) - 0.99
         ELSE
            LIM3 = TRC(3) - BLC(3) + 1.01
            OFF3 = BLC(3) + 0.01
            IV = LIM3
            OFFCH = BLC(3) - 0.99
            END IF
         IS0 = -IV
C                                       read window: X Y V
         IF (NAXSKP.EQ.0) THEN
            WINI(1) = I1L + BLC(1) - BBLC(1) + 0.01
            WINI(2) = I2L + BLC(2) - BBLC(2) + 0.01
            WINI(3) = WINI(1) + I1U - I1L
            WINI(4) = WINI(2) + I2U - I2L
C                                       window V X Y
         ELSE IF (NAXSKP.EQ.1) THEN
            WINI(1) = BLC(1) + 0.01
            WINI(2) = I1L + BLC(2) - BBLC(1) + 0.01
            WINI(3) = TRC(1) + 0.01
            WINI(4) = WINI(2) + I1U - I1L
C                                       window X V Y
         ELSE
            WINI(1) = I1L + BLC(1) - BBLC(1) + 0.01
            WINI(2) = BLC(2) + 0.01
            WINI(3) = WINI(1) + I1U - I1L
            WINI(4) = TRC(2) + 0.01
            END IF
         NXI = CATOLD(KINAX)
         NYI = CATOLD(KINAX+1)
         LIM1 = WINI(3) - WINI(1) + 1
         LIM2 = WINI(4) - WINI(2) + 1
         XLO = 1000000
         XHI = -XLO
         YLO = XLO
         YHI = XHI
C                                       loop to read cube
         DO 700 I7 = 1,LIM7
            CORN(7) = BLC(7) + I7 - 0.99
            DO 600 I6 = 1,LIM6
               CORN(6) = BLC(6) + I6 - 0.99
               DO 500 I5 = 1,LIM5
                  CORN(5) = BLC(5) + I5 - 0.99
                  DO 400 I4 = 1,LIM4
                     CORN(4) = BLC(4) + I4 - 0.99
                     IS0 = IS0 + IV
            DO 390 I3 = 1,LIM3
               CORN(3) = I3 + OFF3 - 1
               IF ((DOCRT.LE.0.0) .AND. (MOD(I3,25).EQ.1)) THEN
                  WRITE (MSGTXT,1310) CORN(3)
                  CALL MSGWRT (1)
                  END IF
C                                       init cube
               CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), CORN(3), BOI,
     *            IRET)
               BOI = BOI + 1
               CALL MINIT ('READ', LUNI1, INDI1, NXI, NYI, WINI, BUFF1,
     *            JBUFSZ, BOI, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1320) IRET
                  GO TO 980
                  END IF
               DO 380 I2 = 1,LIM2
C                                       read cube
                  CALL MDISK ('READ', LUNI1, INDI1, BUFF1, IBIND, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1330) IRET
                     GO TO 980
                     END IF
C                                       V-x-y transpose special
                  IF (NAXSKP.EQ.1) THEN
                     IF (BLOTCH(I2,I3).EQ.1) THEN
                        XLO = MIN (XLO, I2)
                        XHI = MAX (XHI, I2)
                        YLO = MIN (YLO, I3)
                        YHI = MAX (YHI, I3)
                        IF (OPTYPE.NE.'WEIG') THEN
                           WT = 1.0
                        ELSE IF ((WEIGHT(I2,I3).GE.APARM(3)) .AND.
     *                     (WEIGHT(I2,I3).NE.FBLANK)) THEN
                           IF (OPTYPE.EQ.'WEI2') THEN
                              WT = (WEIGHT(I2,I3) ** 2) / W2SUM
                           ELSE
                              WT = WEIGHT(I2,I3) / WSUM
                              END IF
                        ELSE
                           WT = 0.0
                           END IF
                        DO 350 I1 = 1,LIM1
                           IF (XBUFF1(IBIND+I1-1).NE.FBLANK) THEN
                              V = XBUFF1(IBIND+I1-1) * WT
                              SPCTRM(IS0+I1) = SPCTRM(IS0+I1) + V
                              END IF
 350                       CONTINUE
                        END IF
C                                       Other 2 cases
                  ELSE
                     IF (NAXSKP.EQ.2) THEN
                        IS = I2 + IS0
                        JY = CORN(3)
                     ELSE
                        JY = WINI(2) - 1 + I2
                        IS = I3 + IS0
                        END IF
                     DO 370 I1 = 1,LIM1
                        JX = WINI(1) - 1 + I1
                        IF ((BLOTCH(JX,JY).EQ.1) .AND.
     *                     (XBUFF1(IBIND+I1-1).NE.FBLANK)) THEN
                           IF (OPTYPE(:3).NE.'WEI') THEN
                              WT = 1.0
                           ELSE IF ((WEIGHT(JX,JY).GE.APARM(3)) .AND.
     *                        (WEIGHT(JX,JY).NE.FBLANK)) THEN
                              IF (OPTYPE.EQ.'WEI2') THEN
                                 WT = (WEIGHT(JX,JY) ** 2) / W2SUM
                              ELSE
                                 WT = WEIGHT(JX,JY) / WSUM
                                 END IF
                           ELSE
                              WT = 0.0
                              END IF
                           IF (WT.GT.0.0) THEN
                              XLO = MIN (XLO, I1)
                              XHI = MAX (XHI, I1)
                              IF (NAXSKP.EQ.2) THEN
                                 YLO = MIN (YLO, I3)
                                 YHI = MAX (YHI, I3)
                              ELSE
                                 YLO = MIN (YLO, I2)
                                 YHI = MAX (YHI, I2)
                                 END IF
                              V = XBUFF1(IBIND+I1-1) * WT
                              SPCTRM(IS) = SPCTRM(IS) + V
                              END IF
                           END IF
 370                    CONTINUE
                     END IF
 380              CONTINUE
 390           CONTINUE
 400                 CONTINUE
 500              CONTINUE
 600           CONTINUE
 700        CONTINUE
C                                       display results
         IS0 = IS0 + IV
         LINCNT = 999
         WRITE (ALINE,1700) BLNUM
         IF (DOCRT.NE.0.0) THEN
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, LINMAX, TITL1, TITL2,
     *         ALINE, LINCNT, IPAGE, PSCR, IRET)
            IF (IRET.GT.0) GO TO 990
            END IF
         IF ((ITYPE.EQ.1) .AND. (CUNITU.EQ.'JY')) THEN
            WRITE (MSGTXT,1226) AREAT
         ELSE
            WRITE (MSGTXT,1227) BUNITS
            CUNITS = BUNITS
            CUNITU = BUNITU
            END IF
         ALINE = MSGTXT
         IF (DOCRT.LE.0.0) CALL MSGWRT (5)
         IF (DOCRT.NE.0.0) THEN
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, LINMAX, TITL1, TITL2,
     *         ALINE, LINCNT, IPAGE, PSCR, IRET)
            IF (IRET.GT.0) GO TO 990
            END IF
C                                       Clean image
         XPIX = MAX (1, XPIX)
         DO 715 I = 1,IS0
            IF (ITYPE.EQ.1) THEN
               SPCTRM(I) = SPCTRM(I) / AREAT
               WRITE (ALINE,1710) I+OFFCH, SPCTRM(I)
            ELSE
               SPCTRM(I) = SPCTRM(I) / XPIX
               WRITE (ALINE,1711) I+OFFCH, SPCTRM(I), BUNITS
               END IF
            IF (DOCRT.NE.0.0) THEN
               CALL PRTLIN (PRTLUN, PRTIND, DOCRT, LINMAX, TITL1,
     *            TITL2, ALINE, LINCNT, IPAGE, PSCR, IRET)
               IF (IRET.GT.0) GO TO 990
               END IF
            IF (OUTFIL.NE.' ') THEN
               V = OLDD(KDCRV+PLAX) + (I-1.0+BLC(PLAX+1) -
     *            OLDR(KRCRP+PLAX)) * OLDR(KRCIC+PLAX)
               WRITE (ALINE,1715) V, SPCTRM(I), BLNUM
               NCHAR = ITRIM (ALINE)
               CALL ZTXIO ('WRIT', PLTLUN, PLTIND, ALINE(:NCHAR), IRET)
               IF (IRET.NE.0) OUTFIL = ' '
               END IF
 715        CONTINUE
         IF (NPLOTS.GT.0) THEN
            IPLOTS = IPLOTS + 1
            CALL RCOPY (NC, SPCTRM, SPCTRA(1,IPLOTS))
            BLNUMS(IPLOTS) = BLNUM
            END IF
C                                       units
         AUNITS(2) = CUNITS
         AUNITS(1) = 'PIXELS'
C                                       printer plot
         IF ((PRTLEV.GE.0.99) .AND. (IS0.GT.1)) THEN
            NM = 51
            IF (LINMAX.GE.119) NM = 101
            SMIN = 1.E15
            SMAX = -SMIN
            DO 750 I = 1,IS0
               IF (SPCTRM(I).NE.FBLANK) THEN
                  SMIN = MIN (SMIN, SPCTRM(I))
                  SMAX = MAX (SMAX, SPCTRM(I))
                  END IF
 750           CONTINUE
            IF ((SMIN.GE.0.0) .AND. (SMIN.LE.0.2*SMAX)) SMIN = 0.0
            LINCNT = 990
            WRITE (ALINE,1750) SMIN, SMAX, CUNITS
            IF (DOCRT.NE.0.0) THEN
               CALL PRTLIN (PRTLUN, PRTIND, DOCRT, LINMAX, TITL1, TITL2,
     *            ALINE, LINCNT, IPAGE, PSCR, IRET)
               IF (IRET.GT.0) GO TO 990
               END IF
            END IF
C                                       top bar
         IF ((PRTLEV.GE.0.99) .AND. (IS0.GT.1) .AND. (SMIN.LT.SMAX)
     *      .AND. (DOCRT.NE.0.0)) THEN
            DO 760 I = 1,NM,5
               PLINE(I:I+4) = '+----'
 760           CONTINUE
            WRITE (ALINE,1760) PLINE(1:NM)
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, LINMAX, TITL1, TITL2,
     *         ALINE, LINCNT, IPAGE, PSCR, IRET)
            IF (IRET.GT.0) GO TO 990
C                                       Plot
            DO 780 I = 1,IS0
               PLINE = ' '
               IF (MOD(I,10).EQ.0) THEN
                  DO 770 J = 1,NM,5
                     PLINE(J:J) = CPLUS
 770                 CONTINUE
                  END IF
               PLINE(1:1) = CEYE
               PLINE(NM:NM) = CEYE
               J = (SPCTRM(I) - SMIN) * (NM-1) / (SMAX - SMIN) + 0.5
               PLINE(J+1:J+1) = CAST
               IF (NM.EQ.51) WRITE (ALINE,1775) I+OFFCH, PLINE(1:NM),
     *            SPCTRM(I)
               IF (NM.EQ.101) WRITE (ALINE,1776) I+OFFCH, PLINE(1:NM),
     *            SPCTRM(I)
               CALL PRTLIN (PRTLUN, PRTIND, DOCRT, LINMAX, TITL1,
     *            TITL2, ALINE, LINCNT, IPAGE, PSCR, IRET)
               IF (IRET.GT.0) GO TO 990
 780           CONTINUE
C                                       bottom bar
            DO 785 I = 1,NM,5
               PLINE(I:I+4) = '+----'
 785           CONTINUE
            WRITE (ALINE,1760) PLINE(1:NM)
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, LINMAX, TITL1, TITL2,
     *         ALINE, LINCNT, IPAGE, PSCR, IRET)
            IF (IRET.GT.0) GO TO 990
            END IF
C                                       SL file
         IF ((DOSLIC.GT.0.0) .AND. (IS0.GT.1)) THEN
            IF (NAXSKP.EQ.0) THEN
               XLO = XLO + WINI(1) - 1
               XHI = XHI + WINI(1) - 1
               YLO = YLO + WINI(2) - 1
               YHI = YHI + WINI(2) - 1
            ELSE IF (NAXSKP.EQ.1) THEN
               XLO = XLO + WINI(2) - 1
               XHI = XHI + WINI(2) - 1
               YLO = YLO + OFF3 - 1
               YHI = YHI + OFF3 - 1
            ELSE
               XLO = XLO + WINI(1) - 1
               XHI = XHI + WINI(1) - 1
               YLO = YLO + OFF3 - 1
               YHI = YHI + OFF3 - 1
               END IF
            IF (NAXSKP.GT.0) THEN
               XLO = (XLO + XHI) / 2
               XHI = XLO
               YLO = (YLO + YHI) / 2
               YHI = YLO
               END IF
            CALL SLIBLS (IS0, SPCTRM, XLO, XHI, YLO, YHI, IRET)
            IF (IRET.GT.0) GO TO 990
            END IF
C                                       real plot
         IF ((NPLOTS.GT.0) .AND. (IPLOTS.EQ.NPLOTS)) THEN
            CALL PLTBLS (NC, IPLOTS, BLNUMS, SPCTRA, IRET)
            IF (IRET.GT.0) GO TO 990
            IPLOTS = 0
            END IF
C                                       loop for another sum
         GO TO 50
         END IF
C                                       Close files
 800  CALL ZCLOSE (LUNI1, INDI1, IRET)
      CALL ZCLOSE (LUNI2, INDI2, IRET)
      IF ((NPLOTS.GT.0) .AND. (IPLOTS.GT.0)) THEN
         CALL PLTBLS (NC, IPLOTS, BLNUMS, SPCTRA, IRET)
         IF (IRET.GT.0) GO TO 990
         IPLOTS = 0
         END IF
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
      IF ((DOCRT.LT.0.0) .AND. (PRTIND.GT.0)) CALL LPCLOS (PRTLUN,
     *   PRTIND, LINCNT, IERR)
       IF ((OUTFIL.NE.' ') .OR. (PLTIND.GT.0)) CALL ZTXCLS (PLTLUN,
     *    PLTIND, IERR)
C                                       resume AIPS on error
 995  IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLSMTV: ERROR',I3,1X,A)
 1010 FORMAT ('Summed image: ',A12,'.',A6,'.',I4)
 1011 FORMAT ('Blotch image: ',A12,'.',A6,'.',I4,' at depth',5I5)
 1051 FORMAT ('Begin subimage xpix =',I5,' -',I5,'  ypix =',I5,' -',I5)
 1146 FORMAT ('Row',I5,' include xpix',7(I5,' -',I5,2X))
 1147 FORMAT ('Row',I5,' exclude xpix',7(I5,' -',I5,2X))
 1175 FORMAT ('BLSMTV: MINIT (BLOTCH READ) ERROR',I3)
 1185 FORMAT ('BLSMTV: READ (BLOTCH) ERROR',I3)
 1214 FORMAT (A,' map summed over',I10,' pixels, using square',
     *   ' of values')
 1215 FORMAT (A,' map summed over',I10,' pixels')
 1216 FORMAT (A,' map flux',1PE13.5,' JY.  beam area',0PF7.2,
     *   ' pixels')
 1217 FORMAT (A,' map sum brightness',1PE13.5,' in ',A)
 1218 FORMAT ('Blotch number',I2)
 1226 FORMAT ('Target map beam area',0PF7.2,' pixels')
 1227 FORMAT ('Blotch map brightnesses in ',A)
 1310 FORMAT ('Start summing plane',I6)
 1320 FORMAT ('BLSMTV: MINIT (CUBE READ) ERROR',I3)
 1330 FORMAT ('BLSMTV: READ (CUBE) ERROR',I3)
 1700 FORMAT ('Spectrum of blotch area(s)',I5,' :')
 1710 FORMAT ('Channel',I5,' total flux',1PE12.4,'  Jy')
 1711 FORMAT ('Channel',I5,' average brightness',1PE12.4,1X,A)
 1715 FORMAT (2(1PE14.6),I4)
 1750 FORMAT ('Plot spectrum from',1PE12.4,' to',1PE12.4,1X,A)
 1760 FORMAT (6X,A)
 1775 FORMAT (I5,1X,A,1X,1PE12.4)
 1776 FORMAT (I5,1X,A,1X,1PE12.4)
 1900 FORMAT ('BLSMTV: TV ERROR CODE',I7)
      END
      SUBROUTINE BLSTVF (IG1, IG2, ICHAN, LINC, TTYLUN, TTYIND,  LPOLY,
     *   NPY, NV, XV, YV, LZOOM, SCRTCH, IRET)
C-----------------------------------------------------------------------
C   BLSTVF does the finding of the desired polygons
C   Inputs: IG1     I          Graphics plane to use: 1 - NGRAPH (new)
C           IG2     I          Graphics plane to use for former ones
C           ICHAN   I          Image plane (bit mask)
C           LINC    I(2)       TV load pixel increments (x,y)
C           TTYLUN  I          LUN of terminal opened for user comm.
C           TTYIND  I          FTAB pointer for terminal
C           LPOLY   I          Previous number of polygons
C   In/out: NPY     I          Number of polygons
C           NV      I(30)      Number of vertices in each polygon
C           XV      I(400)     X pixel position of each vertex wrt the
C                              original image
C           YV      I(400)     Y pixel pos of vertices wrt input image
C   Output: SCRTCH  I(>520)    scratch buffer
C           IRET    I          error code 0 ok, 1 TV, 2 TTY, 99 user
C-----------------------------------------------------------------------
      CHARACTER TEMP*4, ON*4, OFF*4
      INTEGER   IG1, IG2, ICHAN, LINC(2), TTYLUN, TTYIND, LPOLY, NPY,
     *   NV(30), XV(400), YV(400), SCRTCH(*), IRET, LZOOM(3)
      INTEGER   IB, IE, IE0, NPY0, I, J, CATBLK(256), IERR, JERR, IBO,
     *   ITW(3), QUAD, IBUT, ICH, LB, LE, IX(3), IY(3), ICH2, LIE0, LPY,
     *   INPY
      REAL      PPOS(2), RPOS(2), DLIM
      LOGICAL   T, F, DOIT
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /MAPHDR/ CATBLK
      DATA T, F /.TRUE.,.FALSE./
      DATA ON,    OFF
     *   /'ONNN','OFFF'/
C-----------------------------------------------------------------------
      IRET = 1
      INPY = NPY
      NPY0 = NPY + LPOLY
      IE = 0
      IE0 = 0
      LIE0 = 1
C                                       Correct old ones to this subimg
      IF (NPY0.LE.0) GO TO 15
         DO 10 I = 1,NPY0
            IB = IE + 1
            IE = IB + NV(I) - 1
            IF (I.EQ.LPOLY) LIE0 = IE + 1
            DO 10 J = IB,IE
               XV(J) = (XV(J)-CATBLK(IIWIN)) / LINC(1) + CATBLK(IICOR)
               YV(J) = (YV(J)-CATBLK(IIWIN+1)) / LINC(2) +
     *            CATBLK(IICOR+1)
 10         CONTINUE
         IE0 = IE
C                                       clear graphics
 15   ICH = IG1 + NGRAY
      CALL YZERO (ICH, IERR)
      IF (IERR.NE.0) GO TO 900
      ICH2 = IG2 + NGRAY
      CALL YZERO (ICH2, IERR)
      NPY = INPY
      NPY0 = NPY + LPOLY
      IE = IE0
C                                       reset zoom
      CALL COPY (3, LZOOM, TVZOOM)
      CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), F, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       check window size
      CALL YWINDO ('READ', WINDTV, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       interactive zoom, enhance
      I = LUTOUT + 1
      CALL TVFIDL (ICHAN, I, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL COPY (3, TVZOOM, LZOOM)
C                                       fill current polygons
      IF (LPOLY.GT.0) CALL BLTFIL (LPOLY, NV, XV, YV, IG2, SCRTCH, IERR)
      IF (NPY.GT.0) CALL BLTFIL (NPY, NV(LPOLY+1), XV(LIE0), YV(LIE0),
     *   IG1, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL ZTIME (ITW)
      NPY0 = NPY0 + 1
      LPY = NPY + LPOLY
C                                       Start new polygon
 40   IF (LPY.GE.30) GO TO 800
      NPY = NPY + 1
      LPY = NPY + LPOLY
      IB = IE + 1
      IE = IB
      NV(LPY) = 0
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
            LPY = LPY - 1
            GO TO 200
C                                       Mark new vertex
 60      NV(LPY) = NV(LPY) + 1
         XV(IE) = RPOS(1) + 0.01
         YV(IE) = RPOS(2) + 0.01
         XV(IE) = MAX (CATBLK(IICOR), MIN (CATBLK(IICOR+2), XV(IE)))
         YV(IE) = MAX (CATBLK(IICOR+1), MIN (CATBLK(IICOR+3), YV(IE)))
         IF (IB.EQ.IE) XV(IE+1) = XV(IE)
         IF (IB.EQ.IE) YV(IE+1) = YV(IE)
         I = MAX (IB, IE-1)
         IE = IE + 1
C                                       draw line
         CALL IMVECT (ON, ICH, 2, XV(I), YV(I), SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       done with polygon
         IF ((IBUT.LT.2) .AND. (IE.LT.400)) GO TO 50
            NV(LPY) = NV(LPY) + 1
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
         DO 110 I = NPY0,LPY
            CALL IMVECT (ON, ICH, NV(I), XV(LB), YV(LB), SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            LB = LB + NV(I)
 110        CONTINUE
         GO TO 40
C                                       Find nearest corner
 120  DLIM = 0.5
 125  LB = 1 + IE0
      DO 135 I = NPY0,LPY
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
         IX(2) = MAX (CATBLK(IICOR), MIN (CATBLK(IICOR+2), IX(2)))
         IY(2) = MAX (CATBLK(IICOR+1), MIN (CATBLK(IICOR+3), IY(2)))
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
      IF (LPOLY.GT.0) CALL BLTFIL (LPOLY, NV, XV, YV, IG2, SCRTCH, IERR)
      IF (NPY.GT.0) CALL BLTFIL (NPY, NV(LPOLY+1), XV(LIE0), YV(LIE0),
     *   IG1, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       talk to user
      IRET = 2
      WRITE (MSGTXT,1200)
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      READ (MSGTXT,1201) TEMP
      CALL CHLTOU (4, TEMP)
      IRET = 1
      IF (TEMP.EQ.'REDO') GO TO 15
      IRET = 0
      IF ((TEMP.EQ.'QUIT') .OR. (TEMP.EQ.'Q   ')) IRET = 99
C                                       Correct to image pixels
      NPY0 = LPOLY + NPY
      IF (NPY0.LE.0) GO TO 900
         IE = 0
         DO 210 I = 1,NPY0
            IB = IE + 1
            IE = IB + NV(I) - 1
            DO 210 J = IB,IE
               XV(J) = (XV(J)-CATBLK(IICOR)) * LINC(1) + CATBLK(IIWIN)
               YV(J) = (YV(J)-CATBLK(IICOR+1)) * LINC(2) +
     *            CATBLK(IIWIN+1)
               XV(J) = MAX (CATBLK(IIWIN), MIN (CATBLK(IIWIN+2), XV(J)))
               YV(J) = MAX (CATBLK(IIWIN+1), MIN (CATBLK(IIWIN+3),
     *            YV(J)))
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
 1200 FORMAT ('Type REDO to do more on this image, QUIT to stop,',
     *   ' hit return to go on')
 1201 FORMAT (A4)
 1800 FORMAT ('Reached limit of 30 polygons')
 1810 FORMAT ('Reached limit of 400 vertices')
 1900 FORMAT ('TV ROUTINE RETURNS ERROR CODE',I7)
 1901 FORMAT ('TERMINAL IO ERROR CODE',I7)
 1902 FORMAT ('Terminating at user''s request')
      END
      SUBROUTINE SLIBLS (IS0, SPCTRM, XLO, XHI, YLO, YHI, IRET)
C-----------------------------------------------------------------------
C   SLIBLS writes a SLice file containing the spectrum found.
C   Inputs:
C      IS0      I      Number of points
C      SPCTRM   R(*)   Data array
C      XLO      I      Lowest X pixel
C      XHI      I      Highest X pixel
C      YLO      I      Lowest Y pixel
C      YHI      I      Highest Y pixel
C   Outputs:
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   IS0, XLO, XHI, YLO, YHI, IRET
      REAL      SPCTRM(*)
C
      INCLUDE 'BLSUM.INC'
      CHARACTER SFILE*48
      INTEGER   ISLDAT(512), IVER, NREC, SLUN, SIND, LREC, IPT, IERR,
     *   LS0
      REAL      RSLDAT(512), RMIN, RMAX
      DOUBLE PRECISION DSLDAT(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (ISLDAT, RSLDAT, DSLDAT)
      DATA SLUN /45/
C-----------------------------------------------------------------------
      CALL FNDEXT ('SL', CATOLD, IVER)
      IVER = IVER + 1
      CALL FILL (256, 0, ISLDAT)
      LREC = 256
      LS0 = IS0
      IF (ISFQID) LS0 = 4 * IS0 - 3
      NREC = (LS0 - 1) / 256 + 2
C                                       create
      CALL EXTINI ('WRIT', 'SL', DISKIN, OLDCNO, IVER, CATOLD, SLUN,
     *   SIND, LREC, NREC, ISLDAT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATE', IVER
         GO TO 995
         END IF
C                                       update record 1
      CALL ZFIO ('READ', SLUN, SIND, 1, ISLDAT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ 1', IVER
         GO TO 990
         END IF
      ISLDAT(57) = LS0
      ISLDAT(58) = 0
      ISLDAT(59) = ISLDAT(1) + 1
      CALL ZFIO ('WRIT', SLUN, SIND, 1, ISLDAT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE 1', IVER
         GO TO 990
         END IF
C                                       inputs/parms in 2nd record
      CALL FILL (256, 0, ISLDAT)
      CALL CHR2H (6, TSKNAM, 1, ISLDAT(1))
      CALL ZDATE (ISLDAT(4))
      CALL ZTIME (ISLDAT(7))
      ISLDAT(10) = 23
      RSLDAT(11) = NLUSER
      CALL CHR2H (12, NAMEIN, 1, RSLDAT(12))
      CALL CHR2H (6, CLAIN, 1, RSLDAT(15))
      RSLDAT(17) = SEQIN
      RSLDAT(18) = DISKIN
      CALL RCOPY (14, BLC, RSLDAT(19))
      IF (NAXSKP.EQ.0) THEN
         RSLDAT(19) = XLO
         RSLDAT(20) = YLO
         RSLDAT(26) = XHI
         RSLDAT(27) = YHI
      ELSE IF (NAXSKP.EQ.1) THEN
         RSLDAT(20) = XLO
         RSLDAT(21) = YLO
         RSLDAT(27) = XHI
         RSLDAT(28) = YHI
      ELSE
         RSLDAT(19) = XLO
         RSLDAT(21) = YLO
         RSLDAT(26) = XHI
         RSLDAT(28) = YHI
         END IF
C                                       units type
      IF (ITYPE.EQ.1) THEN
         CALL CHR2H (4, 'FLUX', 1, RSLDAT(33))
      ELSE
         CALL CHR2H (4, 'AVER', 1, RSLDAT(33))
         END IF
      IF (ISFQID) THEN
         DSLDAT(19) = FRQS(1)
         RSLDAT(36) = (FRQS(IS0) - FRQS(1)) / (LS0 - 1)
         CALL FQTERP (IS0, SPCTRM, FRQS)
         END IF
C                                       min/max
      RMIN = 1.E12
      RMAX = -RMIN
      DO 10 IPT = 1,LS0
         IF (SPCTRM(IPT).NE.FBLANK) THEN
            RMAX = MAX (RMAX, SPCTRM(IPT))
            RMIN = MIN (RMIN, SPCTRM(IPT))
            END IF
 10      CONTINUE
      RSLDAT(34) = RMIN
      RSLDAT(35) = RMAX
      CALL ZFIO ('WRIT', SLUN, SIND, 2, ISLDAT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE 2', IVER
         GO TO 990
         END IF
C                                       write the data
      NREC = 2
      IPT = 1
 20   LREC = LS0 + 1 - IPT
      IF (LREC.GT.0) THEN
         LREC = MIN (LREC, 256)
         CALL RCOPY (LREC, SPCTRM(IPT), RSLDAT)
         NREC = NREC + 1
         CALL ZFIO ('WRIT', SLUN, SIND, NREC, ISLDAT, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE DATA', IVER
            GO TO 990
            END IF
         IPT = IPT + LREC
         IF (IPT.LE.LS0) GO TO 20
         END IF
C                                       close
      CALL ZCLOSE (SLUN, SIND, IRET)
C                                       Slice file created message.
      WRITE (MSGTXT,1020) IVER
      CALL MSGWRT (3)
      GO TO 999
C                                       destroy SL file
 990  CALL MSGWRT (8)
      CALL ZCLOSE (SLUN, SIND, IERR)
      CALL ZPHFIL ('SL', DISKIN, OLDCNO, IVER, SFILE, IERR)
      CALL ZDESTR (DISKIN, SFILE, IERR)
      CALL DELEXT ('SL', DISKIN, OLDCNO, 'WRIT', CATOLD, ISLDAT, IVER,
     *   IERR)
      GO TO 999
C
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' ON ',A,' SL VERS',I5)
 1020 FORMAT ('SLice file version ',I5,' created.')
      END
      SUBROUTINE FQTERP (IFQN, SLROW, FQFS)
C-----------------------------------------------------------------------
C   Smooths data from irregular grid to regular one
C   Inputs:
C      IFQN    I      Number input FQID points
C      FQFS    D(*)   Frequencies
C   In/out:
C      SLROW   R(*)   In: IFQN values
C                     Out: 4*IFQN-3 values on regular grid
C-----------------------------------------------------------------------
      INTEGER   IFQN
      REAL      SLROW(*)
      DOUBLE PRECISION FQFS(*)
C
      INCLUDE 'INCS:PMAD.INC'
      REAL      SB(MAXIMG), WT(MAXIMG), W
      INTEGER   NG(MAXIMG), NB(MAXIMG), IN, OUT, LNZI
      DOUBLE PRECISION FQFINC, FW, FR, F
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      LNZI = 4 * IFQN - 3
      FQFINC = (FQFS(IFQN) - FQFS(1)) / (LNZI - 1)
      CALL RFILL (LNZI, 0.0, SB)
      CALL RFILL (LNZI, 0.0, WT)
      CALL FILL (LNZI, 0, NG)
      CALL FILL (LNZI, 0, NB)
      FW = 2.5D0 * FQFINC
C                                       convolve
      DO 30 IN = 1,IFQN
         FR = FQFS(IN)
         F = FQFS(1) - FQFINC
         DO 20 OUT = 1,LNZI
            F = F + FQFINC
            W = ((FR - F) / FW) ** 2
            IF (W.LT.10.D0) THEN
               IF (SLROW(IN).NE.FBLANK) THEN
                  W = EXP(-W)
                  WT(OUT) = WT(OUT) + W
                  SB(OUT) = SB(OUT) + W * SLROW(IN)
                  NG(OUT) = NG(OUT) + 1
               ELSE
                  NB(OUT) = NB(OUT) + 2
                  END IF
               END IF
 20         CONTINUE
 30      CONTINUE
C                                       average
      DO 40 OUT = 1,LNZI
         IF ((WT(OUT).GT.0.0) .AND. (NB(OUT).LT.NG(OUT))) THEN
            SLROW(OUT) = SB(OUT) / WT(OUT)
         ELSE
            SLROW(OUT) = FBLANK
            END IF
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PLTBLS (NC, IPLOTS, BLNUMS, SPCTRA, IRET)
C-----------------------------------------------------------------------
C   PLTBLS makes a plot file containing IPLOTS spectra
C   Inputs:
C      NC       I         Number of points in each spectrum
C      IPLOTS   I         Number of spectra
C      BLNUMS   I(*)      Blotch numbers of these spectra
C      SPCTRA   R(NC,*)   Data to be plotted
C   Outputs:
C      IRET     I         > 0 => error
C-----------------------------------------------------------------------
      INTEGER   NC, IPLOTS, BLNUMS(*), IRET
      REAL      SPCTRA(NC,*)
C
      INCLUDE 'BLSUM.INC'
      INTEGER   I, J, NXPLOT, NYPLOT, IAPARM(5), VER, BUFFER(256), INP,
     *   IPSIZE, PTYPE, TVCHN, GRCHN, TVCORN(2), LUNPL, FINDPL, ID(3),
     *   IT(3), IPLOT, LTYPE, IERR
      REAL      PRANGE(2,4), PBLC(2), PTRC(2), XBLC(2,4), XTRC(2,4), DX,
     *   DY, YYOFF(2), XYOFF(2), XYSCL(2), TR, TI, AMULT(2), XMULT(2),
     *   SIZE, CHOUT(4), X, Y, AX(5), AY(5)
      LOGICAL   DOTV, GOOD, OFF
      CHARACTER SAVPRE(2)*5, PFILE*48, ATIME*8, ADATE*12, TEXT*132
      SAVE BUFFER, CHOUT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       find data scaling
      IF (APARM(2).LE.APARM(1)) THEN
         DO 20 J = 1,IPLOTS
            PRANGE(1,J) = 1.E20
            PRANGE(2,J) = -1.E20
            DO 10 I = 1,NC
               IF (SPCTRA(I,J).NE.FBLANK) THEN
                  PRANGE(1,J) = MIN (PRANGE(1,J), SPCTRA(I,J))
                  PRANGE(2,J) = MAX (PRANGE(2,J), SPCTRA(I,J))
                  END IF
 10            CONTINUE
 20         CONTINUE
         IF (APARM(2).LT.APARM(1)) THEN
            DO 25 J = 2,IPLOTS
               PRANGE(1,1) = MIN (PRANGE(1,1), PRANGE(1,J))
               PRANGE(2,1) = MAX (PRANGE(2,1), PRANGE(2,J))
 25            CONTINUE
            DO 30 J = 2,IPLOTS
               PRANGE(1,J) = PRANGE(1,1)
               PRANGE(2,J) = PRANGE(2,1)
 30            CONTINUE
            END IF
C                                       fixed scale
      ELSE
         DO 40 J = 1,IPLOTS
            PRANGE(1,J) = APARM(1)
            PRANGE(2,J) = APARM(2)
 40         CONTINUE
         END IF
      DO 50 J = 1,IPLOTS
         X = 0.05 * (PRANGE(2,J) - PRANGE(1,J))
         PRANGE(2,J) = PRANGE(2,J) + X
         PRANGE(1,J) = PRANGE(1,J) - X
 50      CONTINUE
C                                       Graph drawing parameters.
      PBLC(1) = 0.0
      PBLC(2) = 0.0
      PTRC(1) = 1000.0
      PTRC(2) = 1000.0
      CALL FILL (5, 1, IAPARM)
C                                       plot layout
      IF (IPLOTS.EQ.1) THEN
         NXPLOT = 1
         NYPLOT = 1
         XBLC(1,1) =PBLC(1)
         XBLC(2,1) = PBLC(2)
         XTRC(1,1) = PTRC(1)
         XTRC(2,1) = PTRC(2)
      ELSE IF (IPLOTS.EQ.4) THEN
         NXPLOT = 2
         NYPLOT = 2
         XBLC(1,1) = PBLC(1)
         XBLC(2,1) = PTRC(2) / NYPLOT
         XTRC(1,1) = PTRC(1) / NXPLOT
         XTRC(2,1) = PTRC(2)
         XBLC(1,2) = PTRC(1) / NXPLOT
         XBLC(2,2) = PTRC(2) / NYPLOT
         XTRC(1,2) = PTRC(1)
         XTRC(2,2) = PTRC(2)
         XBLC(1,3) = PBLC(1)
         XBLC(2,3) = PBLC(2)
         XTRC(1,3) = PTRC(1) / NXPLOT
         XTRC(2,3) = PTRC(2) / NYPLOT
         XBLC(1,4) = PTRC(1) / NXPLOT
         XBLC(2,4) = PBLC(2)
         XTRC(1,4) = PTRC(1)
         XTRC(2,4) = PTRC(2) / NYPLOT
         PRANGE(1,1) = MIN (PRANGE(1,1), PRANGE(1,2))
         PRANGE(2,1) = MAX (PRANGE(2,1), PRANGE(2,2))
         PRANGE(1,2) = PRANGE(1,1)
         PRANGE(2,2) = PRANGE(2,1)
         PRANGE(1,3) = MIN (PRANGE(1,3), PRANGE(1,4))
         PRANGE(2,3) = MAX (PRANGE(2,3), PRANGE(2,4))
         PRANGE(1,4) = PRANGE(1,3)
         PRANGE(2,4) = PRANGE(2,3)
      ELSE IF (IPLOTS.EQ.3) THEN
         IF (XNPLOT.GT.0) THEN
            NXPLOT = 1
            NYPLOT = 3
            XBLC(1,1) = PBLC(1)
            XBLC(2,1) = PTRC(2) - 1000.0 / NYPLOT
            XTRC(1,1) = PTRC(1)
            XTRC(2,1) = PTRC(2)
            XBLC(1,2) = PBLC(1)
            XBLC(2,2) = PBLC(2) + 1000.0 / NYPLOT
            XTRC(1,2) = PTRC(1)
            XTRC(2,2) = PTRC(2) - 1000.0 / NYPLOT
            XBLC(1,3) = PBLC(1)
            XBLC(2,3) = PBLC(2)
            XTRC(1,3) = PTRC(1)
            XTRC(2,3) = PBLC(2) + 1000.0 / NYPLOT
         ELSE
            NXPLOT = 3
            NYPLOT = 1
            XBLC(1,1) = PBLC(1)
            XBLC(2,1) = PBLC(2)
            XTRC(1,1) = PBLC(1) + 1000.0 / NXPLOT
            XTRC(2,1) = PTRC(2)
            XBLC(1,2) = PBLC(1) + 1000.0 / NXPLOT
            XBLC(2,2) = PBLC(2)
            XTRC(1,2) = PTRC(1) - 1000.0 / NXPLOT
            XTRC(2,2) = PTRC(2)
            XBLC(1,3) = PTRC(1) - 1000.0 / NXPLOT
            XBLC(2,3) = PBLC(2)
            XTRC(1,3) = PTRC(1)
            XTRC(2,3) = PTRC(2)
            PRANGE(1,1) = MIN (PRANGE(1,1), PRANGE(1,2))
            PRANGE(2,1) = MAX (PRANGE(2,1), PRANGE(2,2))
            PRANGE(1,1) = MIN (PRANGE(1,1), PRANGE(1,3))
            PRANGE(2,1) = MAX (PRANGE(2,1), PRANGE(2,3))
            PRANGE(1,2) = PRANGE(1,1)
            PRANGE(2,2) = PRANGE(2,1)
            PRANGE(1,3) = PRANGE(1,1)
            PRANGE(2,3) = PRANGE(2,1)
            END IF
C                                       2 panels
      ELSE
         IF (XNPLOT.GT.0) THEN
            NXPLOT = 1
            NYPLOT = 2
            XBLC(1,1) = PBLC(1)
            XBLC(2,1) = PTRC(2) - 1000.0 / NYPLOT
            XTRC(1,1) = PTRC(1)
            XTRC(2,1) = PTRC(2)
            XBLC(1,2) = PBLC(1)
            XBLC(2,2) = PBLC(2)
            XTRC(1,2) = PTRC(1)
            XTRC(2,2) = PTRC(2) - 1000.0 / NYPLOT
         ELSE
            NXPLOT = 2
            NYPLOT = 1
            XBLC(1,1) = PBLC(1)
            XBLC(2,1) = PBLC(2)
            XTRC(1,1) = PBLC(1) + 1000.0 / NXPLOT
            XTRC(2,1) = PTRC(2)
            XBLC(1,2) = PBLC(1) + 1000.0 / NXPLOT
            XBLC(2,2) = PBLC(2)
            XTRC(1,2) = PTRC(1)
            XTRC(2,2) = PTRC(2)
            PRANGE(1,1) = MIN (PRANGE(1,1), PRANGE(1,2))
            PRANGE(2,1) = MAX (PRANGE(2,1), PRANGE(2,2))
            PRANGE(1,2) = PRANGE(1,1)
            PRANGE(2,2) = PRANGE(2,1)
            END IF
         END IF
C                                       loop over plots
      DO 200 IPLOT = 1,IPLOTS
C                                       Offsets for current plot.
         YYOFF(1) = XBLC(1,IPLOT)
         YYOFF(2) = XBLC(2,IPLOT)
         XYOFF(1) = 0.0 + OFFCH
         XYSCL(1) = NC + 1.0 + OFFCH
         XYOFF(2) = PRANGE(1,IPLOT)
         XYSCL(2) = PRANGE(2,IPLOT)
C                                       Set up location common
         LOCNUM = 1
         ROT(LOCNUM) = 0.0
         CORTYP(LOCNUM) = 0
         LABTYP(LOCNUM) = 0
         AXTYP(LOCNUM) = 0
         TR = XYSCL(2) - XYOFF(2)
         TI = TR
         SAVPRE(2) = ' '
         CALL METSCL (LABEL, TR, SAVPRE(2), GOOD)
         AMULT(2) = TR / TI
         SAVPRE(1) = ' '
         AMULT(1) = 1.0
         CPREF(1,LOCNUM) = SAVPRE(1)
         CPREF(2,LOCNUM) = SAVPRE(2)
         XMULT(1) = AMULT(1)
         XMULT(2) = AMULT(2)
         DO 110 I = 1,2
            SIZE = 1000.0 / NXPLOT
            IF (I.EQ.2) SIZE = 1000.0 / NYPLOT
            XYSCL(I) = SIZE / (XYSCL(I) - XYOFF(I))
            TR = SIZE / XYSCL(I)
            RPLOC(I,LOCNUM) = XBLC(I,IPLOT)
            RPVAL(I,LOCNUM) = XYOFF(I) * XMULT(I)
            AXINC(I,LOCNUM) = TR * XMULT(I) /
     *         (XTRC(I,IPLOT) - XBLC(I,IPLOT))
            CTYP(I,LOCNUM) = AUNITS(I)
 110        CONTINUE
C                                       Blank bottom label.
         IF (XBLC(2,IPLOT).GT.100.) THEN
            CPREF(1,LOCNUM) = ' '
            CTYP(1,LOCNUM) = ' '
            END IF
C                                       Blank left label.
         IF (XBLC(1,IPLOT).GT.100.) THEN
            CPREF(2,LOCNUM) = ' '
            CTYP(2,LOCNUM) = ' '
            END IF
C                                       Create plot file
         IF (IPLOT.EQ.1) THEN
C                                       Update catalog header.
            VER = 0
            CALL MADDEX ('PL', DISKIN, OLDCNO, CATOLD, BUFFER, .TRUE.,
     *         'WRIT', VER, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL ZPHFIL ('PL', DISKIN, OLDCNO, VER, PFILE, IRET)
            IPSIZE = 0
            PTYPE = 0
            DOTV = .FALSE.
            CALL GINIT (DISKIN, OLDCNO, PFILE, IPSIZE, PTYPE, NPARM,
     *         XNAMEI, DOTV, TVCHN, GRCHN, TVCORN, CATOLD, BUFFER,
     *         LUNPL, FINDPL, IRET)
            IF (IRET.NE.0) GO TO 960
C                                       Not fully initialized, may make
C                                       INP too large which is okay.
            CALL RFILL (4, 0.5, CHOUT)
            CALL CHNTIC (XBLC, XTRC, INP)
            INP = MAX (INP, 3)
            LTYPE = MOD (ABS (LABEL), 100)
            IF (LTYPE.EQ.2) CHOUT(1) = 2.5
            IF (LTYPE.GT.2) CHOUT(1) = INP + 5.5
            IF (LTYPE.GT.1) CHOUT(2) = 2.0
            IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
            IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = CHOUT(4) +
     *         1.333
            CALL GINITL (PBLC, PTRC, XYRATO, CHOUT, IAPARM, BUFFER,
     *         IRET)
            IF (IRET.NE.0) GO TO 970
            WRITE (MSGTXT,1100) VER
            CALL MSGWRT (2)
            END IF
C                                       Draw border
         CALL GLTYPE (1, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GPOS (XBLC(1,IPLOT), XTRC(2,IPLOT), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (XBLC(1,IPLOT), XBLC(2,IPLOT), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (XTRC(1,IPLOT), XBLC(2,IPLOT), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (XTRC(1,IPLOT), XTRC(2,IPLOT), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (XBLC(1,IPLOT), XTRC(2,IPLOT), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         J = NCTOT / NCPER - 1
         DO 120 I = 1,J
            X = NCPER * I + 0.5 + OFFCH
            X = XYSCL(1) * (I - XYOFF(1)) + YYOFF(1)
            CALL GPOS (X, XBLC(2,IPLOT), BUFFER, IRET)
            IF (IRET.NE.0) GO TO 970
            CALL GVEC (X, XTRC(2,IPLOT), BUFFER, IRET)
            IF (IRET.NE.0) GO TO 970
 120        CONTINUE
C                                       blotch number
         CALL GPOS (XTRC(1,IPLOT), XTRC(2,IPLOT), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 999
         WRITE (TEXT,1120) BLNUMS(IPLOT)
         CALL REFRMT (TEXT, '_', I)
         DX = -I - 4.
         DY = -3.5
         CALL GICHAR (1, I, 0, DX, DY, TEXT, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
C                                       Top labels: type & name
         IF ((IPLOT.EQ.1) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
C                                       Date/time/version
            IF (LABEL.GT.1) THEN
               DX = 0.0
               DY = 0.5
               CALL GPOS (PBLC(1), PTRC(2), BUFFER, IRET)
               IF (IRET.NE.0) GO TO 970
               CALL ZDATE (ID)
               CALL ZTIME (IT)
               CALL TIMDAT (IT, ID, ATIME, ADATE)
               WRITE (TEXT,1110) VER, ADATE, ATIME
               CALL REFRMT (TEXT, '_', I)
               CALL GCHAR (I, 0, DX, DY, TEXT, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 970
               END IF
            END IF
         CALL CLAB1 (XBLC(1,IPLOT), XTRC(1,IPLOT), CHOUT, LABEL, XYRATO,
     *      .FALSE., BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         DX = 8.0 / SQRT (ABS(XNPLOT))
         DX = DX * ABS(FACTOR)
         IF (DX.LT.2.5) DX = 2.5
         DY = DX
         IF (DX/XYRATO.LT.2.5) THEN
            DY = DY * XYRATO
         ELSE
            DX = DX / XYRATO
            END IF
C                                       plot data
         CALL GLTYPE (4, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         OFF = .TRUE.
         DO 130 I = 1,NC
            IF ((SPCTRA(I,IPLOT).EQ.FBLANK) .OR.
     *         (SPCTRA(I,IPLOT).GT.PRANGE(2,IPLOT)) .OR.
     *         (SPCTRA(I,IPLOT).GT.PRANGE(2,IPLOT))) THEN
               OFF = .TRUE.
            ELSE
               X = XYSCL(1) * (I + OFFCH - XYOFF(1)) + YYOFF(1)
               Y = XYSCL(2) * (SPCTRA(I,IPLOT) - XYOFF(2)) + YYOFF(2)
               IF (OFF) THEN
                  CALL GPOS (X, Y, BUFFER, IRET)
                  IF (FACTOR.GT.0.0) OFF = .FALSE.
               ELSE
                  CALL GVEC (X, Y, BUFFER, IRET)
                  END IF
               IF (IRET.NE.0) GO TO 970
               AX(1) = X
               AY(1) = Y
               AX(2) = AX(1)
               AX(3) = AX(1)
               AX(4) = AX(1) - DX
               AX(5) = AX(1) + DX
               AY(2) = AY(1) + DY
               AY(3) = AY(1) - DY
               AY(4) = AY(1)
               AY(5) = AY(1)
               CALL PNTPLT (ISYM, AX, AY, XBLC(1,IPLOT), XTRC(1,IPLOT),
     *            .FALSE., .FALSE., BUFFER, IRET)
               IF (IRET.NE.0) GO TO 970
               CALL GPOS (X, Y, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 970
               END IF
 130        CONTINUE
 200     CONTINUE
C                                       finish plot
      CALL GFINIS (BUFFER, IRET)
      IF (IRET.GT.0) GO TO 975
      CALL HIPLOT (DISKIN, OLDCNO, VER, BUFFER, IERR)
      IRET = 0
      GO TO 999
C                                       ZPHFIL or GINIT failure.
 960  MSGTXT = 'ERROR DURING GRAPH FILE CREATION'
      CALL MSGWRT (8)
      CALL DELEXT ('PL', DISKIN, OLDCNO, 'WRIT', CATOLD, BUFFER, VER,
     *   IERR)
      GO TO 999
C                                       Try to finish partial graph
 970  MSGTXT = 'ERROR DURING GRAPHING. WILL TRY TO FINISH ' //
     *   'PARTIAL GRAPH'
      CALL MSGWRT (6)
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.GT.0) GO TO 975
      CALL HIPLOT (DISKIN, OLDCNO, VER, BUFFER, IERR)
      GO TO 999
C                                       Destroy the plot file
 975  CALL ZCLOSE (LUNPL, FINDPL, IERR)
      CALL ZDESTR (DISKIN, PFILE, IERR)
      CALL DELEXT ('PL', DISKIN, OLDCNO, 'WRIT', CATOLD, BUFFER, VER,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('PLot file version',I4,'  created.')
 1110 FORMAT ('PLot file version',I4,'__created ',A,A)
 1120 FORMAT (I3)
      END
