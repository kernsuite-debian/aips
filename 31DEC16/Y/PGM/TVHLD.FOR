LOCAL INCLUDE 'TVHLD.INC'
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:PTVC.INC'
      INTEGER   NTYPES
      PARAMETER (NTYPES = 5)
      HOLLERITH XNAMEI(3), XCLASI(2), XNAMEO(3), XCLASO(2)
      REAL      XSEQI, XDISKI, XSEQO, XDISKO, XBLC(7), XTRC(7), TBLC(7),
     *   TTRC(7), XXINC, XYINC, XPTS, XHISTO
      REAL      IRANG(2), CURANG(2), BUFF1(MABFSS)
      INTEGER   SEQIN, CNOIN, DISKIN, SEQO, CNOO, DISKO, IBLC(2),
     *   ITRC(2), IXINC, IYINC, NXI, NYI, NXT, NYT, JBUFSZ, NPOINT,
     *   ITVC(4), ITVW(4), SCRTCH(256), ICURFN, CURTYP, CATOLD(256),
     *   TVSCR(TVMOFM)
      CHARACTER NAMEIN*12, CLASIN*6, NAMOUT*12, CLASOU*6, CURFUN*2
      COMMON /INPARM/ XNAMEI, XCLASI, XSEQI, XDISKI, XNAMEO, XCLASO,
     *   XSEQO, XDISKO, XBLC, XTRC, TBLC, TTRC, XXINC, XYINC, XPTS,
     *   XHISTO
      COMMON /PARMS/ CATOLD, IRANG, CURANG, SEQIN, CNOIN, DISKIN, SEQO,
     *   CNOO, DISKO, IBLC, ITRC, IXINC, IYINC, NXI, NYI, NXT, NYT,
     *   JBUFSZ, NPOINT, ITVC, ITVW, ICURFN, CURTYP
      COMMON /PARMSC/ NAMEIN, CLASIN, NAMOUT, CLASOU, CURFUN
      COMMON /BUFFRS/ BUFF1, SCRTCH, TVSCR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DTVC.INC'
LOCAL END
      PROGRAM TVHLD
C-----------------------------------------------------------------------
C! Loads TV with histogram-equalized image interactively
C# Map Modeling Interactive TV
C-----------------------------------------------------------------------
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
C   TVHLD loads the TV with a histogram-equalized image
C   Inputs:
C      INNAME    C*12       Image name
C      INCLASS   C*6        Image class
C      INSEQ     R          Image sequence number
C      INDISK    R          Image disk number
C      TBLC      R(7)       Bottom left corner - one plane selected
C      TTRC      R(7)       Top right corner
C      TXINC     R          X pixel increment
C      TYINC     R          Y pixel increment
C      PIXRANGE  R(2)       Initial display pixel range
C      NPOINTS   R          Size of histogram to use
C-----------------------------------------------------------------------
      INCLUDE 'TVHLD.INC'
      CHARACTER PRGNAM*6
      INTEGER   IRET, NWORDS
      REAL      IMAGE(2), TVIMAG(2)
      LONGINT   PIMAGE, PTVIMG
      DATA PRGNAM /'TVHLD'/
C-----------------------------------------------------------------------
C                                       init
      CALL TVHLDI (PRGNAM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       get dynamic memory
      NWORDS = (NXI * NYI) / 1024 + 4
      CALL ZMEMRY ('GET ', PRGNAM, NWORDS, IMAGE, PIMAGE, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
         GO TO 980
         END IF
      NWORDS = (NXT * NYT) / 1024 + 4
      CALL ZMEMRY ('GET ', PRGNAM, NWORDS, TVIMAG, PTVIMG, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
         GO TO 980
         END IF
C                                       read
      CALL TVHLDG (NXI, IMAGE(1+PIMAGE), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       interact
      CALL TVHLDD (NXI, IMAGE(1+PIMAGE), NXT, TVIMAG(1+PTVIMG), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       output, history
      CALL TVHLDO (NXI, IMAGE(1+PIMAGE), IRET)
      GO TO 990
C
 980  CALL MSGWRT (8)
C
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE TVHLDI (PRGNAM, IRET)
C-----------------------------------------------------------------------
C   TVHLDI gets adverbs, finds input image, opens TV, sets parameters
C   Inputs:
C      PRGNAM   C*6   Program name
C   Outputs:
C      IRET     I     > 0 -> quit
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*(*)
      INTEGER   IRET
C
      INTEGER   NPARM, IERR, I
      CHARACTER MTYPE*2, STAT*4
      INCLUDE 'TVHLD.INC'
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
      NPARM = 46
      CALL GTPARM (PRGNAM, NPARM, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR, 'GETTING INPUT ADVERBS'
         CALL MSGWRT (8)
      ELSE IF ((NPOPS.GT.NINTRN) .OR. (NTVDEV.LE.0)) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (NPOPS.GT.NINTRN) THEN
            MSGTXT = 'NO TV IN BATCH JOBS'
         ELSE
            MSGTXT = 'YOU DO NOT HAVE A TV DEVICE'
            END IF
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       interpret name adverbs
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLASI, CLASIN)
      SEQIN = XSEQI + 0.1
      DISKIN = XDISKI + 0.1
      CALL H2CHR (12, 1, XNAMEO, NAMOUT)
      CALL H2CHR (6, 1, XCLASO, CLASOU)
      SEQO = XSEQO + 0.1
      DISKO = XDISKO + 0.1
C                                       Get CATBLK from old file.
      CNOIN = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLASIN, SEQIN, MTYPE,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLASIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'READ', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READINGIMAGE HEADER'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
      CALL COPY (256, CATBLK, CATOLD)
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), XBLC, XTRC, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL RCOPY (5, XBLC(3), XTRC(3))
      CALL RCOPY (5, XBLC(3), TBLC(3))
      CALL RCOPY (5, XBLC(3), TTRC(3))
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), TBLC, TTRC, IERR)
      IF (IERR.NE.0) GO TO 999
      IBLC(1) = XBLC(1) + 0.1
      IBLC(2) = XBLC(2) + 0.1
      ITRC(1) = XTRC(1) + 0.1
      ITRC(2) = XTRC(2) + 0.1
      TBLC(1) = MAX (TBLC(1), XBLC(1))
      TBLC(2) = MAX (TBLC(2), XBLC(2))
      TTRC(1) = MIN (TTRC(1), XTRC(1))
      TTRC(2) = MIN (TTRC(2), XTRC(2))
      IXINC = XXINC + 0.1
      IYINC = XYINC + 0.1
      IXINC = MAX (1, IXINC)
      IYINC = MAX (1, IYINC)
      XXINC = IXINC
      XYINC = IYINC
      NXI = ITRC(1) - IBLC(1) + 1
      NYI = ITRC(2) - IBLC(2) + 1
      NPOINT = XPTS + 0.1
      IF (NPOINT.LT.100) NPOINT = 8192
      NPOINT = MIN (NPOINT, 9000)
      XPTS = NPOINT
      IF (XHISTO.LT.1000.0) XHISTO = 100000.
      IF (XHISTO.GT.100000.) XHISTO = 100000.
      ICURFN = 1
      CURTYP = 2
C                                       Open TV
      CALL TVOPEN (SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING THE TV DEVICE'
         GO TO 990
         END IF
C                                       TV picture fit?
      ITVC(1) = TBLC(1) + 0.1
      ITVC(2) = TBLC(2) + 0.1
      ITVC(3) = TTRC(1) + 0.1
      ITVC(4) = TTRC(2) + 0.1
      NXT = (ITVC(3) - ITVC(1)) / IXINC + 1
      NYT = (ITVC(4) - ITVC(2)) / IYINC + 1
      IF (NXT.GT.MAXXTV(1)) THEN
         I = (NXT - MAXXTV(1) + 1) / 2
         ITVC(1) = ITVC(1) + I * IXINC
         ITVC(3) = ITVC(3) - I * IXINC
         NXT = (ITVC(3) - ITVC(1)) / IXINC + 1
         TBLC(1) = ITVC(1)
         TTRC(1) = ITVC(3)
         END IF
      IF (NYT.GT.MAXXTV(2)) THEN
         I = (NYT - MAXXTV(2) + 1) / 2
         ITVC(2) = ITVC(2) + I * IYINC
         ITVC(4) = ITVC(4) - I * IYINC
         NYT = (ITVC(4) - ITVC(2)) / IYINC + 1
         TBLC(2) = ITVC(2)
         TTRC(2) = ITVC(4)
         END IF
      I = (MAXXTV(1) - NXT) / 2
      ITVW(1) = I + 1
      ITVW(3) = ITVW(1) - 1 + NXT
      I = (MAXXTV(2) - NYT) / 2
      ITVW(2) = I + 1
      ITVW(4) = ITVW(2) - 1 + NYT
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
      CALL TVCLOS (SCRTCH, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XGAUIN: ERROR',I3,' ON ',A)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
      END
      SUBROUTINE TVHLDG (MX, IMAGE, IRET)
C-----------------------------------------------------------------------
C   Reads in the image data from the aips catalog file
C   Inputs
C      MX      I      number X pixels
C   Outputs
C      IMAGE   R(*)   image
C      IRET    I      error code
C-----------------------------------------------------------------------
      INTEGER   MX, IRET
      REAL      IMAGE(MX,*)
C
      INCLUDE 'TVHLD.INC'
      INTEGER   IDEPTH(5), IX, IY, I, BOI, LUN, LUNTMP, IND, IBIND, NX,
     *   NY
      CHARACTER PHNAME*48
C-----------------------------------------------------------------------
      DO 10 I = 1,5
         IDEPTH(I) = XBLC(I+2) + 0.1
         CATBLK(IIDEP+I-1) = IDEPTH(I)
 10      CONTINUE
      IRANG(1) = 1.E10
      IRANG(2) = -IRANG(1)
      LUN = LUNTMP (0)
      CALL ZPHFIL ('MA', DISKIN, CNOIN, 1, PHNAME, IRET)
      CALL ZOPEN (LUN, IND, DISKIN, PHNAME, .TRUE., .FALSE.,.TRUE.,
     *   IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT IMAGE'
         GO TO 990
         END IF
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, BOI, IRET)
      BOI = BOI + 1
      CALL MINIT ('READ', LUN, IND, NX, NY, IBLC, BUFF1, JBUFSZ, BOI,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT I/O TO INPUT IMAGE'
         GO TO 990
         END IF
      DO 20 IY = 1,NYI
         CALL MDISK ('READ', LUN, IND, BUFF1, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ INPUT IMAGE'
            GO TO 990
            END IF
         DO 15 IX = 1,NXI
            IMAGE(IX,IY) = BUFF1(IBIND-1+IX)
            IF (IMAGE(IX,IY).NE.FBLANK) THEN
               IRANG(1) = MIN (IRANG(1), IMAGE(IX,IY))
               IRANG(2) = MAX (IRANG(2), IMAGE(IX,IY))
               END IF
 15         CONTINUE
 20      CONTINUE
      IF (IRANG(1).GT.IRANG(2)) THEN
         IRET = 10
         MSGTXT = 'NO VALID DATA FOUND'
         GO TO 990
         END IF
      CURANG(1) = IRANG(1)
      CURANG(2) = IRANG(2)
      CALL ZCLOSE (LUN, IND, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
      CALL TVCLOS (SCRTCH, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVHLDG ERROR',I5,' ON ',A)
      END
      SUBROUTINE TVHLDD (NX, IMAGE, NT, TVIMAG, IRET)
C-----------------------------------------------------------------------
C   TVHLDD controls the interaction and menus for TVHLD
C   Inputs:
C      NX       I      Number X pixels in main image
C      IMAGE    R(*)   Input image
C      NT       I      Number X pixels in image to load to TV
C   Output:
C      TVIMAG   R(*)   TV display image
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NX, NT, IRET
      REAL      IMAGE(NX,*), TVIMAG(NT,*)
C
      INTEGER   NOPTS
      PARAMETER (NOPTS=14)
      INCLUDE 'TVHLD.INC'
C
      INTEGER   NCOL, NROWS(2), GRCHS(3), TOPSEP, SIDSEP, TIMLIM, I, J,
     *   NTITLE, TVBUTT, CHOICE, ICOLOR, LUTBUF(TVMLUT), NLEVS, QUAD,
     *   ITW(3), MTYPE, II, IC, ITYPE
      CHARACTER CHOICS(NOPTS)*12, TITLE*72, ISHELP*6, FUNCS(4)*4,
     *   TYPES(NTYPES)*3
      LOGICAL   LEAVE(NOPTS), DOIT, LINEAR
      REAL      SLOPE, OFFS, OFMBUF(TVMOFM), RPOS(2), PPOS(2), X, X0, A
      EQUIVALENCE (LUTBUF, OFMBUF)
      DATA CHOICS /'OFF ZOOM', 'OFF TRANS', 'OFF COLOR', 'TVZOOM',
     *   'TVTRANSF', 'TVPSEUDO', 'TVPHLAME', 'LOAD xx',
     *   'ADJUST LOW', 'ADJUST HIGH', 'HISTO LOG', ' ', 'EXIT', 'ABORT'/
      DATA LEAVE /NOPTS*.FALSE./
      DATA FUNCS /'LIN', 'LOG', 'SQRT', 'LOG2'/
      DATA TYPES /'LIN','LOG','1/4','SQR','3/4'/
C-----------------------------------------------------------------------
      GRCHS(1) = 1
      GRCHS(2) = 2
      GRCHS(3) = 4
      MTYPE = -1
      NCOL = 2
      NROWS(1) = 8
      NROWS(2) = 6
      SIDSEP = 20
      TOPSEP = 25
      NTITLE = 1
      TITLE = ' '
      TIMLIM = 0
      ICOLOR = 0
      ISHELP = 'TVHLD'
      LINEAR = (IRANG(1).GE.0.0) .OR. (IRANG(2).LE.0.0)
C                                       init TV
      CALL YINIT (TVSCR, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT THE TV'
         GO TO 990
         END IF
      CALL YSLECT ('ONNN', 1, ICOLOR, TVSCR, IRET)
      DO 10 I = 1,3
         J = NGRAY + GRCHS(I)
         CALL YSLECT ('ONNN', J, ICOLOR, TVSCR, IRET)
 10      CONTINUE
C                                       initial TV load
      CALL TVHLDL (NX, IMAGE, NT, TVIMAG, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'LOAD HIST EQUALIZED IMAGE'
         GO TO 990
         END IF
C                                       Menu loop
 50   CALL TLABEL (IRET)
      I = MOD (ICURFN, 4) + 1
      CHOICS(8) = 'LOAD ' // FUNCS(I)
      I = MOD (CURTYP, NTYPES) + 1
      CHOICS(11) = 'HISTO ' // TYPES(I)
      CALL TVMENU (MTYPE, NCOL, NROWS, GRCHS, TOPSEP, SIDSEP, ISHELP,
     *   CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHOICE, TVBUTT, TVSCR,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'MENU MANAGER'
         GO TO 990
         END IF
      IF (CHOICS(CHOICE).EQ.'ABORT') THEN
         IRET = -1
         GO TO 995
      ELSE IF (CHOICS(CHOICE).EQ.'EXIT') THEN
         IRET = 0
         GO TO 900
      ELSE IF (CHOICS(CHOICE).EQ.'OFF ZOOM') THEN
         TVZOOM(1) = 0
         TVZOOM(2) = MAXXTV(1) / 2
         TVZOOM(3) = MAXXTV(2) / 2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .FALSE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RESETTING TVZOOM'
            GO TO 990
            END IF
C                                       off transf
      ELSE IF (CHOICS(CHOICE).EQ.'OFF TRANS') THEN
         NLEVS = MAXINT + 1
         SLOPE = (LUTOUT+1.0) / (MAXINT+1.0)
         DO 60 I = 1,NLEVS
            LUTBUF(I) = I * SLOPE - 0.5
 60         CONTINUE
         I = 1
         ICOLOR = 7
         CALL YLUT ('WRIT', I, ICOLOR, .FALSE., LUTBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RESET LUT (B&W) TABLES'
            GO TO 990
            END IF
C                                       off color
      ELSE IF (CHOICS(CHOICE).EQ.'OFF COLOR') THEN
         I = OFMINP + 1
         CALL RFILL (I, 0.0, OFMBUF)
         NLEVS = LUTOUT + 1
         IF (I.LT.NLEVS) NLEVS = I
         OFFS = 1.0 / REAL (OFMOUT)
         SLOPE = OFFS * (OFMOUT + 1.0) / REAL(NLEVS)
         DO 65 I = 1,NLEVS
            OFMBUF(I) = I * SLOPE - OFFS
 65         CONTINUE
         I = (OFMINP + 1) / NLEVS
         J = NLEVS
         DO 70 II = 2,I
            CALL RCOPY (NLEVS, OFMBUF, OFMBUF(J+1))
            J = J + NLEVS
 70         CONTINUE
         ICOLOR = 7
         CALL YOFM ('WRIT', ICOLOR, .FALSE., OFMBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RESET COLOR TABLES'
            GO TO 990
            END IF
      ELSE IF (CHOICS(CHOICE).EQ.'TVZOOM') THEN
         CALL TVZOME (IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'ZOOMING TV IMAGE'
            GO TO 990
            END IF
      ELSE IF (CHOICS(CHOICE).EQ.'TVTRANSF') THEN
         ICOLOR = 7
         IC = 1
         ITYPE = 1
         WRITE (MSGTXT,1200)
         CALL MSGWRT (1)
         WRITE (MSGTXT,1201)
         CALL MSGWRT (1)
         WRITE (MSGTXT,1202)
         CALL MSGWRT (1)
         WRITE (MSGTXT,1203)
         CALL MSGWRT (1)
         WRITE (MSGTXT,1204)
         CALL MSGWRT (1)
         CALL IENHNS (IC, ICOLOR, ITYPE, RPOS, TVSCR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'ENHANCING TV IMAGE'
            GO TO 990
            END IF
      ELSE IF (CHOICS(CHOICE).EQ.'TVPSEUDO') THEN
         NLEVS = LUTOUT + 1
         CALL TVPSUD (NLEVS, TVSCR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'ENHANCE PSEUDO COLORS'
            GO TO 990
            END IF
      ELSE IF (CHOICS(CHOICE).EQ.'TVPHLAME') THEN
         NLEVS = LUTOUT + 1
         CALL TVFLAM (NLEVS, TVSCR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'ENHANCE FLAME COLORS'
            GO TO 990
            END IF
      ELSE IF (CHOICS(CHOICE)(:5).EQ.'LOAD ') THEN
         ICURFN = MOD (ICURFN, 4) + 1
         CALL TVHLDL (NX, IMAGE, NT, TVIMAG, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'LOAD HIST EQUALIZED IMAGE'
            GO TO 990
            END IF
      ELSE IF (CHOICS(CHOICE)(:6).EQ.'HISTO ') THEN
         CURTYP = MOD (CURTYP, NTYPES) + 1
         CALL TVHLDL (NX, IMAGE, NT, TVIMAG, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'LOAD HIST EQUALIZED IMAGE'
            GO TO 990
            END IF
      ELSE IF (CHOICS(CHOICE)(:7).EQ.'ADJUST') THEN
         J = 2
         IF (CHOICS(CHOICE).EQ.'ADJUST LOW') J = 1
         CALL YWINDO ('READ', WINDTV, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'FIND TV WINDOW'
            GO TO 990
            END IF
         CALL ZTIME (ITW)
C                                       turn on cursor
         PPOS(1) = 0.0
         PPOS(2) = 0.0
         RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
         IF (LINEAR) THEN
            X = (CURANG(J)-IRANG(1)) / (IRANG(2)-IRANG(1))
            RPOS(1) = X * (WINDTV(3) - WINDTV(1)) + WINDTV(1)
C                                       quadratic
         ELSE
            A = -IRANG(2) / IRANG(1)
            X0 = (WINDTV(3)-A*WINDTV(1) - SQRT(A)*(WINDTV(3)-WINDTV(1)))
     *         / (1.0 - A)
            A = IRANG(2) / (WINDTV(3)**2 - 2.0*WINDTV(3)*X0 + X0**2)
            IF (CURANG(J).GE.0.0) THEN
               RPOS(1) = SQRT (CURANG(J) / A) + X0
            ELSE
               RPOS(1) = -SQRT(-CURANG(J) / A) + X0
               END IF
            END IF
         IF (RPOS(1).LT.WINDTV(1)) RPOS(1) = WINDTV(1)
         IF (RPOS(1).GT.WINDTV(3)) RPOS(1) = WINDTV(3)
         CALL YCURSE ('ONNN', .FALSE., .FALSE., RPOS, QUAD, TVBUTT,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'TURN ON CURSOR'
            GO TO 990
            END IF
 80      CALL YCURSE ('READ', .FALSE., .FALSE., RPOS, QUAD, TVBUTT,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'TURN ON CURSOR'
            GO TO 990
            END IF
         CALL DLINTR (RPOS, TVBUTT, PPOS, ITW, DOIT)
         IF (DOIT) THEN
            IF (LINEAR) THEN
               X = (RPOS(1) - WINDTV(1)) / (WINDTV(3) - WINDTV(1))
               X = IRANG(1) + X * (IRANG(2) - IRANG(1))
            ELSE IF (RPOS(1).GE.X0) THEN
               X = A * (RPOS(1) - X0)**2
            ELSE
               X = -A * (RPOS(1) - X0)**2
               END IF
            IF ((J.EQ.1) .AND. (X.LT.CURANG(2))) THEN
               CURANG(J) = X
               CALL TVHLDL (NX, IMAGE, NT, TVIMAG, IRET)
            ELSE IF ((J.EQ.2) .AND. (X.GT.CURANG(1))) THEN
               CURANG(J) = X
               CALL TVHLDL (NX, IMAGE, NT, TVIMAG, IRET)
               END IF
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'LOAD HIST EQUALIZED IMAGE'
               GO TO 990
               END IF
            END IF
         IF (TVBUTT.LE.0) GO TO 80
         END IF
      GO TO 50
C                                       clean up a little
 900  DO 910 I = 1,4
         J = NGRAY + I
         CALL YSLECT ('OFFF', J, ICOLOR, TVSCR, IRET)
 910     CONTINUE
      GO TO 995
C
 990  CALL MSGWRT (8)
C
 995  CALL TVCLOS (SCRTCH, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVHLDD ERROR',I4,' ON ',A)
 1200 FORMAT ('Cursor X position controls intercept')
 1201 FORMAT ('Cursor Y position controls slope')
 1202 FORMAT ('Hit buttons A or B to turn plot off or back on')
 1203 FORMAT ('Hit button C to reverse sign of slope')
 1204 FORMAT ('Hit button D to exit')
      END
      SUBROUTINE TVHLDL (NX, IMAGE, NT, TVIMAG, IRET)
C-----------------------------------------------------------------------
C   TVHLDL computes the hist equal image and loads it to the TV
C   Inputs:
C      NX       I      Number X pixels in main image
C      IMAGE    R(*)   Input image
C      NT       I      Number X pixels in image to load to TV
C   Output:
C      TVIMAG   R(*)   TV display image
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NX, NT, IRET
      REAL      IMAGE(NX,*), TVIMAG(NT,*)
C
      INCLUDE 'TVHLD.INC'
      INTEGER   INCS, ICH, NPIX, NROW, IY, IYC
      REAL      RANGE(2)
      CHARACTER FUNCS(4)*2, FUNC*2
      DATA FUNCS /'LN','LG','SQ','L2'/
      DATA RANGE /0.0, 1.0/
C-----------------------------------------------------------------------
C                                       compute the TV image
      CALL TVHLDC (.TRUE., NX, IMAGE, NT, TVIMAG)
C                                       load TV
      ICH = 1
      CALL YZERO (ICH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'ZERO IMAGE PLANE'
         GO TO 990
         END IF
      NPIX = ITVW(3) - ITVW(1) + 1
      NROW = ITVW(4) - ITVW(2) + 1
      FUNC = FUNCS(ICURFN)
      CALL COPY (4, ITVC, CATBLK(IIWIN))
      CALL COPY (4, ITVW, CATBLK(IICOR))
      CALL CHR2H (2, FUNC, 1, CATH(IITRA))
      RANGE(1) = -0.5 / NPOINT
      RANGE(2) = 1.0 - RANGE(1)
      CATR(IRRAN) = RANGE(1)
      CATR(IRRAN+1) = RANGE(2)
      CALL YCWRIT (ICH, ITVW, CATBLK, TVSCR, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE IMAGE CATALOG'
         GO TO 990
         END IF
      IYC = ITVW(2) - 1
      DO 50 IY = 1,NROW
         IYC = IYC + 1
         CALL ISCALE (FUNC, MAXINT, RANGE, NPIX, INCS, TVIMAG(1,IY),
     *      TVSCR)
         CALL YIMGIO ('WRIT', ICH, ITVW, IYC, 0, NPIX, TVSCR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE ROW TO TV'
            GO TO 990
            END IF
 50      CONTINUE
C                                       write title
      CALL TLABEL (IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVHLDL ERROR',I4,' ON ',A)
      END
      SUBROUTINE TLABEL (IRET)
C-----------------------------------------------------------------------
C   Does the top line label
C   Outputs:
C      IRET
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'TVHLD.INC'
      CHARACTER LINE*72, TYPES(NTYPES)*3
      INTEGER   IX, IY, IGR, J, JTRIM
      DATA TYPES /'LIN','LOG','1/4','SQR','3/4'/
C-----------------------------------------------------------------------
C                                       write title
      CALL YWINDO ('READ', WINDTV, IRET)
      IX = WINDTV(1) + 29
      IY = WINDTV(4) - 17 - CSIZTV(2)
      WRITE (LINE,1050) TYPES(CURTYP), CURANG
      IGR = 4 + NGRAY
      CALL YZERO (IGR, IRET)
      J = JTRIM (LINE)
      CALL IMCHAR (IGR, IX, IY, 0, 0, LINE(:J), TVSCR, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE TITLE LINE ON TV'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVHLDL ERROR',I4,' ON ',A)
 1050 FORMAT ('HISTO ',A,'  MIN ',F13.8,'  TO MAX ',F11.6)
      END
      SUBROUTINE TVHLDC (FORTV, NX, IMAGE, NT, TVIMAG)
C-----------------------------------------------------------------------
C   TVHLDC actually computes the histogram and then the histogram
C   equalized image
C   Inputs:
C      FORTV    L      T => use TV increments, size, for TVIMAG
C                      F => increment 1, input image -> output
C      NX       I      X dimension of image
C      IMAGE    R(*)   Input image data
C      NT       I      X dimension of TVIMAG
C   Output:
C      TVIMAG   R      Output image
C      IRET     I      > 0 => failure
C-----------------------------------------------------------------------
      LOGICAL   FORTV
      INTEGER   NX, NT
      REAL      IMAGE(NX,*), TVIMAG(NT,*)
C
      INCLUDE 'TVHLD.INC'
      INTEGER   NHISTO, IY, IX, J, I, L, IX1, IY1
      REAL      HISTOG(100000), X, Y, VALS(9020), Z, RL, SUM
      DOUBLE PRECISION NHIST
C-----------------------------------------------------------------------
C                                       compute histogram
      NHISTO = XHISTO + 0.1
      NHISTO = MIN (100000, MAX (1000, NHISTO))
      NPOINT = XPTS + 0.1
      NPOINT = MAX (100, MIN (9000, NPOINT))
      CALL RFILL (NHISTO, 0.0, HISTOG)
      NHIST = 0.0D0
      Y = NHISTO / (CURANG(2) - CURANG(1))
      DO 20 IY = 1,NYI
         DO 10 IX = 1,NXI
            IF (IMAGE(IX,IY).NE.FBLANK) THEN
               IF ((IMAGE(IX,IY).GE.CURANG(1)) .AND.
     *            (IMAGE(IX,IY).LE.CURANG(2))) THEN
                  X = Y * (IMAGE(IX,IY) - CURANG(1)) + 1.000001
                  J = X
                  IF ((J.GE.1) .AND. (J.LE.NHISTO)) HISTOG(J) =
     *               HISTOG(J) + 1.0
                  END IF
               END IF
 10         CONTINUE
 20      CONTINUE
C                                       logarithmic
      IF (CURTYP.EQ.2) THEN
         DO 30 J = 1,NHISTO
            HISTOG(J) = LOG (HISTOG(J)+1.0)
 30         CONTINUE
      ELSE IF (CURTYP.EQ.3) THEN
         DO 31 J = 1,NHISTO
            HISTOG(J) = HISTOG(J) ** 0.25
 31         CONTINUE
      ELSE IF (CURTYP.EQ.4) THEN
         DO 32 J = 1,NHISTO
            HISTOG(J) = SQRT (HISTOG(J))
 32         CONTINUE
      ELSE IF (CURTYP.EQ.5) THEN
         DO 33 J = 1,NHISTO
            HISTOG(J) = HISTOG(J) ** 0.75
 33         CONTINUE
         END IF
C                                       total count
      NHIST = 0.0D0
      DO 40 J = 1,NHISTO
         NHIST = NHIST + HISTOG(J)
 40      CONTINUE
C                                       sum histogram to
C                                       make intensity ranges
      IF (NPOINT.GT.NHIST/3) NPOINT = NHIST/3
      Y = NHISTO / (CURANG(2) - CURANG(1))
      X = NHIST / NPOINT
      Z = 0.0
      L = 1
      RL = 0.0
      DO 80 I = 1,NPOINT
         SUM = 0.0
         DO 60 J = L,NHISTO
            SUM = SUM + HISTOG(J)
            IF (SUM.GE.X) GO TO 70
            L = L + 1
 60         CONTINUE
         VALS(I+1) = CURANG(2)
         GO TO 80
C                                       get value
 70      Z = SUM - X
         RL = L - (Z / HISTOG(L)) * (L - RL)
         VALS(I+1) = CURANG(1) + RL / Y
         HISTOG(L) = Z
 80      CONTINUE
      VALS(1) = CURANG(1)
      VALS(NPOINT+2) = CURANG(2)
      VALS(NPOINT+3) = CURANG(2) + 1.E9
C                                       TV image computation
      IF (FORTV) THEN
         IY1 = TBLC(2) - XBLC(2) + 1.01
         IX1 = TBLC(1) - XBLC(1) + 1.01
         DO 120 IY = 1,NYT
            I = IY1 + (IY-1) * IYINC
            DO 110 IX = 1,NXT
               J = IX1 + (IX - 1) * IXINC
               TVIMAG(IX,IY) = IMAGE(J,I)
 110           CONTINUE
            CALL HISMAK (NXT, NPOINT, VALS, CURANG, TVIMAG(1,IY),
     *         TVIMAG(1,IY))
 120        CONTINUE
C                                       image itself
      ELSE
         DO 150 IY = 1,NYI
            CALL HISMAK (NXI, NPOINT, VALS, CURANG, IMAGE(1,IY),
     *         IMAGE(1,IY))
 150        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE HISMAK (LROW, NVALS, VALS, PIXRNG, DATA, RESULT)
C-----------------------------------------------------------------------
C   converts real intensities into histogram equalized ones
C   Inputs:
C      LROW     I      Number pixels in row
C      NVALS    I      Number values in VALS
C      VALS     R(*)   Break pints in intensity between steps
C      PIXRNG   R(2)   Intensity range
C      DATA     R(*)   Input row, magic value blanked.
C   Output:
C      RESULT   R(*)   Output row.
C-----------------------------------------------------------------------
      INTEGER   LROW, NVALS
      REAL      VALS(*), PIXRNG(2), DATA(*), RESULT(*)
C
      REAL      X, Y1, Y2, Y
      INTEGER   I, J, MVALS, J1, J2
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      X = 0.5 / NVALS
      MVALS = NVALS + 2
      DO 50 I = 1,LROW
         IF (DATA(I).EQ.FBLANK) THEN
            RESULT(I) = FBLANK
         ELSE IF (DATA(I).LE.PIXRNG(1)) THEN
            RESULT(I) = -X
         ELSE IF (DATA(I).GE.PIXRNG(2)) THEN
            RESULT(I) = 1.0 + X
         ELSE
            J1 = 1
            J2 = MVALS
            J = MVALS / 2
            IF (DATA(I).GE.VALS(J)) THEN
               J1 = J
               J = (3 * MVALS) / 4
               IF (DATA(I).GE.VALS(J))THEN
                  J1 = J
                  J = (7 * MVALS) / 8
                  IF (DATA(I).GE.VALS(J)) THEN
                     J1 = J
                     J = (15 * MVALS) / 16
                     IF (DATA(I).GE.VALS(J)) J1 = J
                  ELSE
                     J = (13 * MVALS) / 16
                     IF (DATA(I).GE.VALS(J)) J1 = J
                     END IF
               ELSE
                  J = (5 * MVALS) / 8
                  IF (DATA(I).GE.VALS(J)) THEN
                     J1 = J
                     J = (11 * MVALS) / 16
                     IF (DATA(I).GE.VALS(J)) J1 = J
                  ELSE
                     J = (9 * MVALS) / 16
                     IF (DATA(I).GE.VALS(J)) J1 = J
                     END IF
                  END IF
            ELSE
               J = MVALS / 4
               IF (DATA(I).GE.VALS(J)) THEN
                  J1 = J
                  J = (3 * MVALS) / 8
                  IF (DATA(I).GE.VALS(J)) THEN
                     J1 = J
                     J = (7 * MVALS) / 16
                     IF (DATA(I).GE.VALS(J)) J1 = J
                  ELSE
                     J = (5 * MVALS) / 16
                     IF (DATA(I).GE.VALS(J)) J1 = J
                     END IF
               ELSE
                  J = MVALS / 8
                  IF (DATA(I).GE.VALS(J)) THEN
                     J1 = J
                     J = (3 * MVALS) / 16
                     IF (DATA(I).GE.VALS(J)) J1 = J
                  ELSE
                     J = MVALS / 16
                     IF (DATA(I).GE.VALS(J)) J1 = J
                     END IF
                  END IF
               END IF
            DO 30 J = J1,MVALS
               IF (DATA(I).LT.VALS(J)) GO TO 40
 30            CONTINUE
            J = MVALS + 1
 40         Y1 = VALS(J-1)
            Y2 = VALS(J)
            Y = J - 2 + (DATA(I) - Y1) / (Y2 - Y1)
            RESULT(I) = Y / (NVALS + 1.0)
            END IF
 50      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE TVHLDO (NX, IMAGE, IRET)
C-----------------------------------------------------------------------
C   TVHLDO computes a final image, creates a catalog file, and writes
C   it out
C   Inputs:
C      NX      I      Number X pixels in IMAGE
C   In/Out:
C      IMAGE   R(*)   Image values in, his equal values out
C   Outputs
C      IRET    I      Failure code
C-----------------------------------------------------------------------
      INTEGER   NX, IRET
      REAL      IMAGE(NX,*)
C
      INCLUDE 'TVHLD.INC'
      INTEGER   I, J, LUN, IND, LUNTMP, IBIND, HLUN1, HLUN2, IWIN(7)
      REAL      X, RMIN, RMAX
      LOGICAL   ISBLNK, T
      CHARACTER CBLANK*6, PHNAME*48, NOTTYP*2, HILINE*72
      DATA CBLANK, NOTTYP /' ', ' '/
      DATA T /.TRUE./
      DATA HLUN1, HLUN2 /27, 28/
C-----------------------------------------------------------------------
C                                       compute final image
      CALL TVHLDC (.FALSE., NX, IMAGE, NX, IMAGE)
C                                       build output header
      CALL COPY (256, CATOLD, CATBLK)
      CALL SUBHDR (XBLC, XTRC, 1.0, 1.0)
      CALL MAKOUT (NAMEIN, CLASIN, SEQIN, CBLANK, NAMOUT, CLASOU, SEQO)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASOU, KHIMCO, CATH(KHIMN))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHIMN))
      CATBLK(KIIMS) = SEQO
C                                       max/min, blanked
      ISBLNK = .FALSE.
      RMIN = 1.E10
      RMAX = -RMIN
      DO 20 J = 1,NYI
         DO 10 I = 1,NXI
            X = IMAGE(I,J)
            IF (X.EQ.FBLANK) THEN
               ISBLNK = .TRUE.
            ELSE
               RMIN = MIN (RMIN, X)
               RMAX = MAX (RMAX, X)
               END IF
 10         CONTINUE
 20      CONTINUE
      CATR(KRDMN) = RMIN
      CATR(KRDMX) = RMAX
      CATR(KRBLK) = 0.0
      IF (ISBLNK) CATR(KRBLK) = FBLANK
C                                       Set output units.
      CALL CHR2H (8, 'HISTEQAL', 1, CATH(KHBUN))
C                                       doit
      CALL MCREAT (DISKO, CNOO, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CATALOG FILE CREATION FAILED'
         GO TO 990
         END IF
      SEQO = CATBLK(KIIMS)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CNOO
      FRW(NCFILE) = 2
C                                       open the IO
      CALL ZPHFIL ('MA', DISKO, CNOO, 1, PHNAME, IRET)
      LUN = LUNTMP (0)
      CALL ZOPEN (LUN, IND, DISKO, PHNAME, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT IMAGE FILE'
         GO TO 990
         END IF
      IWIN(1) = 1
      IWIN(2) = 1
      IWIN(3) = NXI
      IWIN(4) = NYI
      CALL MINIT ('WRIT', LUN, IND, NXI, NYI, IWIN, BUFF1, JBUFSZ, 1,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT I/O TO OUTPUT IMAGE'
         GO TO 990
         END IF
      DO 50 J = 1,NYI
         CALL MDISK ('WRIT', LUN, IND, BUFF1, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE ROW OF OUTPUT IMAGE'
            GO TO 990
            END IF
         CALL RCOPY (NXI, IMAGE(1,J), BUFF1(IBIND))
 50      CONTINUE
      CALL MDISK ('FINI', LUN, IND, BUFF1, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE LAST ROW OF OUTPUT IMAGE'
         GO TO 990
         END IF
      CALL ZCLOSE (LUN, IND, IRET)
      FRW(NCFILE) = 1
C                                       Copy any header keywords
      CALL KEYCOP (DISKIN, CNOIN, DISKO, CNOO, IRET)
C                                       HI file
      CALL HIINIT (2)
      CALL HISCOP (HLUN1, HLUN2, DISKIN, DISKO, CNOIN, CNOO, CATBLK,
     *   SCRTCH, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
      CALL HENCO1 (TSKNAM, NAMEIN, CLASIN, SEQIN, DISKIN, HLUN2, BUFF1,
     *   IRET)
      IF (IRET.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, NAMOUT, CLASOU, SEQO, DISKO, HLUN2, BUFF1,
     *   IRET)
      IF (IRET.NE.0) GO TO 100
      DO 60 I = 1,7
         IWIN(I) = XBLC(I) + 0.1
 60      CONTINUE
      WRITE (HILINE,1060) TSKNAM, 'BLC', IWIN
      CALL HIADD (HLUN2, HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
      DO 65 I = 1,7
         IWIN(I) = XTRC(I) + 0.1
 65      CONTINUE
      WRITE (HILINE,1060) TSKNAM, 'TRC', IWIN
      CALL HIADD (HLUN2, HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
      WRITE (HILINE,1065) TSKNAM, NPOINT
      CALL HIADD (HLUN2, HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
      WRITE (HILINE,1070) TSKNAM, CURANG
      CALL HIADD (HLUN2, HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
 100  CALL HICLOS (HLUN2, T, BUFF1, IRET)
C                                       Copy CC files and others
      CALL ALLTAB (0, NOTTYP, HLUN1, HLUN2, DISKIN, DISKO, CNOIN,
     *   CNOO, CATBLK, SCRTCH, BUFF1, IRET)
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVHLDO ERROR',I4,' ON ',A)
 1060 FORMAT (A6,A,' =',2(I6,','),4(I5,','),I5)
 1065 FORMAT (A6,'NPOINTS =',I7,'  / levels in histogram equalization')
 1070 FORMAT (A6,'PIXRANGE=',1PE11.4,',',1PE11.4,
     *   '  / final intensity range')
      END

