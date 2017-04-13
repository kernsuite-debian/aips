LOCAL INCLUDE 'XSMTH.INC'
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, OPCODE*4,
     *   HISCRD(5)*64
      HOLLERITH OLDH(256), XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2),
     *   XOPCOD
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7), XPOINT,
     *   DPARM(10), TAB(5000), WCUTOF, SUPRAD, BUFF1(MABFSS),
     *   BUFF2(MABFSS), OLD4(256)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD(256), NUMHIS, JBUFSZ, ICODE, IXDIV, SCRTCH(256)
      DOUBLE PRECISION OLD8(128)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOU,
     *   XSEQO, XDISKO, BLC, TRC, XPOINT, XOPCOD, DPARM
      COMMON /PARMS/ TAB, WCUTOF, SUPRAD,
     *   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   JBUFSZ, ICODE, IXDIV, NUMHIS
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, OPCODE, HISCRD
      COMMON /BUFRS/ BUFF1, BUFF2, SCRTCH
      COMMON /OLDHDR/ CATOLD
      EQUIVALENCE (CATOLD, OLD4, OLDH, OLD8)
LOCAL END
      PROGRAM XSMTH
C-----------------------------------------------------------------------
C! XSMTH smooths image x-axis by convolution or resampling.
C# Map-util Spectral
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
C   XSMTH is an AIPS task to smooth the x axis by convolution or
C   interpolation with resampling.  It began from TAFFY by W. D.
C   Cotton, March 1983.
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
C      NPOINTS        XPOINT        Number of x axis points out.
C      OPCODE         OPCODE        'SMOT' => convolve (default)
C                                   'INTE' => polynomial interpolate
C      DPARM(10)      DPARM         (1) type of function
C                                   (2) diameter of func in cells
C                                   (3) support of func in cells
C   Programmer Eric W. Greisen  May 1983.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'XSMTH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DEVI.INC'
      DATA PRGM /'XSMTH '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL XSMTIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL XSMTMA (IRET)
      IF (IRET.EQ.0) CALL XSMTHI
C                                       Close down files, etc.
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE XSMTIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   XSMTIN gets input parameters for XSMTH and creates an output file.
C   Inputs:  PRGN    C*6       Program name
C   Output:  IRET    I         Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER  STAT*4, PRGN*6, MTYPE*2
      INTEGER  IRET, IERR, NPARM, IROUND
      INCLUDE 'XSMTH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 40
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRTCH, IERR)
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
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
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
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'READ', SCRTCH, IERR)
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
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *    SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATOLD(KIDIM), CATOLD(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get user modification to CATBLK
      IRET = 4
      CALL XSMHED (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output file.
      NEWCNO = 1
      IRET = 4
      CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 2
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
C                                       copy most keywords
      CALL KEYPCP (DISKIN, OLDCNO, DISKO, NEWCNO, 0, ' ', IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XSMTIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE XSMTMA (IRET)
C-----------------------------------------------------------------------
C   XSMTMA sends image one row at a time to one of the smoothing
C   routineS and then writes the modified data.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER IFILE*48
      INTEGER   IRET, IROUND, LUNI, LUNO, RBP, NYI, NXI, WINI(4), NXO,
     *   NYO, WINO(4), BOI, BOO, BPI, BPO, LIM2, LIM3, LIM4, LIM5,
     *   LIM6, LIM7, I1, I2, I3, I4, I5, I6, I7, IPOS(7), CORN(7),
     *   BOTEMP, KOFF, LIMO, LIMIT, IBIND, OBIND, INDI, INDO, LIM1
      REAL      OUTMAX, OUTMIN
      LOGICAL   T, F, BLNKD
      INCLUDE 'XSMTH.INC'
      REAL      XDATA(MAXIMG)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI, LUNO /16,17/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Open some output file
      RBP = 2
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
C                                       Use actual f.p. output
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
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
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
      BPI = 2
      BPO = 2
      OUTMAX = -1.0E20
      OUTMIN = 1.0E20
      BLNKD = F
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
      LIM2 = MAX (1, CATBLK(KINAX+1))
      LIM3 = MAX (1, CATBLK(KINAX+2))
      LIM4 = MAX (1, CATBLK(KINAX+3))
      LIM5 = MAX (1, CATBLK(KINAX+4))
      LIM6 = MAX (1, CATBLK(KINAX+5))
      LIM7 = MAX (1, CATBLK(KINAX+6))
      KOFF = 0
      CORN(7) = 1
      LIMO = CATBLK(KINAX) - 1
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
                  DO 300 I3 = 1,LIM3
      IPOS(3) = BLC(3) + I3 - 0.9
      CORN(3+KOFF) = I3
C                                       Init. files, first input.
      CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3), BOTEMP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1099) IRET
         GO TO 990
         END IF
      BOI = BOTEMP + 1
      CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF1, JBUFSZ,
     *   BOI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) 'READ', IRET
         GO TO 990
         END IF
C                                       Init output file.
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORN(3), BOTEMP, IRET)
      BOO = BOTEMP + 1
      CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2, JBUFSZ,
     *   BOO, IRET)
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
C                                       Copy/scale to buffer.
         DO 165 I1 = 1,LIM1
            XDATA(I1) = BUFF1(IBIND+I1-1)
 165        CONTINUE
C                                       Write.
         CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
         OBIND = OBIND - 1
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1120) 'WRIT', IRET
            GO TO 990
            END IF
C                                       Call function
         OBIND = OBIND + 1
         IF (ICODE.EQ.1) CALL XSMCON (IPOS, XDATA, BUFF2(OBIND), IRET)
         IF (ICODE.EQ.2) CALL XSMINT (IPOS, XDATA, BUFF2(OBIND), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1180) IRET
            GO TO 990
            END IF
C                                       Check max, min, blanking.
         LIMIT = OBIND + LIMO
         DO 200 I1 = OBIND,LIMIT
            BLNKD = BLNKD .OR. (BUFF2(I1).EQ.FBLANK)
            IF (BUFF2(I1).EQ.FBLANK) GO TO 200
               OUTMAX = MAX (OUTMAX, BUFF2(I1))
               OUTMIN = MIN (OUTMIN, BUFF2(I1))
 200           CONTINUE
 250     CONTINUE
C                                       Dump plane to output.
C                                       Flush buffer.
      CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1120) 'FINI', IRET
         GO TO 990
         END IF
C                                       Update CATBLK.
         CATR(KRDMX) = OUTMAX
         CATR(KRDMN) = OUTMIN
         CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRTCH,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1260) IRET
            GO TO 990
            END IF
 300     CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Mark blanking in CATBLK.
      CATR(KRBLK) = 0.0
      IF (BLNKD) CATR(KRBLK) = FBLANK
      CATR(KRDMN) = OUTMIN
      CATR(KRDMX) = OUTMAX
C                                       Final call to functions
      IPOS(1) = -1
      IF (ICODE.EQ.1) CALL XSMCON (IPOS, XDATA, BUFF2(OBIND), IRET)
      IF (ICODE.EQ.2) CALL XSMINT (IPOS, XDATA, BUFF2(OBIND), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1180) IRET
         GO TO 990
         END IF
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XSMTMA: ERROR',I5,' OPENING INPUT FILE')
 1010 FORMAT ('XSMTMA: ERROR',I5,' OPENING OUTPUT FILE')
 1099 FORMAT ('XSMTMA: COMOF3 ERROR',I5)
 1100 FORMAT ('XSMTMA: INIT-FOR-',A4,' ERROR',I5)
 1120 FORMAT ('XSMTMA: ',A4,' ERROR',I5)
 1180 FORMAT ('XSMTMA: SMOOTHING FUNCTION ERROR',I5)
 1260 FORMAT ('XSMTMA: CATIO ERROR',I5,' UPDATING CATBLK')
      END
      SUBROUTINE XSMTHI
C-----------------------------------------------------------------------
C   XSMTHI converts output to integer, copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER LABEL*8, LINE*80, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR, I
      LOGICAL   T
      INCLUDE 'XSMTH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'  '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       BLC
      WRITE (MSGTXT,2000) TSKNAM, BLC
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       TRC
      WRITE (MSGTXT,2001) TSKNAM, TRC
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       OPCODE
      WRITE (MSGTXT,2002) TSKNAM, OPCODE
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Add any user supplied history.
      IF (NUMHIS.LE.0) GO TO 20
         WRITE (LABEL,1010) TSKNAM
         LINE(1:8) = LABEL(1:8)
         DO 15 I = 1,NUMHIS
            LINE(9:72) = HISCRD(I)(1:64)
            CALL HIADD (LUN2, LINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 20
 15         CONTINUE
C                                       Close HI file
 20   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XSMTHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'  ')
 2000 FORMAT (A6,' BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,' TRC =',6(F6.0,','),F6.0)
 2002 FORMAT (A6,' OPCODE = ''',A4,'''')
      END
      SUBROUTINE XSMHED (IRET)
C-----------------------------------------------------------------------
C   XSMHED modifies the output header for subimaging and change of
C   x-axis scale.  It also pre-computes parameters for convolution
C   including the gridded function values and for interpoolation
C   including the output x-coordinate positions.
C   Input:
C      CATBLK(256)    I     Output catalog header, also CATR, CATD
C      CATOLD(256)    I     Input catalog header, also OLD4, OLD8
C   Output:
C      CATBLK(256)    I     Modified output catalog header.
C      IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER CODES(2)*4, POSTYP(10)*4, CHTM12*12
      INTEGER   IRET, I, NCODE, TABSIZ, IT, N, IROUND
      LOGICAL   EQUAL
      REAL      WIDTHS(4), SUPS(4), XMUL, FX, W, X, Y
      INCLUDE 'XSMTH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA WIDTHS /4.0, 2.0, 2.0, 3.0/
      DATA SUPS /1.0, 3.0, 1.0, 4.0/
      DATA NCODE, CODES /2, 'SMOT','INTE'/
      DATA TABSIZ /5000/
      DATA POSTYP /'RA  ','DEC ', 'RA--','DEC-', 'LL  ','MM  ',
     *   'GLON','GLAT', 'ELON','ELAT'/
C-----------------------------------------------------------------------
      IRET = 8
C                                       Set default OPCODE
      ICODE = 1
      IF (NCODE.LE.0) GO TO 20
         DO 10 I = 1,NCODE
            ICODE = I
            IF (OPCODE.EQ.CODES(I)) GO TO 20
 10         CONTINUE
C                                       Default OPCODE is first.
         OPCODE = CODES(1)
         ICODE = 1
C                                       Convolution: parms & tables
 20   IF (ICODE.NE.1) GO TO 80
         IF (XPOINT.LE.2.0) XPOINT = TRC(1) - BLC(1) + 1.0
         IF (XPOINT.GT.MAXIMG) XPOINT = MAXIMG
         IT = IROUND(DPARM(1)) + 1
         IF ((IT.LT.1) .OR. (IT.GT.4)) IT = 1
         DPARM(1) = IT - 1
         IF ((DPARM(2).LT.0.1) .OR. (DPARM(2).GT.(TRC(1)-BLC(1)+1.)/3.))
     *      DPARM(2) = WIDTHS(IT)
         IF ((DPARM(3).LT.DPARM(2)) .OR. (DPARM(3).GT.4.*SUPS(IT)*
     *      DPARM(2))) DPARM(3) = SUPS(IT) * DPARM(2)
         SUPRAD = DPARM(3) / 2.0
         IXDIV = (TABSIZ - 1) / (SUPRAD + 0.1)
         IF (IXDIV.GT.500) IXDIV = 500
         WCUTOF = DPARM(4)
         IF (WCUTOF.LE.0.0) WCUTOF = 0.33
         IF (WCUTOF.GE.1.0) WCUTOF = 0.95
         CALL RFILL (TABSIZ, 0.0, TAB)
         N = 1 + SUPRAD*IXDIV + 0.99
         FX = 2.0 / DPARM(2)
         Y = 1.0 / IXDIV
         TAB(1) = 1.0
C                                       Compute look-up tables
         IF (IT.NE.1) GO TO 30
            DO 25 I = 2,N
               X = Y * (I - 1)
               TAB(I) = MAX (0.0, 1.0-FX*X)
 25            CONTINUE
 30      IF (IT.NE.2) GO TO 40
            FX = -LOG(2.0) * FX * FX
            DO 35 I = 2,N
               X = Y * (I - 1)
               TAB(I) = EXP (FX * X * X)
 35            CONTINUE
 40      IF (IT.NE.3) GO TO 50
            DO 45 I = 2,N
               X = Y * (I - 1)
C                                       whole array 1.0 except last
C                                       GvM, Oct/26/1992
                  TAB(I) = 1.0
                  IF (I.EQ.N) TAB(I) = 0.5
 45            CONTINUE
 50      IF (IT.NE.4) GO TO 60
            FX = 3.14159 * FX
            DO 55 I = 2,N
               X = Y * (I - 1) * FX
               TAB(I) = SIN(X) / X
 55            CONTINUE
C                                       Normalize integral
 60      W = -TAB(1)
         DO 65 I = 1,N,IXDIV
            W = W + 2. * TAB(I)
 65         CONTINUE
         DO 70 I = 1,N
            TAB(I) = TAB(I) / W
 70         CONTINUE
         GO TO 200
C                                       Interpolation: parms
 80   CONTINUE
         IT = IROUND (DPARM(1))
         IT = MIN (3, MAX (0, IT))
         DPARM(1) = IT
         IF (XPOINT.LE.2.0) XPOINT = (TRC(1) - BLC(1)) * 2. + 1.0
         IF (XPOINT.GT.MAXIMG) XPOINT = MAXIMG
         XMUL = (TRC(1) - BLC(1)) / (XPOINT - 1.0)
         N = XPOINT + 0.01
         DO 85 I = 1,N
            TAB(I) = (I-1) * XMUL + 1.0
 85         CONTINUE
C                                       Init interp coeff.
         CALL SETCOF (IT)
         GO TO 200
C                                       Set axes in output CATBLK.
 200  XMUL = (TRC(1) - BLC(1)) / (XPOINT - 1.0)
      CALL SUBHDR (BLC, TRC, XMUL, 1.0)
      IRET = 0
C                                       Remove clean beam on CONV
C                                       if x is position axis
      IF (ICODE.EQ.1) THEN
         IF ((CATR(KRBMJ).EQ.0.0) .AND. (CATR(KRBMN).EQ.0.0)) GO TO 999
         DO 210 I = 1,10
            CALL H2CHR (4, 1, CATH(KHCTP), CHTM12)
            EQUAL = CHTM12(1:4) .EQ. POSTYP(I)
            IF (EQUAL) THEN
               CATR(KRBMJ) = 0.0
               CATR(KRBMN) = 0.0
               CATR(KRBPA) = 0.0
               WRITE (MSGTXT,1220)
               CALL MSGWRT (3)
               GO TO 999
               END IF
 210        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1220 FORMAT ('CLEAN beam parameters reset due to x-axis convolution')
      END
      SUBROUTINE XSMINT (IPOS, XDATA, RESULT, IRET)
C-----------------------------------------------------------------------
C   XSMINT interpolates an image row  Up to 10 history entries are
C   written on the last call to the HISCRD array in common.  If IRET
C   > 0, then the output file will be destroyed.  After all data have
C   been processed a final call will be made with IPOS(1)=-1.
C   Inputs:
C      IPOS(7)   I    BLC (input image) of first value in XDATA
C      XDATA(*)   R    Input row, magic value blanked.
C   Values from commons:
C      FBLANK    R    Value of blanked pixel.
C      DPARM(10) R    Input adverb array.
C      CATBLK    I    Output catalog header (also CATR, CATD)
C      CATOLD    I    Input catalog header (also OLD4, OLD8)
C   Output:
C      RESULT(*) R    Output row.
C      IRET      I    Return code   0 => OK
C                                  >0 => error, terminate.
C   Output in COMMON:
C      NUMHIS     I    # history entries (max. 10)
C      HISCRD(NUMHIS) C*64   History records
C      CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      CHARACTER CTYPE(4)*8
      INTEGER   IPOS(7), MO, MO2, I, K, KP, IXI, N, NP, IT, IRET
      REAL      XDATA(*), RESULT(*), SVX(14), S
      INCLUDE 'XSMTH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA CTYPE /'LINEAR', 'CUBIC', 'QUINTIC', 'SEPTIC'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if last call
      IF (IPOS(1).LT.0) GO TO 900
C                                       Dummy version, copy to output
      N = TRC(1) - BLC(1) + 1.01
      MO = DPARM(1) + 1.01
      MO2 = 2 * MO
      NP = CATBLK(KINAX)
      DO 20 I = 1,NP
         RESULT(I) = FBLANK
         S = 0.0
         CALL IEVERT (TAB(I), IXI, SVX)
         DO 15 K = 1,MO2
            KP = K + IXI
            IF ((KP.GE.1) .AND. (KP.LE.N)) GO TO 10
               IF ((KP.LT.1-MO) .OR. (KP.GT.N+MO)) GO TO 20
                  IF (KP.LT.1) KP = 1 - KP
                  IF (KP.GT.N) KP = 2*N - KP + 1
 10         IF (XDATA(KP).EQ.FBLANK) GO TO 20
               S = S + SVX(K) * XDATA(KP)
 15         CONTINUE
         RESULT(I) = S
 20      CONTINUE
      GO TO 999
C                                       Last call - do history etc.
 900  NUMHIS = 2
      WRITE (HISCRD(1),1901) XPOINT
      IT = DPARM(1) + 1.01
      WRITE (HISCRD(2),1902) CTYPE(IT)
C
 999  RETURN
C-----------------------------------------------------------------------
 1901 FORMAT ('XPOINTS =',F7.0,10X,'/ output x dimension')
 1902 FORMAT ('ITYPE   =''',A8,'''',7X,'/ interpolation level')
      END
      SUBROUTINE XSMCON (IPOS, XDATA, RESULT, IRET)
C-----------------------------------------------------------------------
C   XSMCON convolves an input row with a convolving look up table
C   established in common.  Up to 10 history entries are written
C   on the last call to the HISCRD array in common.  If IRET > 0,
C   then the output file will be destroyed.  After all data have
C   been processed a final call will be made with IPOS(1)=-1.
C   Inputs:
C      IPOS(7)   I    BLC (input image) of first value in XDATA
C      XDATA(*)   R    Input row, magic value blanked.
C   Values from commons:
C      FBLANK    R    Value of blanked pixel.
C      DPARM(10) R    Input adverb array.
C      CATBLK    I    Output catalog header (also CATR, CATD)
C      CATOLD    I    Input catalog header (also OLD4, OLD8)
C      TAB(5000) R    Convolution look-up table
C      WCUTOF    R    Blank if sum weights < WCUTOF
C      SUPRAD    R    Conv. function support radius (old cells)
C      IXDIV     I    # divisions / old cell in look-up table
C   Output:
C      RESULT(*) R    Output row.
C      IRET      I    Return code   0 => OK
C                                  >0 => error, terminate.
C   Output in COMMON:
C      NUMHIS     I    # history entries (max. 10)
C      HISCRD(NUMHIS) C*64   History records
C      CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      CHARACTER CTYPE(4)*8
      INTEGER   IPOS(7), LROW, I, IRET, IROW, IT, J, J1, J2, L
      REAL      XDATA(*), RESULT(*), S, W, XJ, XMUL, XDIV
      INCLUDE 'XSMTH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA CTYPE /'HANNING', 'GAUSSIAN', 'PILL BOX', 'SIN(X)/X'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if last call
      IF (IPOS(1).LT.0) GO TO 900
         LROW = TRC(1) - BLC(1) + 1.01
         IROW = CATBLK(KINAX)
         XMUL = (LROW-1.0) / (IROW-1.0)
         XJ = 1.0 - XMUL
         XDIV = IXDIV
         DO 30 I = 1,IROW
            XJ = XJ + XMUL
            J1 = XJ - SUPRAD + 0.9999
            J2 = XJ + SUPRAD
            J1 = MAX (1, J1)
            J2 = MIN (LROW, J2)
            S = 0.0
            W = 0.0
            IF (J1.GT.J2) GO TO 20
               DO 10 J = J1,J2
                  IF (XDATA(J).EQ.FBLANK) GO TO 10
                     L = ABS(XJ-J)*XDIV + 1.5
                     S = XDATA(J) * TAB(L) + S
                     W = TAB(L) + W
 10               CONTINUE
 20         RESULT(I) = FBLANK
            IF (W.GT.WCUTOF) RESULT(I) = S / W
 30         CONTINUE
         GO TO 999
C                                       Last call - do history etc.
 900  NUMHIS = 5
      WRITE (HISCRD(1),1901) XPOINT
      IT = DPARM(1) + 1.01
      WRITE (HISCRD(2),1902) CTYPE(IT)
      WRITE (HISCRD(3),1903) DPARM(2), DPARM(3)
      WRITE (HISCRD(4),1904) IXDIV
      WRITE (HISCRD(5),1905) WCUTOF
C
 999  RETURN
C-----------------------------------------------------------------------
 1901 FORMAT ('XPOINT =',F7.0,10X,'/ output x dimension')
 1902 FORMAT ('CTYPE  =''',A8,'''',7X,'/ convolving function')
 1903 FORMAT ('CWIDTH =',F7.1,' CSUPRT =',F7.1,2X,'/ width, support',
     *   ' (old cells)')
 1904 FORMAT ('IXDIV  =',I7,10X,'/ conv function eval ixdiv times',
     *   '/cell')
 1905 FORMAT ('WCUTOFF=',1PE12.4,5X,'/ min ok sum of conv function')
      END
      SUBROUTINE IEVERT (DX, IX, SVECT)
C-----------------------------------------------------------------------
C   Compute subscript offset and weights for interpolating at a
C   specified position in a vector.  Interpolation is done using a
C   group of pixels centered on the specified position. The order of
C   interpolation is specified by integer MORD in subroutine SETCOF.
C   We do linear interpolation for MORD=0, cubic for 1, and quintic
C   for 2. If MORD=1 (i.e., cubic interpolation) we will be using
C   four pixels in the interpolation.  From the Everett interpolation
C   package originally coded by Larry Goad at KPNO.
C   Inputs:  DX     R       Position in vector
C   Outputs: IX     I       Offset to start
C            SVECT  R(*)    Weights
C-----------------------------------------------------------------------
      INTEGER   IX
      REAL      SVECT(1), DX
      INTEGER   NR, IR0, IV0, MC, IR
      REAL      U, W, CW, CU, W2, U2
      INCLUDE 'INCS:DEVI.INC'
C-----------------------------------------------------------------------
      IX = DX
      U  = DX - IX
      IX = IX - IS0
      CALL RFILL (NVALS, 0.0, SVECT)
C                                       distance of point from cells
      W  = 1.0 - U
      CW = W
      CU = U
      W2 = W * W
      U2 = U * U
      SVECT(IS0)   = W
      SVECT(IS0+1) = U
C                                       leave if on cell or linear
      IF ((U.EQ.0) .OR. (MORD2.EQ.0)) GO TO 999
      NR  = 1
      IR0 = 0
      IV0 = IS0 - 1
      DO 20 MC = 1,MORD2
C                                       (IR0=MC*MC):
         IR0 = IR0 + NR
C                                       (NR=2*MC+1):
         NR  = NR  + 2
C                                       (IV0=IS0-MC-1):
         IV0 = IV0 - 1
         CU  = CU * (U2 - IR0) / ((IR0 + IR0 + MC) + (IR0 + IR0 + MC))
         CW  = CW * (W2 - IR0) / ((IR0 + IR0 + MC) + (IR0 + IR0 + MC))
         DO 10 IR = 1,NR
            SVECT(IV0+IR)   = SVECT(IV0+IR)   + CW * BCOEF(IR0+IR)
            SVECT(IV0+IR+1) = SVECT(IV0+IR+1) + CU * BCOEF(IR0+IR)
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SETCOF (IORD)
C-----------------------------------------------------------------------
C   SETCOF computes certain quantities which are needed by IEVERT
C   when it computes the actual weights for an interpolation. The
C   result produced by SETCOF is in the BCOEF array in the COMMON
C   block, and is based on binomial coefficients computed by BINOM.
C   From the Everett interpolation package originally coded by
C   Larry Goad at KPNO.
C   Input: IORD   I     Order of interp 0 = linear, 1 cubic, 2 quintic
C-----------------------------------------------------------------------
      INTEGER   IORD, IN, NT, M, N
      REAL      XT
      INCLUDE 'INCS:DEVI.INC'
C-----------------------------------------------------------------------
      MORD2 = MIN (IORD, 6)
      IN = 1
      NT = 0
      BCOEF(1) = 1.
C                                   Compute the BCOEF array:
      DO 50 M = 1,MORD2
         IN = IN + NT + 1
         NT = M + M
         XT = NT
         CALL BINOM (XT, NT, BCOEF(IN))
C
         DO 40 N = 1,NT,2
            BCOEF(IN+N) = -BCOEF(IN+N)
 40         CONTINUE
 50      CONTINUE
C                                    Set up pointer constants:
      IS0   = MORD2 + 1
      NVALS = IS0  + IS0
C
 999  RETURN
      END
      SUBROUTINE BINOM (X, M, VAL)
C-----------------------------------------------------------------------
C   BINOM generates binomial coefficients for use in the Everett
C   interpolation routines. It is called only by SETCOF.  From the
C   Everett interpolation package originally coded by Larry Goad, KPNO.
C-----------------------------------------------------------------------
      INTEGER    M
      REAL       X, VAL(1)
      INTEGER    I
      REAL       R, XL
C-----------------------------------------------------------------------
      VAL(1) = 1.
      R = 0.
      XL = X + 1.
C
      DO 20 I = 1,M
         XL = XL - 1.
         R = R + 1.
         VAL(I+1) = VAL(I) * XL / R
 20      CONTINUE
C
      RETURN
      END
