LOCAL INCLUDE 'XSUM.INC'
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, OPCODE*4,
     *   HISCRD(7)*64
      HOLLERITH OLDH(256), XNAMEI(3), XCLAIN(2), XNAMOU(3),
     *   XCLAOU(2), XOPCOD
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7),
     *   OLD4(256), BUFF1(MABFSS), BUFF2(MABFSS)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD(256), NUMHIS, JBUFSZ, ICODE, SCRTCH(256)
      DOUBLE PRECISION OLD8(128)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOU,
     *   XSEQO, XDISKO, BLC, TRC, XOPCOD
      COMMON /PARMS/ SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, JBUFSZ, ICODE, NUMHIS
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, OPCODE, HISCRD
      COMMON /BUFRS/ BUFF1, BUFF2, SCRTCH
      COMMON /OLDHDR/ CATOLD
      EQUIVALENCE (CATOLD, OLD4, OLDH, OLD8)
LOCAL END
      PROGRAM XSUM
C-----------------------------------------------------------------------
C! XSUM sums or averages each row of image to produce N-1 D image.
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
C   XSUM produces an N-1 dimensional map holding the sum or average
C   of each row of the input image.
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
C      OPCODE         OPCODE        'SUM ' or 'AVER'
C   Programmer E.W. Greisen, May, 1983 from TAFFY by W. D. Cotton.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'XSUM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'XSUM  '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL XSUMIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL XSUMMA (IRET)
      IF (IRET.EQ.0) CALL XSUMHI
C                                       Close down files, etc.
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE XSUMIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   XSUMIN gets input parameters for XSUM and creates an output file.
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
      INCLUDE 'XSUM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
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
      NPARM = 29
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
     *   SEQOUT)
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
 1000 FORMAT ('XSUMIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE XSUMMA (IRET)
C-----------------------------------------------------------------------
C   XSUMMA sends image one row at a time to the summing routine.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER IFILE*48
      INTEGER   IRET, IROUND, LUNI, LUNO, RBP, NYI, NXI, WINI(4),
     *   NXO, NYO, WINO(4), BOI, BOO, BPI, BPO, LIM2, LIM3, LIM4,
     *   LIM5, LIM6, LIM7, I1, I2, I3, I4, I5, I6, I7, IPOS(7),
     *   CORN(7), BOTEMP, KOFF, LIMO, LIMIT, IBIND, OBIND, INDI,
     *   INDO, LIM1, ISCR(256)
      REAL      OUTMAX, OUTMIN
      LOGICAL   T, F, BLNKD
      INCLUDE 'XSUM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
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
C                                       Use actual f.p. output file.
      RBP = 2
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Setup for I/O
      NXI = CATOLD(KINAX)
      NYI = CATOLD(KINAX+1)
      IF (NYI.LE.0) NYI = 1
      NXO = CATBLK(KINAX)
      NYO = CATBLK(KINAX+1)
      IF (NYO.LE.0) NYO = 1
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
      BPO = 2
      BPI = BPO
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
      KOFF = -1
      CORN(7) = 1
      LIMO = 0
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
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3), BOTEMP,
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
C                                       Init output file.
         IF (I3.LE.1) THEN
            CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORN(3), BOTEMP,
     *         IRET)
            BOO = BOTEMP + 1
            CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2,
     *         JBUFSZ, BOO, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) 'WRIT', IRET
               GO TO 990
               END IF
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
C                                       Write.
            IF (I2.LE.1) THEN
               CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
               OBIND = OBIND - 1
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1120) 'WRIT', IRET
                  GO TO 990
                  END IF
               END IF
C                                       Call XSUMIT
            OBIND = OBIND + 1
            CALL XSUMIT (IPOS, BUFF1(IBIND), BUFF2(OBIND), IRET)
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
 200              CONTINUE
 250        CONTINUE
C                                       Flush buffer.
         IF (I3.LT.LIM3) GO TO 300
            CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'FINI', IRET
               GO TO 990
               END IF
C                                       Update CATBLK.
            CATR(KRDMX) = OUTMAX
            CATR(KRDMN) = OUTMIN
            CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', ISCR,
     *         IRET)
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
      CALL XSUMIT (IPOS, BUFF1, BUFF2, IRET)
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
 1000 FORMAT ('XSUMMA: ERROR',I3,' OPENING INPUT FILE')
 1010 FORMAT ('XSUMMA: ERROR',I5,' OPENING OUTPUT FILE')
 1099 FORMAT ('XSUMMA: COMOFF ERROR',I3)
 1100 FORMAT ('XSUMMA: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('XSUMMA: ',A4,' ERROR',I3)
 1180 FORMAT ('XSUMMA: XSUMIT ERROR',I3)
 1260 FORMAT ('XSUMMA: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE XSUMHI
C-----------------------------------------------------------------------
C   XSUMHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER LABEL*8, LINE*80, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR, I
      LOGICAL   T
      INCLUDE 'XSUM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   BUFF1, SCRTCH, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       BLC
      WRITE (MSGTXT,2000) TSKNAM, BLC
      CALL HIADD (LUN2, MSGTXT, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       TRC
      WRITE (MSGTXT,2001) TSKNAM, TRC
      CALL HIADD (LUN2, MSGTXT, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       OPCODE
      WRITE (MSGTXT,2002) TSKNAM, OPCODE
      CALL HIADD (LUN2, MSGTXT, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Add any user supplied history.
      IF (NUMHIS.LE.0) GO TO 20
         WRITE (LABEL,1010) TSKNAM
         LINE(1:8) = LABEL(1:8)
         DO 15 I = 1,NUMHIS
            LINE(9:72) = HISCRD(I)(1:64)
            CALL HIADD (LUN2, LINE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 20
 15         CONTINUE
C                                       Close HI file
 20   CALL HICLOS (LUN2, T, SCRTCH, IERR)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, BUFF1, SCRTCH, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XSUMHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 2000 FORMAT (A6,' BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,' TRC =',6(F6.0,','),F6.0)
 2002 FORMAT (A6,' OPCODE = ''',A4,'''')
      END
      SUBROUTINE XSMHED (IRET)
C-----------------------------------------------------------------------
C   XSMHED sets parameters and corrects the header for sub-imaging and
C   for dropping the first axis.
C   Input:
C      CATBLK(256)    I     Output catalog header, also CATR, CATD
C      CATOLD(256)    I     Input catalog header, also OLD4, OLD8
C   Output:
C      CATBLK(256)    I     Modified output catalog header.
C      IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER FCHARS(3)*4, CODES(2)*4, CTEMP*8
      INTEGER   LIMIT, I, IRET, IL1, NCODE, INDXI, INC, INDEX
      INCLUDE 'XSUM.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA FCHARS /'FREQ','VELO','FELO'/
      DATA NCODE, CODES /2, 'SUM ','AVER'/
C-----------------------------------------------------------------------
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
C                                       Set axes in output CATBLK.
 20   LIMIT = CATOLD(KIDIM) - 1
      IL1 = LIMIT + 1
      INC = 2
C                                       Copy/update axis values
      DO 70 I = 1,LIMIT
         CATBLK(KINAX+I-1) = TRC(I+1) - BLC(I+1) + 1.01
         CATR(KRCRP+I-1) = OLD4(KRCRP+I) - BLC(I+1) + 1.0
         CATR(KRCIC+I-1) = CATR(KRCIC+I)
         CATD(KDCRV+I-1) = OLD8(KDCRV+I)
         INDXI = KHCTP + I * INC
         INDEX = KHCTP + (I-1) * INC
         CALL H2CHR (8, 1, OLDH(INDXI), CTEMP)
         CALL CHR2H (8, CTEMP, 1, CATH(INDEX))
         IF (CATBLK(KIALT).EQ.0) GO TO 70
            CALL H2CHR (4, 1, CATH(INDEX), CTEMP(1:4))
            IF ((CTEMP(1:4).EQ.FCHARS(1)) .OR.
     *          (CTEMP(1:4).EQ.FCHARS(2)) .OR.
     *          (CTEMP(1:4).EQ.FCHARS(3)))
     *         CATR(KRARP) = CATR(KRARP) - BLC(I+1) + 1.0
 70      CONTINUE
      IF (IL1.LE.KICTPN) THEN
         DO 75 I = IL1,KICTPN
            CATBLK(KINAX+I-1) = 1
 75         CONTINUE
         END IF
C                                       reset CATBLK(KIDIM)
C                                       and blank last axis type.
      CATBLK(KIDIM) = CATOLD(KIDIM) - 1
      INDEX = KHCTP + CATBLK(KIDIM) * INC
      CALL CHR2H (8, '        ', 1, CATH(INDEX))
      INDEX = CATBLK(KIDIM)
      CATD(KDCRV+INDEX) = 0.0D0
      CATR(KRCRP+INDEX) = 0.0
      CATR(KRCIC+INDEX) = 0.0
      CATBLK(KINAX+INDEX) = 1
C                                       Finished.
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE XSUMIT (IPOS, DATA, RESULT, IRET)
C-----------------------------------------------------------------------
C   XSUMIT performs the row sum or average.
C   Inputs:
C      IPOS(7)   I    BLC (input image) of first value in DATA
C      DATA(*)   R    Input row, magic value blanked.
C   Values from commons:
C      ICODE     I    Opcode number from list in NEWHED.
C      FBLANK    R    Value of blanked pixel.
C      CATBLK    I    Output catalog header (also CATR, CATD)
C      CATOLD    I    Input catalog header (also OLD4, OLD8)
C   Output:
C      RESULT(*) R    Output row.
C      IRET      I    Return code   0 => OK
C                               >0 => error, terminate.
C   Output in COMMON
C     NUMHIS     I    # history entries (max. 10)
C     HISCRD(NUMHIS) C*64   History records
C     CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      CHARACTER TEMPCH*8
      INTEGER   IRET, I, IPOS(7), LROW, N
      REAL      DATA(1), RESULT(1), S
      INCLUDE 'XSUM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if last call
      IF (IPOS(1).LT.0) GO TO 900
         LROW = TRC(1) - BLC(1) + 1.01
         S = 0.
         N = 0
         DO 10 I = 1,LROW
            IF (DATA(I).EQ.FBLANK) GO TO 10
               S = S + DATA(I)
               N = N + 1
 10         CONTINUE
         RESULT(1) = FBLANK
         IF (N.LE.0) GO TO 999
            IF (ICODE.EQ.1) RESULT(1) = S
            IF (ICODE.EQ.2) RESULT(1) = S / N
            GO TO 999
 900  CONTINUE
         NUMHIS = 7
         WRITE (HISCRD(1),1900)
         LROW = TRC(1) - BLC(1) + 1.01
         WRITE (HISCRD(2),1901) LROW
         S = OLD4(KRCRP) - BLC(1) + 1.0
         WRITE (HISCRD(3),1902) S
         WRITE (HISCRD(4),1903) OLD8(KDCRV)
         WRITE (HISCRD(5),1904) OLD4(KRCIC)
         CALL H2CHR (8, 1, OLDH(KHCTP), TEMPCH)
         WRITE (HISCRD(6),1905) TEMPCH
         WRITE (HISCRD(7),1906) OLD4(KRCRT)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT (' Axis over which summing performed:')
 1901 FORMAT (' NAXIS0   =',I6,14X,'/ number subimage points')
 1902 FORMAT (' CRPIX0   =',F10.3,12X,'/ subimage reference pixel')
 1903 FORMAT (' CRVAL0   =',1PE13.5,7X,'/ ref pixel value')
 1904 FORMAT (' CDELT0   =',1PE13.5,7X,'/ coord increment')
 1905 FORMAT (' CTYPE0   =''',A8,'''',10X,'/ axis type')
 1906 FORMAT (' CROTA0   =',F8.2,12X,'/ coord rotation')
      END
