LOCAL INCLUDE 'AHIST.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD(256), NUMHIS, JBUFSZ, XWIN, YWIN, IBLC(2), DBPTR
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOT(2)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6,
     *   HISCRD(10)*64
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7), XXWIN,
     *   XYWIN, PIXRNG(2), BUFF1(MABFSS), BUFF2(MABFSS), DB(MAXIMG,29),
     *   OLDR(256)
      EQUIVALENCE (CATOLD, OLDR)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOT,
     *   XSEQO, XDISKO, BLC, TRC, XXWIN, XYWIN, PIXRNG
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, HISCRD
      COMMON /PARMS/ CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, JBUFSZ, NUMHIS, XWIN, YWIN, IBLC, DBPTR
      COMMON /BUFRS/ BUFF1, BUFF2, DB
LOCAL END
      PROGRAM AHIST
C-----------------------------------------------------------------------
C! Task to alter image intensities by windowed histogram equalization
C# Map-util Analysis
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
C   AHIST does a histogram equalization of pixel intensities using only
C   those pixels within a 2-dimensional window surrounding the pixel.
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
C      XINC           XWIN          X window size in pixels
C      YINC           YWIN          X window size in pixels
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'AHIST.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'AHIST '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL AHEIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL AHEDO (IRET)
C                                       History
      IF (IRET.EQ.0) CALL AHEHIS
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE AHEIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   AHEIN gets input parameters for AHIST and creates an output file.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
C                               4 => user routine detected error.
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, MTYPE*2
      INTEGER   IERR, NPARM, IROUND
      REAL      RTEMP(2)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'AHIST.INC'
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
      NPARM = 32
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOT, CLAOUT)
C                                       Crunch input parameters.
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
      XWIN = IROUND (XXWIN)
      IF (XWIN.LE.1) XWIN = 11
      XWIN = MAX (5, MIN (29, XWIN))
      XWIN = (XWIN / 2) * 2 + 1
      YWIN = IROUND (XYWIN)
      IF (YWIN.LE.1) YWIN = 11
      YWIN = MAX (5, MIN (29, YWIN))
      YWIN = (YWIN / 2) * 2 + 1
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'READ', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       set input range
      RTEMP(1) = PIXRNG(1)
      RTEMP(2) = PIXRNG(2)
      CALL RNGSET (RTEMP, OLDR(KRDMX), OLDR(KRDMN), PIXRNG)
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
C                                       Set output units.
      CALL CHR2H (8, 'HISTEQAL', 1, CATH(KHBUN))
C                                       Set axes in output CATBLK.
      CALL SUBHDR (BLC, TRC, 1, 1)
C                                       Create output file.
      NEWCNO = 1
      IRET = 4
      CALL MCREAT (DISKO, NEWCNO, BUFF1, IERR)
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
 1000 FORMAT ('AHEIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('AHEIN: ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE AHEDO (IRET)
C-----------------------------------------------------------------------
C   AHEDO sends image one row at a time to the user supplied
C   routine and then writes the modified data.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER IFILE*48
      INTEGER   IROUND, LUNI, LUNO, NYI, NXI, WINI(4), NXO, NYO,
     *   WINO(4), BOI, BOO, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2,
     *   I3, I4, I5, I6, I7, IPOS(7), CORN(7), BOTEMP, KOFF, LIMO,
     *   LIMIT, IBIND, OBIND, INDI, INDO, LIM1, DIDRET, OUTCNT
      REAL      OUTMAX, OUTMIN
      LOGICAL   T, F, BLNKD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'AHIST.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI, LUNO /16,17/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open the data files
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
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
      OUTMAX = -1.0E20
      OUTMIN = 1.0E20
      BLNKD = F
      IBLC(1) = BLC(1) + 0.1
      IBLC(2) = BLC(1) + 0.1
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM4 = TRC(4) - BLC(4) + 1.01
      LIM5 = TRC(5) - BLC(5) + 1.01
      LIM6 = TRC(6) - BLC(6) + 1.01
      LIM7 = TRC(7) - BLC(7) + 1.01
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
                     WRITE (MSGTXT,1090) IPOS(3)
                     CALL MSGWRT (1)
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
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORN(3), BOTEMP,
     *      IRET)
         BOO = BOTEMP + 1
         CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2,
     *      JBUFSZ, BOO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'WRIT', IRET
            GO TO 990
            END IF
         DIDRET = 0
         OUTCNT = NYO
         DO 220 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.9
            IPOS(1) = IROUND (BLC(1))
C                                       Read.
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'READ', IRET
               GO TO 990
               END IF
C                                       Check for deferred output.
            IF (DIDRET.GE.0) THEN
               CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
               OBIND = OBIND - 1
               OUTCNT = OUTCNT - 1
               IF (OUTCNT.LT.0) THEN
                  WRITE (MSGTXT,1113)
                  GO TO 990
                  END IF
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1120) 'WRIT', IRET
                  GO TO 990
                  END IF
               END IF
C                                       Call ADHIST
            IF (DIDRET.GE.0) OBIND = OBIND + 1
            CALL ADHIST (IPOS, BUFF1(IBIND), BUFF2(OBIND), IRET)
            DIDRET = IRET
C                                       Error
            IF (DIDRET.GT.0) THEN
               WRITE (MSGTXT,1180) IRET
               GO TO 990
C                                       output is now desired
            ELSE IF (DIDRET.EQ.0) THEN
C                                       Check max, min, blanking.
               LIMIT = OBIND + LIMO
               DO 200 I1 = OBIND,LIMIT
                  IF (BUFF2(I1).NE.FBLANK) THEN
                     OUTMAX = MAX (OUTMAX, BUFF2(I1))
                     OUTMIN = MIN (OUTMIN, BUFF2(I1))
                  ELSE
                     BLNKD = .TRUE.
                     END IF
 200              CONTINUE
               END IF
 220        CONTINUE
C                                       Read out any remaining rows
C                                       from ADHIST.
            IF (OUTCNT.GT.0) THEN
               DO 260 I2 = 1,OUTCNT
                  IPOS(1) = -1
C                                       Check if write requested.
                  CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1120) 'WRIT', IRET
                     GO TO 990
                     END IF
C                                       Call ADHIST
                  CALL ADHIST (IPOS, BUFF1(IBIND), BUFF2(OBIND), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1180) IRET
                     GO TO 990
                     END IF
C                                       Check max, min, blanking.
                  LIMIT = OBIND + LIMO
                  DO 250 I1 = OBIND,LIMIT
                     IF (BUFF2(I1).NE.FBLANK) THEN
                        OUTMAX = MAX (OUTMAX, BUFF2(I1))
                        OUTMIN = MIN (OUTMIN, BUFF2(I1))
                     ELSE
                        BLNKD = .TRUE.
                        END IF
 250                 CONTINUE
 260              CONTINUE
               END IF
C                                       Flush buffer.
            CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'FINI', IRET
               GO TO 990
               END IF
C                                       Update CATBLK.
            CATR(KRDMX) = OUTMAX
            CATR(KRDMN) = OUTMIN
            CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFF1,
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
C                                       Close input map.
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AHEDO: ERROR',I3,' OPENING INPUT FILE')
 1020 FORMAT ('AHEDO: ERROR',I5,' OPENING OUTPUT FILE')
 1090 FORMAT ('Beginning plane',I4)
 1099 FORMAT ('AHEDO: COMOFF ERROR',I3)
 1100 FORMAT ('AHEDO: INIT-FOR-',A4,' ERROR',I3)
 1113 FORMAT ('AHEDO: OUTCNT.LT.0! Too many output rows returned.')
 1120 FORMAT ('AHEDO: ',A,' ERROR',I3)
 1180 FORMAT ('AHEDO: ADHIST ERROR',I3)
 1260 FORMAT ('AHEDO: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE AHEHIS
C-----------------------------------------------------------------------
C   AHEHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER LINE*80, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'AHIST.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       BLC
      WRITE (LINE,2000) TSKNAM, BLC
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       TRC
      WRITE (LINE,2001) TSKNAM, TRC
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       X window
      WRITE (LINE,2002) TSKNAM, 'X', XWIN, 'columns'
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Y window
      WRITE (LINE,2002) TSKNAM, 'Y', YWIN, 'rows'
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Pixel range
      WRITE (LINE,2003) TSKNAM, PIXRNG
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AHEHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,'TRC =',6(F6.0,','),F6.0)
 2002 FORMAT (A6,A1,'WIN = ',I3,10X,' / ',A,' in histogram window')
 2003 FORMAT (A6,'PIXRANGE =',1PE11.3,' ,',1PE11.3,4X,
     *   '/ pixel range limit')
      END
      SUBROUTINE ADHIST (IPOS, DATA, RESULT, IRET)
C-----------------------------------------------------------------------
C   Inputs:
C      IPOS   I(7)    BLC (input image) of first value in DATA
C                     IPOS(1) = -1 => no input data this call.
C      DATA   R(*)    Input row, magic value blanked.
C   Values from commons:
C      FBLANK  R      Value of blanked pixel.
C      CATBLK  I      Output catalog header (also CATR, CATD)
C      CATOLD  I      Input catalog header
C   Output:
C      RESULT  R(*)   Output row.
C      IRET    I      Return code   0 => OK
C                                  >0 => error, terminate.
C   Output in COMMON:
C      NUMHIS  I          # history entries (max. 10)
C      HISCRD  C(NUMHIS)  History records
C      CATBLK  I          Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IRET
      REAL      DATA(*), RESULT(*)
C
      REAL      R
      DOUBLE PRECISION  X, Y, Z, VALS(512), SUM, RL
      INTEGER   LCOL, LY, IY, NHISTO, NVALS, HISTOG(10000), IX, LX,
     *   NHIST, HALFX, JY, I, J, L
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'AHIST.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Data call
      IF (IPOS(1).GT.0) THEN
         LY = IPOS(2) - IBLC(2) + 1
         JY = MOD (LY - 1, YWIN) + 1
         LCOL = CATBLK(KINAX)
         CALL RCOPY (LCOL, DATA, DB(1,JY))
C                                       Defer YWIN/2
         IF (LY.LE.YWIN/2) THEN
            IRET = -1
C                                       Blank YWIN/2
         ELSE IF (LY.LT.YWIN) THEN
            CALL RFILL (LCOL, FBLANK, RESULT)
C                                       Actually do something
         ELSE
            NHISTO = 10000
            NVALS = MIN (500, XWIN*YWIN)
            Y = NHISTO / (PIXRNG(2) - PIXRNG(1))
            CALL FILL (NHISTO, 0, HISTOG)
            NHIST = 0
            HALFX = XWIN / 2
            CALL FILL (HALFX, FBLANK, RESULT(1))
            CALL FILL (HALFX, FBLANK, RESULT(LCOL+1-HALFX))
            DO 100 IX = 1,LCOL
C                                       subtract lost column
               LX = IX - XWIN
               IF (LX.GT.0) THEN
                  DO 10 IY = 1,YWIN
                     R = DB(LX,IY)
                     IF ((R.NE.FBLANK) .AND. (R.GE.PIXRNG(1)) .AND.
     *                  (R.LE.PIXRNG(2))) THEN
                        R = Y * (R - PIXRNG(1)) + 1.00001
                        IF ((R.GE.1.) .AND. (R.LE.NHISTO)) THEN
                           J = R
                           HISTOG(J) = HISTOG(J) - 1
                           NHIST = NHIST - 1
                           END IF
                        END IF
 10                  CONTINUE
                  END IF
C                                       add next column
               DO 20 IY = 1,YWIN
                  R = DB(IX,IY)
                  IF ((R.NE.FBLANK) .AND. (R.GE.PIXRNG(1)) .AND.
     *               (R.LE.PIXRNG(2))) THEN
                     R = Y * (R - PIXRNG(1)) + 1.00001
                     IF ((R.GE.1.) .AND. (R.LE.NHISTO)) THEN
                        J = R
                        HISTOG(J) = HISTOG(J) + 1
                        NHIST = NHIST + 1
                        END IF
                     END IF
 20               CONTINUE
C                                       Do we need to equalize?
               IF (IX.GE.XWIN) THEN
                  LX = IX - HALFX
                  IF (NHIST.EQ.0) THEN
                     RESULT(LX) = FBLANK
                  ELSE IF (DATA(LX).LT.PIXRNG(1)) THEN
                     RESULT(LX) = - 0.5 / NVALS
                  ELSE IF (DATA(LX).GT.PIXRNG(2)) THEN
                     RESULT(LX) = 1.0 + 0.5 / NVALS
C                                       Find by equalization
                  ELSE
                     VALS(1) = PIXRNG(1)
                     X = REAL (NHIST) / REAL (NVALS)
                     Z = 0.0
                     L = 1
                     RL = 0.0
                     SUM = 0.0
                     DO 50 I = 1,NVALS
                        DO 30 J = L,NHISTO
                           SUM = SUM + HISTOG(J)
                           IF (SUM.GE.X) GO TO 40
                           L = L + 1
 30                        CONTINUE
                        VALS(I+1) = (VALS(I) + PIXRNG(2)) / 2.0
                        GO TO 50
C                                       get value
 40                     Z = SUM - X
                        RL = L - (Z / HISTOG(L)) * (L - RL)
                        VALS(I+1) = PIXRNG(1) + RL / Y
                        IF (DATA(LX).LT.VALS(I+1)) GO TO 60
                        SUM = Z - HISTOG(L)
 50                     CONTINUE
                     I = NVALS + 1
                     VALS(NVALS+2) = PIXRNG(2)
                     VALS(NVALS+3) = PIXRNG(2) + 1.E9
 60                  IF (VALS(I+1).GT.VALS(I)) THEN
                        RESULT(LX) = (I - 1.0 + (DATA(LX) - VALS(I)) /
     *                     (VALS(I+1) - VALS(I))) / (NVALS + 1.0)
                     ELSE
                        RESULT(LX) = FBLANK
                        END IF
                     IF ((RESULT(LX).GT.1.01) .OR.
     *                  (RESULT(LX).LT.-0.01)) THEN
                        WRITE (MSGTXT,1060) LX, LY
                        CALL MSGWRT (6)
                        WRITE (MSGTXT,1061) I, VALS(I+1), VALS(I),
     *                     DATA(LX)
                        CALL MSGWRT (6)
                        END IF
                     END IF
                  END IF
 100           CONTINUE
            END IF
C                                       No data in, return blanked
      ELSE IF (IPOS(1).EQ.-1) THEN
         LCOL = CATBLK(KINAX)
         CALL RFILL (LCOL, FBLANK, RESULT)
C                                       Error
      ELSE
         IRET = 99
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('Warning: odd result at x,y',2I6)
 1061 FORMAT ('I vals(i+1) vals(i) data(lx)',I5,3F10.4)
      END
