LOCAL INCLUDE 'HISEQ.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD(256), JBUFSZ, NHISTO, NVALS, NHIST
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOT(2)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7),
     *   PIXRNG(2), XNVAL, XDOHST, BUFF1(MABFSS), BUFF2(MABFSS),
     *   HISTOG(100000), VALS(1012)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOT,
     *   XSEQO, XDISKO, BLC, TRC, PIXRNG, XNVAL, XDOHST
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /PARMS/  CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, NHISTO, NVALS, NHIST, JBUFSZ, HISTOG, VALS
      COMMON /BUFRS/ BUFF1, BUFF2
LOCAL END
      PROGRAM HISEQ
C-----------------------------------------------------------------------
C! Task to convert image intensities by histogram equalization
C# Map-util Utility
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
C   HISEQ write out a histogram-equalized version of the input image.
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
C      PIXRANGE(2)    PIXRNG        Image intensity ramge to include
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'HISEQ.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'HISEQ '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL HIEQIN (PRGM, IRET)
C                                       read input for histogram
      IF (IRET.EQ.0) CALL HIEQRD (IRET)
C                                       write the output
      IF (IRET.EQ.0) CALL HIEQOU (IRET)
C                                       History
      IF (IRET.EQ.0) CALL HIEQHI
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE HIEQIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   HIEQIN gets input parameters for HISEQ and creates an output file.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
C                               4 => user routine detected error.
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6
C
      CHARACTER  STAT*4, MTYPE*2
      INTEGER   IERR, NPARM, IROUND
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'HISEQ.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NHIST = 0
      NHISTO = 100000
      CALL RFILL (NHISTO, 0.0, HISTOG)
      JBUFSZ = 2 * MABFSS
      IRET = 0
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
      NVALS = IROUND (XNVAL)
      IF ((NVALS.LT.10) .OR. (NVALS.GT.1000)) NVALS = 1000
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
      CALL NEWHED (IRET)
      IF (IRET.NE.0) GO TO 999
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
 1000 FORMAT ('HIEQIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('HIEQIN: ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE NEWHED (IRET)
C-----------------------------------------------------------------------
C   NEWHED is a routine to modify the output header appropriately.
C   Input:
C      CATBLK    I(256)  Output catalog header, also CATR, CATD
C      CATOLD    I(256)  Input catalog header, also OLD4, OLD8
C   Output:
C      CATBLK    I(256)  Modified output catalog header.
C      IRET      I       Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      HOLLERITH OLD4(256)
      DOUBLE PRECISION    OLD8(128)
      INTEGER   INDXI, INC, INDEX, LIMIT, I, FIRSTI, FIRSTO
      REAL      RTEMP(2), OLDR(256)
      CHARACTER CTEMP*8, FCHARS(3)*4
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'HISEQ.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (CATOLD, OLD4, OLDR, OLD8)
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
C                                       Set output units.
      CALL CHR2H (8, 'HISTEQAL', 1, CATH(KHBUN))
C                                       Set axes in output CATBLK.
      FIRSTI = -1
      FIRSTO = -1
      LIMIT = CATOLD(KIDIM)
      INC = 2
C                                       Copy/update axis values
      DO 10 I = 1,LIMIT
         CATBLK(KINAX+FIRSTO+I) = TRC(I+FIRSTI+1) -
     *      BLC(I+FIRSTI+1) + 1.01
         CATR(KRCRP+FIRSTO+I) = OLDR(KRCRP+FIRSTI+I) - BLC(I+FIRSTI+1)
     *      + 1.0
         CATR(KRCIC+FIRSTO+I) = OLDR(KRCIC+FIRSTI+I)
         CATD(KDCRV+FIRSTO+I) = OLD8(KDCRV+FIRSTI+I)
         INDXI = KHCTP + (I+FIRSTI) * INC
         INDEX = KHCTP + (I-1) * INC
         CALL H2CHR (8, 1, OLD4(INDXI), CTEMP)
         CALL CHR2H (8, CTEMP, 1, CATH(INDEX))
         IF (CATBLK(KIALT).NE.0) THEN
            CALL H2CHR (4, 1, CATH(INDEX), CTEMP)
            IF ((CTEMP(1:4).EQ.FCHARS(1)) .OR.
     *          (CTEMP(1:4).EQ.FCHARS(2)) .OR.
     *          (CTEMP(1:4).EQ.FCHARS(3)))
     *      CATR(KRARP) = CATR(KRARP) - BLC(FIRSTI+I+1) + 1.0
            END IF
 10      CONTINUE
C                                       set input range
      RTEMP(1) = PIXRNG(1)
      RTEMP(2) = PIXRNG(2)
      CALL RNGSET (RTEMP, OLDR(KRDMX), OLDR(KRDMN), PIXRNG)
C                                       Finished.
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE HIEQRD (IRET)
C-----------------------------------------------------------------------
C   HIEQRD sends image one row at a time to the histogram routine
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER IFILE*48
      INTEGER   IROUND, LUNI, NYI, NXI, WINI(4), BOI, LIM2, LIM3, LIM4,
     *   LIM5, LIM6, LIM7, I2, I3, I4, I5, I6, I7, IPOS(7), CORN(7),
     *   BOTEMP, LIMO, IBIND, INDI, LIM1
      REAL      OLD4(256), DUM(1)
      DOUBLE PRECISION    OLD8(128)
      LOGICAL   T, F, DOMSG
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'HISEQ.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (CATOLD, OLD4, OLD8)
      DATA LUNI /16/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Setup for I/O
      NXI = CATOLD(KINAX)
      NYI = CATOLD(KINAX+1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM4 = TRC(4) - BLC(4) + 1.01
      LIM5 = TRC(5) - BLC(5) + 1.01
      LIM6 = TRC(6) - BLC(6) + 1.01
      LIM7 = TRC(7) - BLC(7) + 1.01
      DOMSG = (LIM3 * LIM4 * LIM5 * LIM6 * LIM7) .GT. 5
      LIMO = CATBLK(KINAX) - 1
      MSGTXT = 'Begin computing the histogram'
      CALL MSGWRT (2)
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = BLC(7) + I7 - 0.9
         CORN(7) = I7
         DO 600 I6 = 1,LIM6
            IPOS(6) = BLC(6) + I6 - 0.9
            CORN(6) = I6
            DO 500 I5 = 1,LIM5
               IPOS(5) = BLC(5) + I5 - 0.9
               CORN(5) = I5
               DO 400 I4 = 1,LIM4
                  IPOS(4) = BLC(4) + I4 - 0.9
                  CORN(4) = I4
                  DO 300 I3 = 1,LIM3
                     IPOS(3) = BLC(3) + I3 - 0.9
                     CORN(3) = I3
                     WRITE (MSGTXT,1010) IPOS(3), IPOS(4), IPOS(5),
     *                  IPOS(6), IPOS(7)
                     IF ((MOD(I3,20).EQ.1) .AND. (DOMSG)) CALL MSGWRT(1)
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
         DO 220 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.9
            IPOS(1) = IROUND (BLC(1))
C                                       Read.
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'READ', IRET
               GO TO 990
               END IF
C                                       Call HISGET
            CALL HISGET (IPOS, BUFF1(IBIND), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1180) IRET
               GO TO 990
               END IF
 220        CONTINUE
 300     CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Final call to functions
      IPOS(1) = -2
      CALL HISGET (IPOS, DUM, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1180) IRET
         GO TO 990
         END IF
C                                       Close input map.
      CALL ZCLOSE (LUNI, INDI, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('HIEQRD: ERROR',I3,' OPENING INPUT FILE')
 1010 FORMAT ('Begin histogramming plane',I5,4I4)
 1099 FORMAT ('HIEQRD: COMOFF ERROR',I3)
 1100 FORMAT ('HIEQRD: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('HIEQRD: ',A,' ERROR',I3)
 1180 FORMAT ('HIEQRD: HISGET ERROR',I3)
      END
      SUBROUTINE HISGET (IPOS, DATA, IRET)
C-----------------------------------------------------------------------
C   Inputs:
C      IPOS   I(7)    BLC (input image) of first value in DATA
C                     IPOS(1) = -1 => no input data this call.
C                     IPOS(1) = -2 => last call (no input data).
C      DATA   R(*)    Input row, magic value blanked.
C   Values from commons:
C      FBLANK  R      Value of blanked pixel.
C      CATBLK  I      Output catalog header (also CATR, CATD)
C      CATOLD  I      Input catalog header (also OLD4, OLD8)
C   Output:
C      IRET    I      Return code   0 => OK
C                                  >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IRET
      REAL      DATA(*)
C
      REAL      OLD4(256), X, Y, Z, SUM, RL
      INTEGER   LROW, I, J, L
      DOUBLE PRECISION OLD8(128)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'HISEQ.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (CATOLD, OLD4, OLD8)
C-----------------------------------------------------------------------
      IRET = 0
C                                       data in this call
      IF (IPOS(1).GT.0) THEN
         Y = NHISTO / (PIXRNG(2) - PIXRNG(1))
         LROW = CATBLK(KINAX)
         DO 20 I = 1,LROW
            IF (DATA(I).NE.FBLANK) THEN
               IF ((DATA(I).GT.PIXRNG(1)) .AND. (DATA(I).LT.PIXRNG(2)))
     *            THEN
                  X = Y * (DATA(I) - PIXRNG(1)) + 1.000001
                  IF ((X.GE.1.0) .AND. (X.LE.NHISTO)) THEN
                     J = X
                     HISTOG(J) = HISTOG(J) + 1
                     NHIST = NHIST + 1
                     END IF
                  END IF
               END IF
 20         CONTINUE
C                                       last call, sum histo
      ELSE
         MSGTXT = 'Summing the histogram to make intensity ranges'
         CALL MSGWRT (2)
         NVALS = MIN (NVALS, NHIST/4)
         IF (NVALS.LT.10) THEN
            WRITE (MSGTXT,1100) NHIST
            CALL MSGWRT (8)
            IRET = 8
         ELSE
            Y = NHISTO / (PIXRNG(2) - PIXRNG(1))
            X = REAL (NHIST) / REAL (NVALS)
            Z = 0.0
            L = 1
            RL = 0.0
            DO 130 I = 1,NVALS
               SUM = 0.0
               DO 110 J = L,NHISTO
                  SUM = SUM + HISTOG(J)
                  IF (SUM.GE.X) GO TO 120
                  L = L + 1
 110              CONTINUE
               VALS(I+1) = PIXRNG(2)
               GO TO 130
C                                       get value
 120           Z = SUM - X
               RL = L - (Z / HISTOG(L)) * (L - RL)
               VALS(I+1) = PIXRNG(1) + RL / Y
               HISTOG(L) = Z
 130           CONTINUE
            END IF
            VALS(1) = PIXRNG(1)
            VALS(NVALS+2) = PIXRNG(2)
            VALS(NVALS+3) = PIXRNG(2) + 1.E9
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ONLY',I7,' PIXELS INCLUDED: NOT ENOUGH FOR HISTOGRAM')
      END
      SUBROUTINE HIEQOU (IRET)
C-----------------------------------------------------------------------
C   HIEQOU sends image one row at a time to the conversion routine and
C   then writes the modified data.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER IFILE*48
      INTEGER   IROUND, LUNI, LUNO, NYI, NXI, WINI(4), NXO, NYO,
     *   WINO(4), BOI, BOO, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2,
     *   I3, I4, I5, I6, I7, IPOS(7), CORN(7), BOTEMP, KOFF, LIMO,
     *   LIMIT, IBIND, OBIND, INDI, INDO, LIM1
      REAL      OUTMAX, OUTMIN, OLD4(256), DUM(1)
      DOUBLE PRECISION    OLD8(128)
      LOGICAL   T, F, BLNKD, DOMSG
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'HISEQ.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (CATOLD, OLD4, OLD8)
      DATA LUNI, LUNO /16,17/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'IN'
         GO TO 990
         END IF
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OUT'
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
      DOMSG = (LIM3 * LIM4 * LIM5 * LIM6 * LIM7) .GT. 5
      MSGTXT = 'Begin equalizing the data'
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = BLC(7) + I7 - 0.9
         CORN(7) = I7
         DO 600 I6 = 1,LIM6
            IPOS(6) = BLC(6) + I6 - 0.9
            CORN(6) = I6
            DO 500 I5 = 1,LIM5
               IPOS(5) = BLC(5) + I5 - 0.9
               CORN(5) = I5
               DO 400 I4 = 1,LIM4
                  IPOS(4) = BLC(4) + I4 - 0.9
                  CORN(4) = I4
                  DO 300 I3 = 1,LIM3
                     IPOS(3) = BLC(3) + I3 - 0.9
                     CORN(3) = I3
                     WRITE (MSGTXT,1010) IPOS(3), IPOS(4), IPOS(5),
     *                  IPOS(6), IPOS(7)
                     IF ((MOD(I3,10).EQ.1) .AND. (DOMSG)) CALL MSGWRT(1)
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
         DO 220 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.9
            IPOS(1) = IROUND (BLC(1))
C                                       Read.
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'READ', IRET
               GO TO 990
               END IF
C                                       Write.
            CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'WRIT', IRET
               GO TO 990
               END IF
C                                       Convert intensities
            CALL HISMAK (IPOS, BUFF1(IBIND), BUFF2(OBIND))
C                                       Check max, min, blanking.
            LIMIT = OBIND + LIMO
            DO 200 I1 = OBIND,LIMIT
               BLNKD = BLNKD .OR. (BUFF2(I1).EQ.FBLANK)
               IF (BUFF2(I1).EQ.FBLANK) GO TO 200
                  OUTMAX = MAX (OUTMAX, BUFF2(I1))
                  OUTMIN = MIN (OUTMIN, BUFF2(I1))
 200              CONTINUE
 220        CONTINUE
C                                       Flush buffer.
         CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1120) 'FINI', IRET
            GO TO 990
            END IF
C                                       Update CATBLK.
         CATR(KRDMX) = OUTMAX
         CATR(KRDMN) = OUTMIN
         CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1220) IRET
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
C                                       Final call to functions
      IPOS(1) = -2
      CALL HISMAK (IPOS, DUM, DUM)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('HIEQOU: ERROR',I3,' OPENING ',A,'PUT FILE')
 1010 FORMAT ('Begin equalizing plane',I5,4I4)
 1099 FORMAT ('HIEQOU: COMOFF ERROR',I3)
 1100 FORMAT ('HIEQOU: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('HIEQOU: ',A,' ERROR',I3)
 1220 FORMAT ('HIEQOU: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE HISMAK (IPOS, DATA, RESULT)
C-----------------------------------------------------------------------
C   converts real intensities into histogram equalized ones
C   Inputs:
C      IPOS   I(7)    BLC (input image) of first value in DATA
C                     IPOS(1) = -1 => no input data this call.
C                     IPOS(1) = -2 => last call (no input data).
C      DATA   R(*)    Input row, magic value blanked.
C   Values from commons:
C      FBLANK  R      Value of blanked pixel.
C      CATBLK  I      Output catalog header (also CATR, CATD)
C      CATOLD  I      Input catalog header (also OLD4, OLD8)
C   Output:
C      RESULT  R(*)   Output row.
C   Output in COMMON:
C      CATBLK  I          Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IPOS(7)
      REAL      DATA(*), RESULT(*)
C
      REAL      OLD4(256), X, Y1, Y2, Y
      INTEGER   I, J, LROW, MVALS, J1, J2
      DOUBLE PRECISION OLD8(128)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'HISEQ.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (CATOLD, OLD4, OLD8)
C-----------------------------------------------------------------------
C                                       Data call
      IF (IPOS(1).GT.0) THEN
         LROW = CATBLK(KINAX)
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
 30               CONTINUE
               J = MVALS + 1
 40            Y1 = VALS(J-1)
               Y2 = VALS(J)
               Y = J - 2 + (DATA(I) - Y1) / (Y2 - Y1)
               RESULT(I) = Y / (NVALS + 1.0)
               END IF
 50         CONTINUE
C                                       Last call - do history etc.
      ELSE
         END IF
C
 999  RETURN
      END
      SUBROUTINE HIEQHI
C-----------------------------------------------------------------------
C   HIEQHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER LINE*72, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR, I, IROUND, IB(7), IT(7), J, J1, J2, K
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'HISEQ.INC'
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
      DO 10 I = 1,7
         IB(I) = IROUND (BLC(I))
         IT(I) = IROUND (TRC(I))
 10      CONTINUE
      WRITE (LINE,1010) TSKNAM, 'BLC', IB
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       TRC
      WRITE (LINE,1010) TSKNAM, 'TRC', IT
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       NBOXES
      WRITE (LINE,1015) TSKNAM, NVALS
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       PIXRANGE
      WRITE (LINE,1020) TSKNAM, PIXRNG
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       record intensities!
      IF (XDOHST.GT.0.0) THEN
         LINE = TSKNAM // '/ peak intensity of each box'
         CALL HIADD (LUN2, LINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         DO 30 I = 1,NVALS,4
            J1 = I + 1
            J2 = I + 4
            K = I + 3
            WRITE (LINE,1025) TSKNAM, I, K, (VALS(J), J = J1,J2)
            CALL HIADD (LUN2, LINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 30         CONTINUE
         END IF
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
 1000 FORMAT ('HIEQHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,A,' =',6(I5,','),I5)
 1015 FORMAT (A6,'NBOXES =',I5,10X,' / number intensity ranges used')
 1020 FORMAT (A6,'PIXRANGE =',1PE12.4,',',1PE12.4)
 1025 FORMAT (A6,'VALS(',I3,'-',I4,')=',3(1PE12.5,','),1PE12.5)
      END
