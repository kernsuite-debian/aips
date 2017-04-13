LOCAL INCLUDE 'MWFLT.INC'
C                                       Local include for MWFLT
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD(256), CATBLK(256), JBUFSZ, ICODE, SCRTCH(512)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, OPCODE*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), XOPCOD
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7),
     *   CPARM(10), DPARM(10), BUFF1(MABFSS), BUFF2(MABFSS)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOU,
     *   XSEQO, XDISKO, BLC, TRC, XOPCOD, CPARM, DPARM
      COMMON /PARMS/ SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD, JBUFSZ, ICODE
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, OPCODE
      COMMON /BUFRS/ BUFF1, BUFF2, SCRTCH
      COMMON /MAPHDR/ CATBLK
LOCAL END
      PROGRAM MWFLT
C-----------------------------------------------------------------------
C! Does median window filtering of an image.
C# Map Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000-2001, 2004, 2006, 2008, 2010, 2015
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
C   MWFLT applies a nonlinear lowpass filter to an image.  Three such
C   filters are currently available.  The first one is the "mode"
C   filter, which is formed by a combination of the box filter and the
C   median window filter in an attempt to approximate the mode of the
C   skewed distribution of pixel values due to the presence of positive
C   sources above the background image.  The second option is to
C   deliver the median window result alone.  The median calculation
C   uses a two-dimensional merge sort algorithm.  The third option is
C   the alpha-trimmed mean filter, which is the mean of the distribution
C   after a specified percent of the outlying pixels have been removed.
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
C      OPCODE         OPCODE        Option MW, ALPHA,
C      CPARM(10)      CPARM         User specified array.
C      DPARM(10)      DPARM         User specified array.
C   Written  by: Thad A. Polk & Donald C. Wells
C   Modified by: Glen Langston
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGM*6
      INCLUDE 'MWFLT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'MWFLT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL MWFIN (PRGM, IRET)
C                                       Filter inage.
      IF (IRET.EQ.0) CALL MWSEND (IRET)
C                                       History
      IF (IRET.EQ.0) CALL MWFHIS
C                                       Close down files, etc.
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE MWFIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   MWFIN gets input parameters for MWFLT and creates an output file.
C   Inputs:  PRGN    C*6       Program name
C   Output:  IRET    I         Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER  STAT*4, PRGN*6, BLANK*6, MTYPE*2, ATYPES(7)*8
      HOLLERITH CATH(256)
      INTEGER   IRET
      INTEGER   IERR, NPARM, IROUND, ITEMP
      REAL      CATR(256)
      DOUBLE PRECISION    CATD(128)
      INCLUDE 'MWFLT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATR, CATH, CATBLK, CATD)
      DATA BLANK /'      '/
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
      NPARM = 49
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
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
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
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATOLD(KIDIM), CATOLD(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       check window
      ITEMP = IROUND (CPARM(1))
      IF (ITEMP.LE.0) ITEMP = 5
      IF (MOD (ITEMP,2).EQ.0) ITEMP = ITEMP + 1
      CPARM(1) = ITEMP
      ITEMP = IROUND (CPARM(2))
      IF (OPCODE.NE.'ROW') THEN
         IF (ITEMP.LE.0) ITEMP = 5
         IF (MOD (ITEMP,2).EQ.0) ITEMP = ITEMP + 1
         CPARM(2) = ITEMP
      ELSE
         IF ((ITEMP.LE.0) .OR. (ITEMP.GT.CPARM(1))) ITEMP =
     *      (CPARM(1) + 1.0) / 2.0 + 0.01
         CPARM(2) = ITEMP
         END IF
      IF ((CPARM(3).LT.0.0) .OR. (CPARM(3).GT.0.5)) CPARM(3) = 0.25
C                                       default: median sky sub.
      IF (DPARM(1).EQ.0.0 .AND. DPARM(2).EQ.0) THEN
         DPARM(1) = 1.
         DPARM(2) = -1.
      ELSE IF (OPCODE.NE.'NRML') THEN
         IF (DPARM(2).EQ.0.0) DPARM(2) = 1.0
         END IF
C                                       if coordinate increment is 1
      IF (ABS(CATR(KRCIC)) .EQ. 1.0) THEN
C                                       "/mm * mm/inch / 400 dots/inch
         CATR(KRCIC) = -(67.2 * 25.4 / 400.0) / 3600.
         END IF
C                                       if coordinate increment is 1
      IF (ABS(CATR(KRCIC+1)) .EQ. 1.0) THEN
C                                       "/mm * mm/inch / 400 dots/inch
         CATR(KRCIC+1) = (67.2 * 25.4 / 400.0) / 3600.
         END IF
C                                       Get user modification to CATBLK
      IRET = 4
      CALL MWINIT (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check if axis types blank
C                                       Ra axis
      CALL H2CHR (8, 1, CATH(KHCTP), ATYPES(1))
C                                       If blank
      IF (ATYPES(1) .EQ.'        ') THEN
C                                       Reset to arc projection
         CALL CHR2H (8, 'RA---ARC', 1, CATH(KHCTP))
         END IF
C                                       Dec axis
      CALL H2CHR (8, 1, CATH(KHCTP+2), ATYPES(2))
C                                       If blank
      IF (ATYPES(2) .EQ.'        ') THEN
C                                       Reset to arc projection
         CALL CHR2H (8, 'DEC--ARC', 1, CATH(KHCTP+2))
         END IF
C                                       Create output file.
      NEWCNO = 1
      IRET = 4
      CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 2
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MWFIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('MWFIN: ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE MWSEND (IRET)
C-----------------------------------------------------------------------
C   MWSEND sends image one row at a time to the user supplied
C   routine and then writes the modified data.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C                                    9 => Image row too long
C-----------------------------------------------------------------------
      CHARACTER IFILE*48
      INTEGER   IRET, IROUND, LUNI, LUNO, NYI, NXI,
     *   WINI(4), NXO, NYO, WINO(4), BOI,
     *   BOO, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2,
     *   I3, I4, I5, I6, I7, IPOS(7), CORN(7), BOTEMP, KOFF,
     *   LIMO, LIMIT, IBIND, OBIND, INDI, INDO, LIM1
      INTEGER   DIDRET, OUTCNT
      REAL      OUTMAX, OUTMIN, CATR(256), OLDR(256)
      DOUBLE PRECISION    OLDD(128), CATD(128)
      LOGICAL   T, F, BLNKD
      INCLUDE 'MWFLT.INC'
      REAL    RDATA(MAXIMG)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR, CATD),         (CATOLD, OLDR, OLDD)
      SAVE IPOS
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
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
C                                       If too big, tell user
      IF (LIM1.GT.MAXIMG) THEN
         MSGTXT = 'MWSEND: IMAGE BIGGER THAN ARRAY !!!!'
         CALL MSGWRT(8)
         MSGTXT = 'MWSEND: CHOOSE A SMALLER SUB-IMAGE !'
         IRET = 9
         GO TO 990
         END IF
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
         CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2, JBUFSZ,
     *      BOO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'WRIT', IRET
            GO TO 990
            END IF
         DIDRET = 1
         OUTCNT = LIM2
C                                       For all rows loop
         DO 220 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.9
            IPOS(1) = IROUND (BLC(1))
C                                       Read.
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'READ', IRET
               GO TO 990
               END IF
C                                       Copy data to array
            CALL RCOPY (LIM1, BUFF1(IBIND), RDATA(1))
C                                       Write.
C                                       Check for deferred output.
            IF (DIDRET.GE.0) THEN
               CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
               OBIND = OBIND - 1
               OUTCNT = OUTCNT - 1
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1120) 'WRIT', IRET
                  GO TO 990
                  END IF
C                                       End IF time to write
               END IF
C                                       Call FILTER.
            IF (DIDRET.GE.0) OBIND = OBIND + 1
C                                       Do quick row median ?
            IF (OPCODE.EQ.'ROW ') THEN
               CALL ROWMED (IPOS, RDATA, BUFF2(OBIND), IRET)
C                                       Else do area filter
            ELSE
               CALL FILTER (IPOS, RDATA, BUFF2(OBIND), IRET)
               END IF
            DIDRET = IRET
            IF (DIDRET.GT.0) THEN
               WRITE (MSGTXT,1180) IRET
               GO TO 990
C                                       do some output now
            ELSE IF (DIDRET.EQ.0) THEN
C                                       Check max, min, blanking.
               LIMIT = OBIND + LIMO
C                                       for all columns in out row
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
C                                       from FILTER.
         IF (OUTCNT.GT.0) THEN
            DO 260 I2 = 1,OUTCNT
               IPOS(1) = -1
C                                       Check if write requested.
               CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
               OBIND = OBIND - 1
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1120) 'WRIT', IRET
                  GO TO 990
                  END IF
C                                       Call FILTER
               OBIND = OBIND + 1
C                                       Do quick row median ?
               IF (OPCODE.NE.'ROW ') THEN
C                                       Else do area filter
                  CALL FILTER (IPOS, RDATA, BUFF2(OBIND), IRET)
                  END IF
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
 250              CONTINUE
 260           CONTINUE
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
         CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', RDATA, IRET)
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
C                                       Final call to functions
      IPOS(1) = -2
C                                       Do quick row median ?
      IF (OPCODE.EQ.'ROW ') THEN
         CALL ROWMED (IPOS, RDATA, BUFF2, IRET)
      ELSE
C                                       Else do area filter
         CALL FILTER (IPOS, RDATA, BUFF2, IRET)
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1180) IRET
         GO TO 990
         END IF
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MWSEND: ERROR',I3,' OPENING INPUT FILE')
 1020 FORMAT ('MWSEND: ERROR',I5,' OPENING OUTPUT FILE')
 1099 FORMAT ('MWSEND: COMOF3 ERROR',I3)
 1100 FORMAT ('MWSEND: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('MWSEND: ',A4,' ERROR',I3)
 1180 FORMAT ('MWSEND: FILTER ERROR',I3)
 1260 FORMAT ('MWSEND: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE MWFHIS
C-----------------------------------------------------------------------
C   MWFHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR, I
      LOGICAL   T
      INCLUDE 'MWFLT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
C                                       copy most header keywords
      CALL KEYPCP (DISKIN, OLDCNO, DISKO, NEWCNO, 0, ' ', IERR)
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   SCRTCH(257), SCRTCH, IERR)
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
      WRITE (HILINE,2000) TSKNAM, BLC
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       TRC
      WRITE (HILINE,2001) TSKNAM, TRC
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       OPCODE
      WRITE (HILINE,2002) TSKNAM, OPCODE
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       window
      I = CPARM(1) + 0.01
      WRITE (HILINE,2003) TSKNAM, I
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
      I = CPARM(2) + 0.01
      IF (OPCODE.NE.'ROW') THEN
         WRITE (HILINE,2004) TSKNAM, I
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 20
      ELSE
         WRITE (HILINE,2005) TSKNAM, I
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
      IF ((OPCODE.EQ.'ALFA') .OR. (OPCODE.EQ.'NRML')) THEN
         WRITE (HILINE,2006) TSKNAM, CPARM(3)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
      IF (OPCODE.NE.'NRML') THEN
         WRITE (HILINE,2007) TSKNAM, DPARM(1)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 20
         WRITE (HILINE,2008) TSKNAM, DPARM(2)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 20
      ELSE
         WRITE (HILINE,2009) TSKNAM, DPARM(1)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 20
         WRITE (HILINE,2010) TSKNAM, DPARM(2)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 20
         WRITE (HILINE,2011) TSKNAM, DPARM(3)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                       Close HI file
 20   CALL HICLOS (LUN2, T, SCRTCH, IERR)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, SCRTCH(257), SCRTCH, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MWFHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,'TRC =',6(F6.0,','),F6.0)
 2002 FORMAT (A6,'OPCODE = ''',A4,'''')
 2003 FORMAT (A6,'CPARM(1) =',I4,'  / MW window width')
 2004 FORMAT (A6,'CPARM(2) =',I4,'  / MW window height')
 2005 FORMAT (A6,'CPARM(2) =',I4,'  / Row pixel level')
 2006 FORMAT (A6,'CPARM(3) =',F5.2,'  / Window cutoff fraction')
 2007 FORMAT (A6,'DPARM(1) =',F5.2,'  / Output=dparm(1)*input +')
 2008 FORMAT (A6,'DPARM(2) =',F5.2,'  / dparm(2)*filter_value')
 2009 FORMAT (A6,'DPARM(1) =',F9.4,'  / Maximum rms out')
 2010 FORMAT (A6,'DPARM(2) =',F9.4,'  / Minumum rms out')
 2011 FORMAT (A6,'DPARM(3) =',F5.2,'  / Weight sqrt(background)')
      END
      SUBROUTINE MWINIT (IRET)
C-----------------------------------------------------------------------
C   MWINIT is a routine in which the user performs several operations
C   associated with beginning the task.  The following functions are
C   performed in MWINIT:
C       1) Modifying the catalog header block to represent the
C   output file.
C       2) Checking the input image and input parameters.
C   A returned value of IRET .NE. 0 will cause the task to terminate.
C       3) Setting default values of some of the input parameters
C   (OUTNAME, OUTCLASS, OUTSEQ, OUTDISK, TRC and BLC defaults are
C   set elsewhere).  As currently set the default OPCODE is the
C   first value in the array CODES which is set in a data statment.
C
C    Input:
C     CATBLK(256)    I     Output catalog header, also CATR, CATD
C     CATOLD(256)    I     Input catalog header, also OLDR, OLDD
C    Output:
C     CATBLK(256)    I     Modified output catalog header.
C     IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NCODE
C                                       # and value of OPCODEs
      PARAMETER (NCODE=7)
      CHARACTER FCHARS(3)*4, CHTMP*4, CODES(NCODE)*4
      HOLLERITH CATH(256)
      INTEGER   LIMIT, I, FIRSTI, FIRSTO, IRET, INDEX
      REAL      CATR(256), OLDR(256)
      DOUBLE PRECISION    CATD(128), OLDD(128)
      INCLUDE 'MWFLT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE  (CATBLK, CATR, CATD, CATH),   (CATOLD, OLDR, OLDD)
      DATA FCHARS /'FREQ','VELO','FELO'/
      DATA CODES /'MW','MAX','MIN','ALFA','NRML','MODE','ROW'/
C-----------------------------------------------------------------------
C                                       Set default OPCODE
      DO 10 I = 1,NCODE
         ICODE = I
         IF (OPCODE.EQ.CODES(I)) GO TO 50
 10      CONTINUE
C                                       Default OPCODE is first.
      OPCODE = CODES(1)
      ICODE = 1
C                                       Set axes in output CATBLK.
 50   FIRSTI = -1
      FIRSTO = -1
      LIMIT = CATOLD(KIDIM)
C                                       Copy/update axis values
      DO 80 I = 1,LIMIT
         CATBLK(KINAX+I-1) = TRC(I) - BLC(I) + 1.01
         CATR(KRCRP+I-1) = CATR(KRCRP+I-1) - BLC(I) + 1.0
         CATR(KRCIC+I-1) = CATR(KRCIC+I-1)
         CATD(KDCRV+I-1) = CATD(KDCRV+I-1)
         INDEX = KHCTP + (I-1) * 2
         CALL CHCOPY (8, 1, OLDR(INDEX), 1, CATH(INDEX))
         IF (CATBLK(KIALT).EQ.0) GO TO 80
            CALL H2CHR (4, 1, CATH(INDEX), CHTMP)
            IF ((CHTMP.EQ.FCHARS(1)) .OR. (CHTMP.EQ.FCHARS(2)) .OR.
     *         (CHTMP.EQ.FCHARS(3))) CATR(KRARP) = CATR(KRARP) -
     *         BLC(I) + 1.0
 80      CONTINUE
C                                       Put other checks here.
C                                       Finished.
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE FILTER (IPOS, RDATA, RESULT, IRET)
C-----------------------------------------------------------------------
C   This subroutine performs selected filtering operations on an
C   image one row at a time (1st dimension).
C       Input RDATA are Real with blanking if necessary; output values
C   are R   which may also be blanked.  The calling routine keeps track
C   of max., min. and the occurence of blanking.
C   NOTE: blanked values are denoted by the value of the common variable
C   FBLANK.
C       FILTER accumulates a scrolling buffer by returning a negative
C   value of IRET.  This tells the calling routine to defer writing the
C   next row.  When rows are deferred an equal number of calls to
C   FILTER will be made with no input data; this allows reading out any
C   rows left in FILTERs internal buffers.  Such a "no input call" is
C   indicated by a value of IPOS(1) of -1.  The writing of the returned
C   values of these "no input calls" is NOT deferred.
C
C       When IRET .GT. 0 the output file will be destroyed.
C
C       The current contents of CATBLK will be written back to the
C   catalog after the last call to FILTER.
C
C   Inputs:
C      IPOS(7)   I    BLC (input image) of first value in RDATA
C                     IPOS(1) = -1 => no input data this call.
C                     IPOS(2) = -2 => last call (no input data).
C      RDATA(*)   R    Input row, magic value blanked.
C   Values from commons:
C      ICODE     I    Opcode number from list in MWINIT.
C      FBLANK    R    Value of blanked pixel.
C      CPARM(10) R    Input adverb array.
C      DPARM(10) R    Input adverb array.
C      CATBLK    I    Output catalog header
C      CATOLD    I    Input catalog header (also OLDR)
C   Output:
C      RESULT(*) R    Output row.
C      IRET      I    Return code   0 => OK
C                               >0 => error, terminate.
C   Output in COMMON
C     CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
C                                       Formal parameters:
      INTEGER   IRET, IPOS(7)
      REAL      RDATA(*), RESULT(*)
C                                       Local variables:
      INCLUDE 'MWFLT.INC'
      INTEGER   MWLIM, MWLIM2, MWLIMS
      PARAMETER (MWLIM2 = 101)
      PARAMETER (MWLIM = 2 * MWLIM2 - 1)
      PARAMETER (MWLIMS = MWLIM * MWLIM)
      INTEGER   CSIZE(MAXIMG), NA, NB, NC, RIGHT, NEWSIZ, IBLC1, INC,
     *   NBUFF(MWLIM,MAXIMG), NTEMP(MWLIM), COUNT(MWLIMS,2), LROW,
     *   MWX, MWY, MW2X, MW2Y, KA, KC, KT, I, K, I2, IC, N
      LOGICAL   FIRST, PRNTD
      REAL      BUFF(MWLIM,MAXIMG), CUT1L, CUT2L, CUT1, CUT2,
     *   TEMP(MWLIM), AC(MWLIMS,2), OUTPUT(MAXIMG),
     *   OLDATA(MAXIMG,MWLIM2), OLDR(128), SIGOUT, INIT, SIGIN, DATAMN,
     *   DATAMX
      SAVE LROW, IBLC1, MWX, MWY, MW2X, MW2Y, I2, KA, KC, DATAMN,
     *   DATAMX, CUT1L, CUT2L, COUNT, CSIZE, NBUFF, PRNTD, FIRST
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATOLD, OLDR)
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if last call (no input
C                                       data).
      IF (IPOS(1).LE.-2) GO TO 900
C                                       Check if first call per plane.
      IF (IPOS(2).LE.BLC(2)) THEN
C                                       First call per plane.
C                                       Initialize variables.
         LROW = (TRC(1) - BLC(1) + 1.01)
         IBLC1 = BLC(2) + 0.1
         MWX   = CPARM(1)
         MWY   = CPARM(2)
         INC = 1.E6 / (MWX * MWY * (TRC(1) - BLC(1) + 1)) + 0.5
         INC = MAX (1, INC) * 10
C                                       Invalid input, die.
         IF ((MWX.GT.MWLIM) .OR. (MWX.LE.0) .OR. (MWY.GT.MWLIM) .OR.
     *      (MWY.LE.0) .OR. (MOD(MWX,2).NE.1) .OR. (MOD(MWY,2).NE.1))
     *      THEN
            WRITE (MSGTXT,1000) MWLIM
            CALL MSGWRT (8)
            IRET = 1
            GO TO 900
            END IF
         MW2X  = MWX  / 2
         MW2Y  = MWY  / 2
         I2    = LROW + MW2X
         KA    = 1
         KC    = 2
         DATAMN = OLDR(KRDMN)
         DATAMX = OLDR(KRDMX)
         CUT1L = DATAMN
         CUT2L = DATAMX
         DO 20 I = 1,MWLIM
            COUNT(I,1) = 0
            COUNT(I,2) = 0
 20         CONTINUE
         DO 30 I = 1,LROW
            CSIZE(I) = 0
            NBUFF(1,I) = 0
 30         CONTINUE
         DO 40 I = 1,MW2Y
            CALL RFILL (LROW, FBLANK, OLDATA(1,I+1))
 40         CONTINUE
         PRNTD = .FALSE.
         FIRST = .TRUE.
C                                       End If first call
         END IF
C                                       Print out row number every
C                                        tenth row if appropriate:
      IF ((MOD(IPOS(2),INC).NE.0) .OR. (IPOS(2).LT.IBLC1) .OR.
     *   (IPOS(2).GT.TRC(2)) .OR. (CPARM(4).LE.0.0) .OR. (PRNTD))
     *   GO TO 110
         WRITE (MSGTXT,1010) IPOS(2)
         CALL MSGWRT (4)
         IF (IPOS(2).EQ.TRC(2)) PRNTD = .TRUE.
C                                       Copy each data vector into
C                                        the vector below it:
 110  DO 120 I = 1,MW2Y
         CALL RCOPY (LROW, OLDATA(1,I+1), OLDATA(1,I))
 120     CONTINUE
C                                       Check if no input data:
      IF (IPOS(1).LT.0) THEN
C                                       No input, copy blanks into
C                                       buffer array
         CALL RFILL (LROW, FBLANK, OLDATA(1,MW2Y+1))
      ELSE
C                                       Copy data into buffer array
         CALL RCOPY (LROW, RDATA, OLDATA(1,MW2Y+1))
         END IF
C                                       Merge the new pixels with
C                                        the old column lists:
      DO 160 I = 1,LROW
         NB = 1
         IF ((RDATA(I).EQ.FBLANK) .OR. (IPOS(1).EQ.-1)) NB = 0
         CALL MWMERG (MWY, CSIZE(I), BUFF(1,I), NBUFF(1,I), NB,
     *      RDATA(I), NEWSIZ, TEMP(1), NTEMP(1))
         DO 150 K= 1,NEWSIZ
            BUFF(K,I) = TEMP(K)
            NBUFF(K,I) = NTEMP(K)
 150        CONTINUE
         CSIZE(I) = NEWSIZ
 160     CONTINUE
C                                       If col position in range
      IF (IPOS(2).GE.IBLC1+MW2Y) THEN
         CUT1 = CUT1L
         CUT2 = CUT2L
         NC   = 0
C                                       Loop to move window along row:
         DO 220 I= 1,I2
            IC = I - MW2X
C                                       Swap merge buffers:
            KT = KA
            KA = KC
            NA = NC
            KC = KT
            NC = 0
            NB = 0
            IF (I.LE.LROW) NB = CSIZE(I)
            RIGHT = MIN (I, LROW)
C                                       Merge next column into window:
            CALL MWMERG (MWX, NA, AC(1,KA), COUNT(1,KA), NB,
     *         BUFF(1,RIGHT), NC, AC(1,KC),COUNT(1,KC))
C                                       Get appropriate result from
C                                        window:
            IF (IC.GE.1) THEN
               N = 1
               IF ((IC.EQ.1) .AND. (IPOS(2).EQ.IBLC1+MW2Y)) N = 5
               DO 170 K = 1,N
                  CALL GTMODE (NC, AC(1,KC), CUT1, CUT2, OUTPUT(IC),
     *               SIGIN, IRET, FIRST)
                  FIRST = .FALSE.
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1020) IRET
                     CALL MSGWRT (8)
                     GO TO 999
                     END IF
 170              CONTINUE
C                                       If Normalized
               IF (OPCODE.EQ.'NRML') THEN
                  SIGOUT = (DPARM(1) - DPARM(2))
     *               * ABS(OUTPUT(IC)) / DATAMX + DPARM(2)
C                                       scale background
                  INIT = DPARM(3) * SQRT (ABS(OUTPUT(IC)/DATAMX))
                  IF (OUTPUT(IC).LT.0.0) INIT = -INIT
                  IF (SIGIN.EQ.0.0) THEN
                     SIGOUT = 1.0
                     SIGIN = 1.0
                     END IF
                     RESULT(IC) =((OLDATA(IC, 1)-OUTPUT(IC)) *
     *                  SIGOUT/SIGIN) + INIT
               ELSE
C                                       Else Median Mode or ALPHA
C                                       Factor Median or Mode
                  RESULT(IC) = DPARM(2) * OUTPUT(IC)
C                                       If not blanked, add data back
                  IF ((IPOS(2).GE.IBLC1+MW2Y) .AND.
     *                 (RESULT(IC).NE.FBLANK))
     *            RESULT(IC) = RESULT(IC) + DPARM(1)*OLDATA(IC, 1)
C                                       End if Normalized
                  END IF
            ELSE
                CUT1L = CUT1
                CUT2L = CUT2
                END IF
 220        CONTINUE
C
C                                       Defer call - set IRET to -1
      ELSE
         IRET = -1
         END IF
C                                       Last call - do history etc.
 900  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WINDOW DIMENSION MUST BE ODD AND BETWEEN 1 AND',I3)
 1010 FORMAT ('Currently on row number', I6)
 1020 FORMAT ('ERROR GETTING RESULT FROM GTMODE, IRET=',I5)
      END
      SUBROUTINE MWMERG (MW, NA, A, IA, NB, B, NC, C, IC)
C-----------------------------------------------------------------------
C Merges two sorted lists of pixel values into a combined sorted list.
C Input:
C    MW    = height of the column (limit on value of "counts")
C    NA    = length of previously merge-sorted array A().
C    A()   = previously merge-sorted pixel values (ascending order).
C    IA()  = integer "counts" associated with A().
C    NB    = length of the array of input pixel values.
C    B()   = input pixels (FP) sorted into ascending order.
C    NC    = length of array C() which receives A() merged with B().
C Output:
C    C()   = merged output array, will be in ascending order.
C    IC()  = integer counts associated with C().
C
C The values which have been through the process too many times are
C discarded. A() and B() are the input
C lists, and C() is the output. NA, NB, AND NC are the number of
C elements in the lists (NC is computed by this routine and need not be
C initialized). The A() list is the old input, and the B() list is new
C items. The process should start up by adding a B() list to an empty
C A() list (i.e., one with NA=0). The B() list should contain floating
C point numbers in ascending order. Both NA=0 and NB=0 are legal, and
C NC=0 is quite possible.
C      Arrays IA() and IC() carry "counts" which are associated with
C the corresponding elements of A() and C(). Counts from IA() are
C incremented as they and their values in A() are copied to IC() and
C C(), except that when they exceed MW the count and value are
C discarded (not copied into IC() and C()). When a B() list item enters
C the process its count is set to 1. The counts remember how many cycles
C ago each item entered the merge sorting process. This is the mechanism
C by which we determine when the pixel associated with a given item
C moves out of the median window.
C-----------------------------------------------------------------------
C                                       Formal parameters:
      INTEGER   MW, NA, IA(1), NB, NC, IC(1)
      REAL      A(1), B(1), C(1)
C                                       Local variables:
      INTEGER   I, J, K, IAI
      REAL      AI, BJ
C-----------------------------------------------------------------------
      I = 0
      J = 0
      K = 0
C                                       Initialize AI and IAI if A()
C                                        is not empty:
 10   CONTINUE
         I = I + 1
         IF (I.GT.NA) GO TO 70
         IAI = IA(I)
         IAI = IAI + 1
         IF (IAI.GT.MW) GO TO 10
      AI = A(I)
C                                       Main merge loop:
 20   CONTINUE
         J = J + 1
         IF (J.GT.NB) GO TO 80
         BJ = B(J)
C                                       Basic comparison of A(I)
C                                        with B(J):
 30      CONTINUE
            K = K + 1
            IF (BJ.LT.AI) GO TO 50
C                                       A(I) is smaller:
               C(K)  = AI
               IC(K) = IAI
 40            CONTINUE
                  I = I + 1
                  IF (I.GT.NA) GO TO 60
                  IAI = IA(I)
                  IAI = IAI + 1
                  IF (IAI.GT.MW) GO TO 40
               AI = A(I)
               GO TO 30
C                                       Else, B(J) is smaller:
 50         CONTINUE
               C(K)  = BJ
               IC(K) = 1
               GO TO 20
C                                       A() is empty, copy rest of B()
C                                        to C():
 60   CONTINUE
         K     = K + 1
         C(K)  = BJ
         IC(K) = 1
 70      CONTINUE
         J = J + 1
         IF (J.GT.NB) GO TO 100
         BJ = B(J)
         GO TO 60
C                                       B() is empty, copy rest of A()
C                                        to C():
 80   CONTINUE
         K     = K + 1
         C(K)  = AI
         IC(K) = IAI
 90      CONTINUE
            I = I + 1
            IF (I.GT.NA) GO TO 100
            IAI = IA(I)
            IAI = IAI + 1
            IF (IAI.GT.MW) GO TO 90
         AI = A(I)
         GO TO 80
C                                       That's all, folks:
 100  CONTINUE
      NC = K
      RETURN
      END
      SUBROUTINE GTMODE (NC, C, CUT1, CUT2, ZOUT, ZSIG, IRET, FIRST)
C-----------------------------------------------------------------------
C   Get the mode and median of a sorted list of numbers.
C   Input:
C      NC      = number elements in the sorted list of pixel values C().
C      C()     = list of pixel values from the window, ascending order.
C      CUT1    = current lower chauvenet cutoff (input and output).
C      CUT2    =    "    upper    "        "    (  "    "    "   ).
C   Output:
C      ZOUT    = mode (or median) of the distribution (output variable)
C      ZSIG    = r.m.s. of the distribution (output variable).
C      IRET    = Return code.
C      FIRST   = Indicates whether this is the first call.
C
C numbers C(), with numbers less than CUT1 or greater than CUT2 ignored.
C The mean and sigma are calculated so that the estimates of CUT1 and
C CUT2 can be updated (they will be generally be used again by the next
C call to this routine). IRET is the return code.  A value of IRET that
C is equal to zero implies that everything is fine while a positive
C value indicates an error and the output file will be destroyed.
C      NOTE: Variables S and SS are declared as REAL*8 because
C they are used in the formula which differences the sum of the
C values and the sum of the squares of the values in order to
C compute the r.m.s. of the distribution. This calculation
C should carry extra bits in its mantissa to preserve precision.
C      GTMODE uses the "3 and 2 rule" to estimate the mode of a
C distribution. This technique is only an approximation, and its
C efficiency as an estimator is rather poor. Its virtue is that the
C approximation is rather good for the kinds of distributions we
C usually encounter, so that its output has small bias compared to
C almost all other estimators. Obviously we need to devise a more
C efficient algorithm which could be substituted inside GTMODE.
C-----------------------------------------------------------------------
C                                       The formal parameters:
      INTEGER   NC, IRET
      LOGICAL   FIRST
      REAL      C(*), CUT1, CUT2, ZOUT, ZSIG
C                                       Local variables:
      INTEGER   ICZ, NZ, OLDNZ, I, NLOOP, OUT
      REAL      Z, W, ZBAR, ZMED, ZMODE, CUT, R
      DOUBLE PRECISION    S, SS
      INCLUDE 'MWFLT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      ZSIG = 1.0E6
      NLOOP = 0
      IF ((OLDNZ.EQ.0).OR.FIRST) OLDNZ = 1
C                                       Check the arguments:
      IF (((OPCODE.NE.'ALFA').AND.(OPCODE.NE.'NRML')).OR.
     *    ((CPARM(3).GE.0).AND.(CPARM(3).LE.0.5))) GO TO 2
             WRITE (MSGTXT,1010)
             CALL MSGWRT (8)
             IRET = 1
             GO TO 90
 2    CONTINUE
      IF (NC.NE.0) GO TO 3
         ZOUT = FBLANK
         GO TO 90
 3    IF (NC.NE.1) GO TO 4
         ZOUT = C(1)
         GO TO 90
 4    CONTINUE
      IF ((OPCODE.NE.'ALFA').AND.(OPCODE.NE.'NRML')) GO TO 5
         OUT  = NINT(NC*CPARM(3))
         CUT1 = C(OUT+1)
         CUT2 = C(NC-OUT-1)
 5    CONTINUE
      IF (CUT1.LE.CUT2) GO TO 6
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
         IRET = 1
         GO TO 90
 6    CONTINUE
      IF (ICODE.GT.3) GO TO 10
C                                       Set up median filter:
         ICZ = 0
         NZ  = NC
         GO TO 50
C                                       Apply rejection cuts,
C                                        accumulate sums:
 10   CONTINUE
      S  = 0.0D00
      SS = 0.0D00
      ICZ= -1
      NZ = 0
      DO 20 I = 1, NC
         Z = C(I)
         IF (Z.LT.CUT1) GO TO 20
         IF (Z.GT.CUT2) GO TO 30
            IF (ICZ.EQ.-1) ICZ = I - 1
            S  = S  + Z
            SS = SS + (Z * Z)
            NZ = NZ + 1
 20      CONTINUE
 30   CONTINUE
C                                       If insufficient data in sample,
C                                        relax rejection cuts:
      IF ((NZ.GT.MIN ((0.3 * NC), 20.0)).AND.
     *    (((OLDNZ-NZ)/OLDNZ.LT.0.1).OR.FIRST)) GO TO 40
         CUT   = (CUT2 - CUT1) * 0.1
         IF (CUT.GT.0.0) GO TO 35
            NLOOP = NLOOP + 1
            CUT1 = C(1)
            CUT2 = C(NC)
            IF (NLOOP.LT.2) GO TO 10
               GO TO 40
 35      CONTINUE
         CUT1  = CUT1 - CUT
         CUT2  = CUT2 + CUT
         GO TO 10
 40   CONTINUE
C                                       Compute mean and r.m.s.:
      W    = NZ
      ZBAR = (S / W)
      IF (OPCODE.NE.'ALFA') GO TO 45
         ZOUT = ZBAR
         GO TO 90
 45   CONTINUE
      IF (NZ.GT.1) ZSIG = SQRT (ABS((SS / W) - (ZBAR * ZBAR)))
C                                       Compute median:
 50   CONTINUE
      I    = ICZ + (NZ / 2)
      ZMED = C(I+1)
      IF (MOD (NZ, 2).EQ.0) ZMED = 0.5 * (C(I) + ZMED)
C                                       If median window filter:
      IF (ICODE.EQ.1) THEN
         ZOUT = ZMED
C                                       max
      ELSE IF (ICODE.EQ.2) THEN
         ZOUT = C(NC)
C                                       min
      ELSE IF (ICODE.EQ.3) THEN
         ZOUT = C(1)
C                                       Use 3-and-2 approximation
C                                       to estimate mode:
      ELSE
         ZMODE = (3.0 * ZMED) - (2.0 * ZBAR)
C                                       Update rejection cuts to be
C                                        used for next GTMODE execution:
         R    = LOG10 (W)
         R    = ((-0.1042 * R) + 1.1695) * R + 0.8895
         R    = MAX (R, 2.0)
         CUT  = (R * ZSIG) + (0.5 * ABS (ZBAR - ZMODE))
         CUT1 = ZMODE - CUT
         CUT2 = ZMODE + CUT
         ZOUT = ZMODE
         OLDNZ = NZ
         END IF
C                                       That's all, folks:
 90   CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('THE LOWER CUTOFF IS GREATER THAN THE UPPER.')
 1010 FORMAT ('CPARM(3) MUST BE BETWEEN 0 AND 0.5')
      END
      SUBROUTINE ROWMED (IPOS, RDATA, RESULT, IRET)
C-----------------------------------------------------------------------
C   ROWMED finds the median value of a region of pixels on a row
C   Inputs:
C      IPOS   I(7)    BLC (input image) of first value in RDATA
C                     IPOS(1) = -1 => no input data this call.
C                     IPOS(2) = -2 => last call (no input data).
C      RDATA   R(*)    Input row, magic value blanked.
C   Values from commons:
C      ICODE   I      Opcode number from list in NEWHED.
C      FBLANK  R      Value of blanked pixel.
C      CPARM   R(10)  Input adverb array.
C      CATBLK  I      Output catalog header (also CATR, CATD)
C      CATOLD  I      Input catalog header (also OLD4, OLD8)
C      DROP1   L      True if one output value per call.
C   Output:
C      RESULT  R(*)   Output row.
C      IRET    I      Return code   0 => OK
C                                  >0 => error, terminate.
C   Output in COMMON:
C     NUMHIS  I          # history entries (max. 10)
C     HISCRD  C(NUMHIS)  History records
C     CATBLK  I          Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IRET
      REAL      RDATA(*), RESULT(*)
C
      INTEGER   SKY, HLFWID, ICOL, LROW, JSTRT, JEND, WIDTH, IDELT
      INTEGER   CURWID, ISKY, JCOL, KCOL, BSATUR, ESATUR, NSAT
      REAL      FWIDTH, FMEDIA, SATUR, LSTMED, SATFAC, SATVAL, MEDFAC
      LOGICAL   ONZE, DOSATR
      SAVE SKY, ISKY, HLFWID, LROW, WIDTH, IDELT, ONZE, DOSATR,
     *     SATUR, SATVAL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'MWFLT.INC'
      REAL     SLIST(MAXIMG)
      DATA     ONZE/.FALSE./
C----------------------------------------------------------------------
      IRET = 0
      LSTMED = 0
C                                       Check if last call
      IF (IPOS(1).LE.-2) GO TO 999
C                                       Check if first call per plane.
      IF (IPOS(2).LE.1 .OR. .NOT.ONZE) THEN
C                                       First call per plane.
C                                       Set length of a row
         LROW = CATBLK(KINAX)
C                                       Get median full width
         WIDTH = CPARM(1)
         IF (WIDTH.LT.1) WIDTH = MAX(LROW / 15, 2)
C                                       Set median half width
         HLFWID=WIDTH/2
C                                       Make Width Odd
         WIDTH=(2*HLFWID)+1
         FWIDTH = 1./FLOAT(WIDTH)
C                                       Get Median selection value
         SKY = CPARM(2)
         IF (SKY.LT.1 .OR. SKY.GT.WIDTH) SKY = HLFWID+1
C                                       Tell User
         WRITE(MSGTXT,1000) WIDTH, SKY
         CALL MSGWRT(3)
C                                       Invert meaning of SKY
         SKY = WIDTH + 1 - SKY
C                                       NO less than smallest
         SKY = MAX (SKY, 1)
C                                       If not skipping pixels
         IF (CPARM(5).LT.2. .OR. CPARM(5).GT.LROW) THEN
            IDELT = 1
         ELSE
            IDELT = CPARM(5)
            WRITE (MSGTXT, 1050) IDELT
            CALL MSGWRT(3)
            END IF
C                                       If handling saturation
         SATUR  = CPARM(6)
         SATVAL = CPARM(7)
C                                       If staturation test
         IF (ABS(SATUR).GT.0.000001) THEN
            DOSATR = .TRUE.
            WRITE (MSGTXT,1070) SATUR, SATVAL
            CALL MSGWRT(3)
         ELSE
            DOSATR = .FALSE.
            END IF
C                                       Scale factor for median
         IF (DPARM(1).EQ.0.0) DPARM(1) = 1.0
         IF (DPARM(2).EQ.0.0) DPARM(2) = 1.0
         MEDFAC = DPARM(2)/DPARM(1)
C                                       Init Only Once
         ONZE = .TRUE.
C                                       End if first call
         END IF
C                                       Rescale output
      DO 50 ICOL = 1, LROW
         RDATA(ICOL) = RDATA(ICOL) * DPARM(1)
 50      CONTINUE
C                                       If saturation adjustment
      IF (DOSATR) THEN
C                                       Zero start count
         BSATUR = 0
C                                       Do Row Saturation
         DO 100 ICOL = 1, LROW
C                                       If staturated
            IF (RDATA(ICOL).GE.SATUR .AND. ICOL .LT. LROW) THEN
C                                       If starting a count
               IF (BSATUR .EQ. 0) BSATUR = ICOL
C                                       Continuuing a count
               ESATUR = ICOL
            ELSE
C                                       Else not staturated
C                                       If counting pixels
               IF (BSATUR .GT. 0) THEN
C                                       Number of staturated pixels
                  NSAT = ESATUR - BSATUR + 1
C                                       For all staturated pixels
                  DO 120 JCOL = 1, NSAT
C                                       factor Add to pixel
                     IF (JCOL.GT.NSAT/2) THEN
                        SATFAC = FLOAT(NSAT-JCOL)/2.
                     ELSE
                        SATFAC = FLOAT(JCOL-1)/2.
                        END IF
                     KCOL   = JCOL+BSATUR-1
                     RDATA(KCOL) = RDATA(KCOL) + (SATFAC * SATVAL)
 120                 CONTINUE
C                                       Mark end of count
                  BSATUR = 0
C                                       End if was counting saturations
                  END IF
               END IF
C                                       End for all pixels loop
 100        CONTINUE
C                                       End if doing saturation test
         END IF
C                                       For all pixels in row
      DO 200 ICOL = 1, LROW, IDELT
C
         JSTRT = MAX(1, ICOL-HLFWID)
         JEND  = MIN(LROW, ICOL+HLFWID)
C                                       Count pixels used
         CURWID = 0
C                                       Copy only pixels needed
         DO 150 JCOL = ICOL-HLFWID, JEND, IDELT
            IF (JCOL.GE.1) THEN
C                                       If not saturated
               IF (RDATA(JCOL).LT.SATUR.OR.(.NOT.DOSATR)) THEN
C                                       Count pixels to sort
                  CURWID = CURWID + 1
                  SLIST(CURWID) = RDATA(JCOL)
                  END IF
               END IF
 150        CONTINUE
C                                       Not all pixels available
         ISKY = SKY * FLOAT(CURWID) * FWIDTH
         ISKY = MAX (ISKY, 1)
C                                       If Pixels in median
         IF (CURWID.GT.0) THEN
C                                       Sort into decending order
            CALL RSORT(CURWID, SLIST)
C                                       Scale Median
            FMEDIA = MEDFAC*SLIST(ISKY)
         ELSE
C                                       Else use previous median
            FMEDIA = LSTMED
            END IF
C                                       Copy set range of values
         DO 160 JCOL = ICOL, ICOL+IDELT-1
C                                       Add scaled median
            RESULT(JCOL) = RDATA(JCOL) + FMEDIA
 160        CONTINUE
C                                       Record Last values
         LSTMED = FMEDIA
C                                       End all columns in row
 200     CONTINUE
C                                       Report Progress
      WRITE (MSGTXT,1100) IPOS(2), LROW
      IF (MOD(IPOS(2)-1,100).EQ.0) CALL MSGWRT(3)
C
 999  RETURN
C-----------------------------------------------------------------------
1000  FORMAT ('ROWMED: Filter Width is',I8,', Median is #',I8,
     * 'th Pixel')
1050  FORMAT ('ROWMED: Using only every ',I8,'th Pixel ')
1070  FORMAT ('ROWMED: Correcting Saturation exceeding',F12.5,
     *  ': with value ',F12.5)
1100  FORMAT ('ROWMED: On row ',I8,', Row Length is',I8,' Pixels')
      END
