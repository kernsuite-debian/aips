LOCAL INCLUDE 'NNLSQ.INC'
C                                       Local include for NNLSQ
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD(256), JBUFSZ, ICODE, SCRTCH(512)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), OLDH(256)
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7),
     *   CPARM(10), BUFF1(MABFSS), BUFF2(MABFSS), OLDR(256)
      DOUBLE PRECISION OLDD(128)
      EQUIVALENCE (CATOLD, OLDR, OLDH, OLDD)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOU,
     *   XSEQO, XDISKO, BLC, TRC, CPARM
      COMMON /PARMS/ CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, JBUFSZ, ICODE
      COMMON /CHRPRM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /BUFRS/ BUFF1, BUFF2, SCRTCH
      INCLUDE 'INCS:DCAT.INC'
LOCAL END
      PROGRAM NNLSQ
C-----------------------------------------------------------------------
C! Fit spectral components in an image
C# Map Math Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2015
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
C   This program solves for the set of positive (non-negative)
C   components which when convolved with a specified Gaussian make
C   a least squares fit to the dirty spectrum. For best results
C   (minimum broadening of the profiles combined with best S/N) the
C   sigma of the Gaussian should be similar to that of the
C   instrumental profile (1-1.5 is probably good).
C   This program used TAFFY as a template.
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
C      CPARM(10)      CPARM         User specified array.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGM*6
      INCLUDE 'NNLSQ.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'NNLSQ '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL NLSQIN (PRGM, IRET)
C                                       Fit image.
      IF (IRET.EQ.0) CALL NLSQMA (IRET)
C                                       History
      IF (IRET.EQ.0) CALL NLSQHI
C                                       Close down files, etc.
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE NLSQIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   NLSQIN gets input parameters for NNLSQ and creates an output file.
C   Inputs:  PRGN    C*6       Program name
C   Output:  IRET    I         Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in TAFFY for more details.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER STAT*4, PRGN*6, BLANK*6, MTYPE*2
      INTEGER   IERR, NPARM, IROUND
      INCLUDE 'NNLSQ.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
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
      NPARM = 38
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
C                                       Get modification to CATBLK
      IRET = 4
      CALL NLSQHD (IRET)
      IF (IRET.NE.0) GO TO 999
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
      SEQOUT = CATBLK(KIIMS)
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NLSQIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('NLSQIN: ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE NLSQMA (IRET)
C-----------------------------------------------------------------------
C   NLSQMA sends image one row at a time to the fitting
C   routine and then writes the modified data.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER IFILE*48
      INTEGER   IRET, IROUND, LUNI, LUNO,
     *   NYI, NXI, WINI(4), NXO, NYO, WINO(4), BOI,
     *   BOO, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2,
     *   I3, I4, I5, I6, I7, IPOS(7), CORN(7), BOTEMP, KOFF,
     *   LIMO, LIMIT, IBIND, OBIND, INDI, INDO, LIM1
      LOGICAL   T, F, BLNKD
      INCLUDE 'NNLSQ.INC'
      REAL      OUTMAX, OUTMIN, RDATA(MAXIMG)
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
         DO 250 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.9
            IPOS(1) = IROUND (BLC(1))
C                                       Read.
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'READ', IRET
               GO TO 990
               END IF
            DO 165 I1 = 1,LIM1
               RDATA(I1) = BUFF1(IBIND+I1-1)
 165           CONTINUE
C                                       Write.
            CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
            OBIND = OBIND - 1
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'WRIT', IRET
               GO TO 990
               END IF
C                                       Call NLSQDO
            OBIND = OBIND + 1
            CALL NLSQDO (IPOS, RDATA, BUFF2(OBIND), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1180) IRET
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
 250        CONTINUE
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
      IPOS(1) = -1
      CALL NLSQDO (IPOS, RDATA, BUFF2, IRET)
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
 1000 FORMAT ('NLSQMA: ERROR',I3,' OPENING INPUT FILE')
 1020 FORMAT ('NLSQMA: ERROR',I5,' OPENING OUTPUT FILE')
 1099 FORMAT ('NLSQMA: COMOFF ERROR',I3)
 1100 FORMAT ('NLSQMA: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('NLSQMA: ',A4,' ERROR',I3)
 1180 FORMAT ('NLSQMA: NLSQDO ERROR',I3)
 1260 FORMAT ('NLSQMA: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE NLSQHI
C-----------------------------------------------------------------------
C   NLSQHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   LUN1, LUN2, IERR
      LOGICAL   T
      INCLUDE 'NNLSQ.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   SCRTCH(257), SCRTCH, IERR)
      IF (IERR.GE.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
C                                       New history
      ELSE
         CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2,
     *      SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 20
         CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *      SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 20
C                                       BLC
         WRITE (HILINE,2000) TSKNAM, BLC
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 20
C                                       TRC
         WRITE (HILINE,2001) TSKNAM, TRC
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         END IF
C                                       Close HI file
 20   CALL HICLOS (LUN2, T, SCRTCH, IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NLSQHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,' BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,' TRC =',6(F6.0,','),F6.0)
      END
      SUBROUTINE NLSQHD (IRET)
C-----------------------------------------------------------------------
C   NLSQHD modifies the catalog header block to
C   define the size of the output file; ie.
C      CATBLK(KIDIM)   = the number of axes,
C      CATBLK(KINAX+i) = the dimension of each axis, and
C      CATBLK(KIBPX)   => 1 = integer*2, 2 = real*4 pixel values.
C   These values are the same as the OLD map.
C   OUTNAME, OUTCLASS, OUTSEQ, OUTDISK, TRC and BLC defaults are
C   set elsewhere.
C
C    Input:
C     CATBLK(256)    I     Output catalog header, also CATR, CATD
C     CATOLD(256)    I     Input catalog header, also OLDR, OLDD
C    Output:
C     CATBLK(256)    I     Modified output catalog header.
C     IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   LIMIT, I, FIRSTI, FIRSTO, IRET
      INTEGER   INDXI, INDEX
      INCLUDE 'NNLSQ.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Set axes in output CATBLK.
      FIRSTI = -1
      FIRSTO = -1
      LIMIT = CATOLD(KIDIM)
C                                       Copy/update axis values
      DO 80 I = 1,LIMIT
         CATBLK(KINAX+FIRSTO+I) = TRC(I+FIRSTI+1) -
     *      BLC(I+FIRSTI+1) + 1.01
         CATR(KRCRP+FIRSTO+I) = OLDR(KRCRP+FIRSTI+I) - BLC(I+FIRSTI+1)
     *      + 1.0
         CATR(KRCIC+FIRSTO+I) = CATR(KRCIC+FIRSTI+I)
         CATD(KDCRV+FIRSTO+I) = OLDD(KDCRV+FIRSTI+I)
         INDXI = KHCTP + (I+FIRSTI) * 2
         INDEX = KHCTP + (I-1) * 2
         CALL CHCOPY (8, 1, OLDH(INDXI), 1, CATH(INDEX))
 80      CONTINUE
C                                       Finished.
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE NLSQDO (IPOS, RDATA, RESULT, IRET)
C-----------------------------------------------------------------------
C   This is a skeleton version of subroutine NLSQDO which allows
C   operations on an image one row at a time (1st dimension).
C   Input data is Real*4 with blanking if necessary; output values
C   are R   which may also be blanked.  The calling routine keeps
C   of max., min. and to occurence of blanking.  The calling routine
C   expects CATBLK(KINAX) values per call are expected returned.
C
C   NOTE: blanked values are denoted by the value of the common variable
C   FBLANK
C
C   If IRET .GT. 0 then the output file will be destroyed.
C
C       After all data has been processed a final call will be made to
C   NLSQDO with IPOS(1)=-1.  This is to allow for the completion of
C   pending operations, i.e. preparation of HIstory cards.
C
C   LUN's 16-18 are open and not available to NLSQDO.
C
C       The current contents of CATBLK will be written back to the
C   catalog after the last call to NLSQDO.
C
C   Inputs:
C      IPOS(7)   I    BLC (input image) of first value in RDATA
C      RDATA(*)   R    Input row, magic value blanked.
C   Values from commons:
C      FBLANK    R    Value of blanked pixel.
C      CPARM(10) R    Input adverb array.
C      DPARM(10) R    Input adverb array.
C      CATBLK    I    Output catalog header (also CATR, CATD)
C      CATOLD    I    Input catalog header (also OLDR, OLDD)
C   Output:
C      RESULT(*) R    Output row.
C      IRET      I    Return code   0 => OK
C                               >0 => error, terminate.
C   Output in COMMON
C      CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IRET
      REAL      RDATA(*), RESULT(*)
C
      INTEGER   MDA
      PARAMETER (MDA=21)
C
      INTEGER   LROW, J, I, MODE, NDEX(MDA), NSOLVE
      REAL      A0(MDA,MDA), A(MDA,MDA), B(MDA), X(MDA), RNORM, W(MDA),
     *   Z(MDA), SIG, SUM
      LOGICAL   FIRST
      SAVE FIRST, A0, NSOLVE
      INCLUDE 'NNLSQ.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C                                       Data for NNLSQ:
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if last call
      IF (IPOS(1).LT.0) GO TO 900
      LROW = CATOLD(KINAX)
C                                       Initialize A0(,) on first call:
      IF (FIRST) THEN
         IF (LROW.GT.MDA) THEN
            WRITE (MSGTXT,1000) LROW, MDA
            CALL MSGWRT (7)
            IRET = 1
            GO TO 999
            END IF
C
         SIG = CPARM(1)
         IF (SIG.LE.0.0) SIG = 1.0
         DO 40 J = 1, LROW
            SUM = 0.0
            DO 20 I = 1, LROW
               A0(I,J) = EXP ((REAL(I) - REAL(J))**2
     *                              / (-2.0 * SIG * SIG))
               SUM = SUM + A0(I,J)
 20            CONTINUE
            DO 30 I = 1, LROW
               A0(I,J) = A0(I,J) / SUM
 30            CONTINUE
 40      CONTINUE
         FIRST = .FALSE.
         NSOLVE = 0
         END IF
C                                       Set up the equations:
      DO 60 J = 1, LROW
         DO 50 I = 1, LROW
            A(I,J) = A0(I,J)
 50         CONTINUE
         B(J) = RDATA(J)
 60      CONTINUE
C                                       Solve the equations:
      CALL NNLS (A, MDA, LROW, LROW, B, X, RNORM, W, Z, NDEX, MODE)
      NSOLVE = NSOLVE + 1
      IF (MOD(NSOLVE,100).EQ.1) THEN
         WRITE (MSGTXT,1060) NSOLVE, RNORM
         CALL MSGWRT (2)
         END IF
C                                       Copy answers to RDATA:
      DO 70 J = 1, LROW
         RESULT(J) = X(J)
 70      CONTINUE
      GO TO 999
C                                       Last call - do history etc.
 900  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR. OUTPUT ROW IS ',I7,' MAXIMUM IS ',I5)
 1060 FORMAT ('NSOLVE =',I8,'    RNORM = ',1PE12.5)
      END
      SUBROUTINE NNLS (A,MDA,M,N,B,X,RNORM,W,ZZ,INDEX,MODE)
C-----------------------------------------------------------------------
C     C.L.Lawson and R.J.Hanson, Jet Propulsion Laboratory, 1973 June 15
C     to appear in 'Solving Least Squares Problems', Prentice-Hall, 1974
C
C         **********   NONNEGATIVE LEAST SQUARES   **********
C
C     Given an M by N matrix, A, and an M-vector, B,  compute an
C     N-vector, X, which solves the least squares problem
C
C                      A * X = B  subject to X .GE. 0
C
C  INPUT:
C     MDA  I       The first dimensioning parameter for the
C                  array, A().
C     M    I       First dimension of matrix A(M,N).
C     N    I       Second dimension of matrix A(M,N).
C  IN/OUT:
C     A    R(MDA,?)    On entry A() contains the M by N
C                  matrix, A.  On exit A() contains
C                  the product matrix, Q*A , where Q is an
C                  M by M orthogonal matrix generated implicitly by
C                  this subroutine.
C     B     R(M)   On entry B() contains the M-vector, B.   On exit B()
C                  contains Q*B.
C  OUT:
C     X     R(N)   On entry X() need not be initialized.  On exit X()
C                  will contain the solution vector.
C     RNORM R      On exit RNORM contains the euclidean norm of the
C                  residual vector.
C     W     R(N)   An N-array of working space. On exit W() will contain
C                  the dual solution vector.   W will satisfy W(I) = 0.
C                  For all I in set P  and W(I) .LE. 0. for all I in
C                  set Z.
C     ZZ    R(N)   An M-array of working space.
C     INDEX R(N)   An integer working array of length at least N.
C                  On exit the contents of this array define the sets
C                  P and Z as follows..
C
C                    INDEX(1)   thru INDEX(NSETP) = set P.
C                    INDEX(IZ1) thru INDEX(IZ2)   = set Z.
C                    IZ1 = NSETP + 1 = NPP1
C                    IZ2 = N
C     MODE I       This is a success-failure flag with the following
C                  meanings.
C                  1     The solution has been computed successfully.
C                  2     The dimensions of the problem are bad.
C                        Either M .LE. 0 or N .LE. 0.
C                  3    Iteration count exceeded.  More than 3*N
C                       iterations.
C-----------------------------------------------------------------------
      INTEGER MDA, N, M, IN1
      INTEGER   MODE, ITER, ITMAX, I, IZ2, IZ1, NSETP, NPP1, IZ, J, L,
     *   IZMAX, JZ, JJ, NEXT, IP, II, INDEX(N), I2TMP, MDA2
      REAL   A(MDA,N), B(M), X(N), W(N), ZZ(M), RNORM, ZERO, ONE, TWO,
     *   FACTOR, SM, WMAX, ASAVE, DUMMY(1), UP, UNORM, ZTEST, ALPHA, T,
     *   CC, SS, DIFF
      INCLUDE 'INCS:DMSG.INC'
      DATA IN1 /1/
C-----------------------------------------------------------------------
      ZERO = 0.
      ONE = 1.
      TWO = 2.
      FACTOR = 0.01
C
      MODE = 1
      IF ((M.LE.0) .OR. (N.LE.0)) THEN
         MODE = 2
         GO TO 999
         END IF
      ITER = 0
      ITMAX = 3*N
C                                       Initialize the arrays INDEX()
C                                       and X().
      DO 20 I = 1,N
         X(I) = ZERO
         INDEX(I) = I
 20      CONTINUE
C
      IZ2 = N
      IZ1 = 1
      NSETP = 0
      NPP1 = 1
C                                       Main loop begins here  ******
 30   CONTINUE
C                                       Quit if all coefficients are
C                                       already in the solution or if M
C                                       cols of A have been
C                                       triangularized.
C
      IF (IZ1.GT.IZ2.OR.NSETP.GE.M) GO TO 350
C                                       Compute components of the dual
C                                       (negative gradient) vector W().
         DO 50 IZ = IZ1,IZ2
            J = INDEX(IZ)
            SM = ZERO
            DO 40 L = NPP1,M
               SM = SM+A(L,J)*B(L)
 40            CONTINUE
            W(J) = SM
 50      CONTINUE
C                                       Find largest positive W(J).
 60      WMAX = ZERO
         DO 70 IZ = IZ1,IZ2
             J = INDEX(IZ)
             IF (W(J).GT.WMAX) THEN
                WMAX = W(J)
                IZMAX = IZ
                END IF
 70          CONTINUE
C                                       If WMAX.LE.0. go to termination.
C                                       This indicates satisfaction of
C                                       the Kuhn-Tucker conditions.
C
         IF (WMAX.LE.0) GO TO 350
         IZ = IZMAX
         J = INDEX (IZ)
C                                       The sign of W(J) is ok for J to
C                                       be moved to set P.
C                                       begin the transformation and
C                                       check new diagonal element to
C                                       avoid near linear dependence.
C
      ASAVE = A(NPP1,J)
      I2TMP = NPP1 + 1
      CALL H12 (1, NPP1, I2TMP, M, A(1,J), IN1, UP, DUMMY, 1, 1, 0)
      UNORM = ZERO
      IF (NSETP.EQ.0) GO TO 100
          DO 90 L = 1,NSETP
   90     UNORM = UNORM+A(L,J)**2
  100 UNORM = SQRT(UNORM)
      IF (DIFF(UNORM+ABS(A(NPP1,J))*FACTOR,UNORM)) 130,130,110
C
C     COL J IS SUFFICIENTLY INDEPENDENT.  COPY B INTO ZZ, UPDATE ZZ AND
C   > SOLVE FOR ZTEST ( = PROPOSED NEW VALUE FOR X(J) ).
C
  110     DO 120 L = 1,M
  120     ZZ(L) = B(L)
      I2TMP = NPP1 + 1
      CALL H12 (2, NPP1, I2TMP, M,A(1,J), IN1, UP, ZZ, 1, 1, 1)
      ZTEST = ZZ(NPP1)/A(NPP1,J)
C
C                                     SEE IF ZTEST IS POSITIVE
C     REJECT J AS A CANDIDATE TO BE MOVED FROM SET Z TO SET P.
C     RESTORE A(NPP1,J), SET W(J)=0., AND LOOP BACK TO TEST DUAL
C
      IF (ZTEST) 130,130,140
C
C     COEFFS AGAIN.
C
  130 A(NPP1,J) = ASAVE
      W(J) = ZERO
      GO TO 60
C
C     THE INDEX  J=INDEX(IZ)  HAS BEEN SELECTED TO BE MOVED FROM
C     SET Z TO SET P.    UPDATE B,  UPDATE INDICES,  APPLY HOUSEHOLDER
C     TRANSFORMATIONS TO COLS IN NEW SET Z,  ZERO SUBDIAGONAL ELTS IN
C     COL J,  SET W(J)=0.
C
  140     DO 150 L = 1,M
  150     B(L) = ZZ(L)
C
      INDEX(IZ) = INDEX(IZ1)
      INDEX(IZ1) = J
      IZ1 = IZ1+1
      NSETP = NPP1
      NPP1 = NPP1+1
C
      MDA2 = MDA
      DO 160 JZ = IZ1,IZ2
         JJ = INDEX(JZ)
         CALL H12 (2, NSETP, NPP1, M, A(1,J), IN1, UP, A(1,JJ), 1,
     *      MDA2, 1)
 160     CONTINUE
C
      IF (NSETP.EQ.M) GO TO 190
          DO 180 L = NPP1,M
  180     A(L,J) = ZERO
  190 CONTINUE
C
      W(J) = ZERO
C                                SOLVE THE TRIANGULAR SYSTEM.
C                                STORE THE SOLUTION TEMPORARILY IN ZZ().
      NEXT  =  1
      GO TO 400
  200 CONTINUE
C
C                       ******  SECONDARY LOOP BEGINS HERE ******
C
C                          ITERATION COUNTER.
C
  210 ITER = ITER+1
      IF (ITER.LE.ITMAX) GO TO 220
      MODE = 3
      MSGTXT = 'NNLS Quitting due to iteration count'
      CALL MSGWRT (6)
      GO TO 350
  220 CONTINUE
C
C                    SEE IF ALL NEW CONSTRAINED COEFFS ARE FEASIBLE.
C                                  IF NOT COMPUTE ALPHA.
C
      ALPHA = TWO
          DO 240 IP = 1,NSETP
          L = INDEX(IP)
          IF (ZZ(IP)) 230,230,240
C
  230     T = -X(L)/(ZZ(IP)-X(L))
          IF (ALPHA.LE.T) GO TO 240
          ALPHA = T
          JJ = IP
  240     CONTINUE
C
C          IF ALL NEW CONSTRAINED COEFFS ARE FEASIBLE THEN ALPHA WILL
C          STILL = 2.    IF SO EXIT FROM SECONDARY LOOP TO MAIN LOOP.
C
      IF (ALPHA.EQ.TWO) GO TO 330
C
C          OTHERWISE USE ALPHA WHICH WILL BE BETWEEN 0. AND 1. TO
C          INTERPOLATE BETWEEN THE OLD X AND THE NEW ZZ.
C
          DO 250 IP = 1,NSETP
          L = INDEX(IP)
  250     X(L) = X(L)+ALPHA*(ZZ(IP)-X(L))
C
C        MODIFY A AND B AND THE INDEX ARRAYS TO MOVE COEFFICIENT I
C        FROM SET P TO SET Z.
C
      I = INDEX(JJ)
  260 X(I) = ZERO
C
      IF (JJ.EQ.NSETP) GO TO 290
      JJ = JJ+1
          DO 280 J = JJ,NSETP
          II = INDEX(J)
          INDEX(J-1) = II
          CALL G1 (A(J-1,II),A(J,II),CC,SS,A(J-1,II))
          A(J,II) = ZERO
              DO 270 L = 1,N
              IF (L.NE.II) CALL G2 (CC,SS,A(J-1,L),A(J,L))
  270         CONTINUE
  280     CALL G2 (CC,SS,B(J-1),B(J))
  290 NPP1 = NSETP
      NSETP = NSETP-1
      IZ1 = IZ1-1
      INDEX(IZ1) = I
C
C        SEE IF THE REMAINING COEFFS IN SET P ARE FEASIBLE.  THEY SHOULD
C        BE BECAUSE OF THE WAY ALPHA WAS DETERMINED.
C        IF ANY ARE INFEASIBLE IT IS DUE TO ROUND-OFF ERROR.  ANY
C        THAT ARE NONPOSITIVE WILL BE SET TO ZERO
C        AND MOVED FROM SET P TO SET Z.
C
          DO 300 JJ = 1,NSETP
          I = INDEX(JJ)
          IF (X(I)) 260,260,300
  300     CONTINUE
C
C         COPY B( ) INTO ZZ( ).  THEN SOLVE AGAIN AND LOOP BACK.
C
          DO 310 I = 1,M
  310     ZZ(I) = B(I)
      NEXT  =  2
      GO TO 400
  320 CONTINUE
      GO TO 210
C                      ******  END OF SECONDARY LOOP  ******
C
  330     DO 340 IP = 1,NSETP
          I = INDEX(IP)
  340     X(I) = ZZ(IP)
C        ALL NEW COEFFS ARE POSITIVE.  LOOP BACK TO BEGINNING.
      GO TO 30
C
C                        ******  END OF MAIN LOOP  ******
C
C                        COME TO HERE FOR TERMINATION.
C                     COMPUTE THE NORM OF THE FINAL RESIDUAL VECTOR.
C
  350 SM = ZERO
      IF (NPP1.GT.M) GO TO 370
          DO 360 I = NPP1,M
  360     SM = SM+B(I)**2
      GO TO 390
  370     DO 380 J = 1,N
  380     W(J) = ZERO
  390 RNORM = SQRT(SM)
      RETURN
C
C     THE FOLLOWING BLOCK OF CODE IS USED AS AN INTERNAL SUBROUTINE
C     TO SOLVE THE TRIANGULAR SYSTEM, PUTTING THE SOLUTION IN ZZ().
C
  400     DO 430 L = 1,NSETP
          IP = NSETP+1-L
          IF (L.EQ.1) GO TO 420
              DO 410 II = 1,IP
  410         ZZ(II) = ZZ(II)-A(II,JJ)*ZZ(IP+1)
  420     JJ = INDEX(IP)
  430     ZZ(IP) = ZZ(IP)/A(IP,JJ)
      GO TO (200, 320), NEXT
 999  RETURN
      END
      SUBROUTINE H12 (MODE, LPIVOT, L1, M, U, IUE, UP, C, ICE, ICV, NCV)
C-----------------------------------------------------------------------
C     C.L.Lawson and R.J.Hanson, Jet Propulsion Laboratory, 1973 Jun 12
C     To appear in 'Solving Least Squares Problems', Prentice-Hall, 1974
C
C     Construction and/or application of a single
C     Householder transformation..     Q = I + U*(U**T)/B
C
C     Input:
C     In/Out:
C     Output:
C     MODE    = 1 OR 2   TO SELECT ALGORITHM  H1  OR  H2 .
C     LPIVOT IS THE INDEX OF THE PIVOT ELEMENT.
C     L1,M   IF L1 .LE. M   THE TRANSFORMATION WILL BE CONSTRUCTED TO
C            ZERO ELEMENTS INDEXED FROM L1 THROUGH M.   IF L1 GT. M
C            THE SUBROUTINE DOES AN IDENTITY TRANSFORMATION.
C     U(),IUE,UP    ON ENTRY TO H1 U() CONTAINS THE PIVOT VECTOR.
C                   IUE IS THE STORAGE INCREMENT BETWEEN ELEMENTS.
C                                       ON EXIT FROM H1 U() AND UP
C                   CONTAIN QUANTITIES DEFINING THE VECTOR U OF THE
C                   HOUSEHOLDER TRANSFORMATION.   ON ENTRY TO H2 U()
C                   AND UP SHOULD CONTAIN QUANTITIES PREVIOUSLY COMPUTED
C                   BY H1.  THESE WILL NOT BE MODIFIED BY H2.
C     C()    ON ENTRY TO H1 OR H2 C() CONTAINS A MATRIX WHICH WILL BE
C            REGARDED AS A SET OF VECTORS TO WHICH THE HOUSEHOLDER
C            TRANSFORMATION IS TO BE APPLIED.  ON EXIT C() CONTAINS THE
C            SET OF TRANSFORMED VECTORS.
C     ICE    STORAGE INCREMENT BETWEEN ELEMENTS OF VECTORS IN C().
C     ICV    STORAGE INCREMENT BETWEEN VECTORS IN C().
C     NCV    NUMBER OF VECTORS IN C() TO BE TRANSFORMED. IF NCV .LE. 0
C            NO OPERATIONS WILL BE DONE ON C().
C
C-----------------------------------------------------------------------
      INTEGER IUE, M
      INTEGER   ICE, ICV, L1, LPIVOT, MODE, NCV, J, I2, INCR, I3, I4,
     *   I
      REAL   U(IUE,M), C(*), ONE, UP, CL, CLINV, SM1
      DOUBLE PRECISION SM,B
C-----------------------------------------------------------------------
      ONE = 1.
C
      IF ((0.GE.LPIVOT) .OR. (LPIVOT.GE.L1) .OR. (L1.GT.M)) GO TO 999
      CL = ABS(U(1,LPIVOT))
      IF (MODE.EQ.2) GO TO 60
C                            ****** CONSTRUCT THE TRANSFORMATION. ******
          DO 10 J = L1,M
   10     CL = MAX(ABS(U(1,J)),CL)
      IF (CL) 999, 999, 20
   20 CLINV = ONE/CL
      SM = (DBLE(U(1,LPIVOT))*CLINV)**2
          DO 30 J = L1,M
   30     SM = SM+(DBLE(U(1,J))*CLINV)**2
C                              CONVERT DBLE. PREC. SM TO SNGL. PREC. SM1
      SM1 = SM
      CL = CL*SQRT(SM1)
      IF (U(1,LPIVOT)) 50,50,40
   40 CL = -CL
   50 UP = U(1,LPIVOT)-CL
      U(1,LPIVOT) = CL
      GO TO 70
C            ****** APPLY THE TRANSFORMATION  I+U*(U**T)/B  TO C. ******
C
   60 IF (CL) 999, 999, 70
   70 IF (NCV.LE.0) GO TO 999
      B = DBLE(UP)*U(1,LPIVOT)
C                       B  MUST BE NONPOSITIVE HERE.  IF B = 0., RETURN.
C
      IF (B) 80, 999, 999
   80 B = ONE/B
      I2 = 1-ICV+ICE*(LPIVOT-1)
      INCR = ICE*(L1-LPIVOT)
          DO 120 J = 1,NCV
          I2 = I2+ICV
          I3 = I2+INCR
          I4 = I3
          SM = C(I2)*DBLE(UP)
              DO 90 I = L1,M
              SM = SM+C(I3)*DBLE(U(1,I))
   90         I3 = I3+ICE
          IF (SM) 100,120,100
  100     SM = SM*B
          C(I2) = C(I2)+SM*DBLE(UP)
              DO 110 I = L1,M
              C(I4) = C(I4)+SM*DBLE(U(1,I))
  110         I4 = I4+ICE
  120     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE G1 (A, B, COS, SIN, SIG)
C-----------------------------------------------------------------------
C   C.L.Lawson and R.J.Hanson, jet propulsion laboratory, 1973 Jun 12
C   To appear in 'Solving Least Squares Problems', Prentice-Hall, 1974
C   Compute orthogonal rotation matrix..
C   Compute.. MATRIX  (C, S) so that (C, S)(A) = (SQRT(A**2+B**2))
C                     (-S,C)         (-S,C)(B)   (   0          )
C   Compute SIG = SQRT(A**2+B**2)
C      SIG is computed last to allow for the possibility that
C      SIG may be in the same location as A or B .
C-----------------------------------------------------------------------
      REAL   A, B, COS, SIN, SIG, ZERO, ONE, XR, YR
C-----------------------------------------------------------------------
      ZERO = 0.
      ONE = 1.
      IF (ABS(A).LE.ABS(B)) GO TO 10
         XR = B/A
         YR = SQRT(ONE+XR**2)
         COS = SIGN(ONE/YR,A)
         SIN = COS*XR
         SIG = ABS(A)*YR
         GO TO 999
 10   IF (B) 20, 30, 20
 20   CONTINUE
         XR = A/B
         YR = SQRT(ONE+XR**2)
         SIN = SIGN(ONE/YR,B)
         COS = SIN*XR
         SIG = ABS(B)*YR
         GO TO 999
 30   CONTINUE
         SIG = ZERO
         COS = ZERO
         SIN = ONE
C
 999  RETURN
      END
      SUBROUTINE G2 (COS, SIN, X, Y)
C-----------------------------------------------------------------------
C   C.L.Lawson and R.J.Hanson, Jet Propulsion Laboratory, 1972 Dec 15
C   to appear in 'Solving Least Squares Problems', Prentice-Hall, 1974
C   apply the rotation computed by g1 to (x,y).
C-----------------------------------------------------------------------
      REAL   COS, SIN, X, Y, XR
C-----------------------------------------------------------------------
      XR = COS*X + SIN*Y
      Y = -SIN*X + COS*Y
      X = XR
C
 999  RETURN
      END
      FUNCTION DIFF (X, Y)
C-----------------------------------------------------------------------
C   C.L.Lawson and R.J.Hanson, Jet Propulsion Laboratory, 1973 June 7
C   to appear in 'Solving Least Squares Problems', Prentice-Hall, 1974
C-----------------------------------------------------------------------
      REAL   X, Y, DIFF
C-----------------------------------------------------------------------
      DIFF = X - Y
C
 999  RETURN
      END
