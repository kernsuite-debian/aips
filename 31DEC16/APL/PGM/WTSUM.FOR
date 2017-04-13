LOCAL INCLUDE 'WTSUM.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQI(4), SEQO(2), DISKI(4), DISKO(2), CNOO(2), CNOI(4),
     *   CATOLD(256,4), CATNEW(256,2), NUMHIS, JBUFSZ, MXLOOP
      HOLLERITH XNAME1(3), XCLAS1(2), XNAME2(3), XCLAS2(2), XNAME3(3),
     *   XCLAS3(2), XNAME4(3), XCLAS4(2), XNAMOU(3), XCLAOT(2)
      CHARACTER NAME(4)*12, CLASS(4)*6, NAMEO(2)*12, CLASSO(2)*6,
     *   HISCRD(2)*64
      REAL      XSEQ1, XDISK1, XSEQ2, XDISK2, XSEQ3, XDISK3, XSEQ4,
     *   XDISK4, XSEQO, XDISKO, BLC(7), TRC(7), DOINVR,
     *   BUFFI(MABFSS,4), BUFFO(MABFSS,2), WBLC(7), WTRC(7)
      LOGICAL   WPLANE, DOMANY
      COMMON /INPARM/ XNAME1, XCLAS1, XSEQ1, XDISK1, XNAME2, XCLAS2,
     *   XSEQ2, XDISK2, XNAME3, XCLAS3, XSEQ3, XDISK3, XNAME4, XCLAS4,
     *   XSEQ4, XDISK4, XNAMOU, XCLAOT, XSEQO, XDISKO, BLC, TRC, DOINVR
      COMMON /CHPARM/ NAME, CLASS, NAMEO, CLASSO, HISCRD
      COMMON /PARMS/ CATOLD, CATNEW, SEQI, SEQO, DISKI, DISKO, CNOI,
     *   CNOO, JBUFSZ, NUMHIS, WBLC, WTRC, WPLANE, DOMANY, MXLOOP
      COMMON /BUFRS/ BUFFI, BUFFO
LOCAL END
      PROGRAM WTSUM
C-----------------------------------------------------------------------
C! weighted sum of two or more data cubes with weight sum also output
C# Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2008, 2010, 2015
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
C   does a weighted sum of two images using two weight images.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAME(1)      Name of input image.
C      INCLASS        CLASS(1)     Class of input image.
C      INSEQ          SEQI(1)      Seq. of input image.
C      INDISK         DISKI(1)     Disk number of input image.
C      IN2NAME        NAME(2)      Name of 2nd input image.
C                                  ' ' => loop inseq:in2seq
C                                  and in3seq:(in2seq-inseq+in3seq)
C      IN2CLASS       CLASS(2)     Class of 2nd input image.
C      IN2SEQ         SEQI(2)      Seq. of 2nd input image.
C      IN2DISK        DISKI(2)     Disk number of 2nd input image.
C      IN3NAME        NAME(3)      Name of 1st weight image.
C      IN3CLASS       CLASS(3)     Class of 1st weight image.
C      IN3SEQ         SEQI(3)      Seq. of 1st weight image.
C      IN3DISK        DISKI(3)     Disk number of 1st weight image.
C      IN4NAME        NAME(4)      Name of 2nd weight image.
C      IN4CLASS       CLASS(4)     Class of 2nd weight image.
C      IN4SEQ         SEQI(4)      Seq. of 2nd weight image.
C      IN4DISK        DISKI(4)     Disk number of 2nd weight image.
C      OUTNAME        NAMEO        Name of the output image
C                                  Default output is input image.
C      OUTCLASS       CLASSO       Class of the output image.
C                                  Default is WTSUM
C                                  weight sum image is WWTSUM
C      OUTSEQ         SEQO         Seq. number of output image.
C      OUTDISK        DISKO        Disk number of the output image.
C      BLC(7)         BLC          Bottom left corner of subimage
C                                  of input image.
C      TRC(7)         TRC          Top right corner of subimage.
C      DOINVERS       DOINVR       > 0 -> weight images are rms (use
C                                         1/rms**2) for weight, output
C                                         rms
C                                  else use as simple weights
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'WTSUM.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'WTSUM '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL WTSUIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL WTSUDO (IRET)
C                                       History
      IF (IRET.EQ.0) CALL WTSUHI
C                                       Close down files, etc.
      CALL DIE (IRET, BUFFI)
C
 999  STOP
      END
      SUBROUTINE WTSUIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   WTSUIN gets input parameters for WTSUM and creates an output file.
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
      CHARACTER  STAT*4, MTYPE*2
      INTEGER   IERR, NPARM, IROUND, II, I, NOMATC
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'WTSUM.INC'
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
      NPARM = 50
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAME1, BUFFI, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFFI, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAME1, NAME(1))
      CALL H2CHR (12, 1, XNAME2, NAME(2))
      CALL H2CHR (12, 1, XNAME3, NAME(3))
      CALL H2CHR (12, 1, XNAME4, NAME(4))
      CALL H2CHR (6, 1, XCLAS1, CLASS(1))
      CALL H2CHR (6, 1, XCLAS2, CLASS(2))
      CALL H2CHR (6, 1, XCLAS3, CLASS(3))
      CALL H2CHR (6, 1, XCLAS4, CLASS(4))
      CALL H2CHR (12, 1, XNAMOU, NAMEO(1))
      CALL H2CHR (6, 1, XCLAOT, CLASSO(1))
C                                       Crunch input parameters.
      SEQI(1) = IROUND (XSEQ1)
      SEQI(2) = IROUND (XSEQ2)
      SEQI(3) = IROUND (XSEQ3)
      SEQI(4) = IROUND (XSEQ4)
      SEQO(1) = IROUND (XSEQO)
      SEQO(2) = IROUND (XSEQO)
      DISKI(1) = IROUND (XDISK1)
      DISKI(2) = IROUND (XDISK2)
      DISKI(3) = IROUND (XDISK3)
      DISKI(4) = IROUND (XDISK4)
      DISKO(1) = IROUND (XDISKO)
C                                       Multi image option
      DOMANY = NAME(2).EQ.' '
      IF (DOMANY) THEN
         MXLOOP = SEQI(2) - SEQI(1) + 1
         NAME(2) = NAME(1)
         CLASS(2) = CLASS(1)
         DISKI(2) = DISKI(1)
         SEQI(2) = SEQI(1) + 1
         NAME(4) = NAME(3)
         CLASS(4) = CLASS(3)
         DISKI(4) = DISKI(3)
         SEQI(4) = SEQI(3) + 1
      ELSE
         MXLOOP = 2
         END IF
C                                       Create new file.
C                                       Get CATBLK from old files
      DO 10 II = 1,4
         CNOI(II) = 1
         MTYPE = 'MA'
         CALL CATDIR ('SRCH', DISKI(II), CNOI(II), NAME(II), CLASS(II),
     *      SEQI(II), MTYPE, NLUSER, STAT, BUFFI, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR, NAME(II), CLASS(II), SEQI(II),
     *         DISKI(II), NLUSER
            GO TO 990
            END IF
C                                       Read CATBLK and mark 'READ'.
         CALL CATIO ('READ', DISKI(II), CNOI(II), CATOLD(1,II), 'READ',
     *      BUFFI, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKI(II)
         FCNO(NCFILE) = CNOI(II)
         FRW(NCFILE) = 0
 10      CONTINUE
C                                       Checks
      NOMATC = 1000
      DO 15 I = 1,KICTPN
         IF ((CATOLD(KIDIM,1).GE.I) .AND. (CATOLD(KINAX+I-1,1).GT.1)
     *      .AND. (CATOLD(KINAX+I-1,1).NE.CATOLD(KINAX+I-1,2)))
     *      NOMATC = MIN (NOMATC, I)
         IF ((CATOLD(KIDIM,2).GE.I) .AND. (CATOLD(KINAX+I-1,2).GT.1)
     *      .AND. (CATOLD(KINAX+I-1,1).NE.CATOLD(KINAX+I-1,2)))
     *      NOMATC = MIN (NOMATC, I)
         IF ((CATOLD(KIDIM,1).GE.3) .AND. (CATOLD(KINAX+I-1,3).GT.1)
     *      .AND. (CATOLD(KINAX+I-1,3).NE.CATOLD(KINAX+I-1,4)))
     *      NOMATC = MIN (NOMATC, I)
         IF ((CATOLD(KIDIM,1).GE.4) .AND. (CATOLD(KINAX+I-1,4).GT.1)
     *      .AND. (CATOLD(KINAX+I-1,1).NE.CATOLD(KINAX+I-1,2)))
     *      NOMATC = MIN (NOMATC, I)
 15      CONTINUE
      IF (CATOLD(KINAX,1).NE.CATOLD(KINAX,3)) NOMATC = MIN (NOMATC, 1)
      IF (CATOLD(KINAX+1,1).NE.CATOLD(KINAX+1,3))
     *    NOMATC = MIN (NOMATC, 2)
      IF (NOMATC.LT.999) THEN
         WRITE (MSGTXT,1015) NOMATC
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD(1,1), CATBLK)
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAME, CLASS, SEQI, '      ', NAMEO, CLASSO, SEQO)
      CALL CHR2H (12, NAMEO(1), KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASSO(1), KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQO(1)
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATOLD(KIDIM,1), CATOLD(KINAX,1), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL RCOPY (7, BLC, WBLC)
      CALL RCOPY (7, TRC, WTRC)
C                                       get window mods to header
      IRET = 4
      CALL WTSHED (BLC, TRC, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output file.
      CNOO(1) = 1
      IRET = 4
      CALL MCREAT (DISKO(1), CNOO(1), BUFFI, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO(1)
      FCNO(NCFILE) = CNOO(1)
      FRW(NCFILE) = 2
      SEQO(1) = CATBLK(KIIMS)
      CALL COPY (256, CATBLK, CATNEW(1,1))
C                                       copy most keywords
      CALL KEYPCP (DISKI(1), CNOI(1), DISKO(1), CNOO(1), 0, ' ', IERR)
C                                       make second output
      NAMEO(2) = NAMEO(1)
      CLASSO(2) = 'WWTSUM'
      DISKO(2) = DISKO(1)
      CALL COPY (256, CATOLD(1,3), CATBLK)
C                                       Put new values in CATBLK.
      CALL CHR2H (12, NAMEO(2), KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASSO(2), KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQO(2)
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATOLD(KIDIM,3), CATOLD(KINAX,3), WBLC, WTRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       window mods to header
      IRET = 4
      CALL WTSHED (WBLC, WTRC, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output file.
      CNOO(2) = 1
      IRET = 4
      CALL MCREAT (DISKO(2), CNOO(2), BUFFI, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO(2)
      FCNO(NCFILE) = CNOO(2)
      FRW(NCFILE) = 2
      SEQO(2) = CATBLK(KIIMS)
C                                       copy most keywords
      CALL KEYPCP (DISKI(1), CNOI(1), DISKO(2), CNOO(2), 0, ' ', IERR)
      CALL COPY (256, CATBLK, CATNEW(1,2))
      WPLANE = (CATBLK(KINAX+2).LE.1) .AND. (CATBLK(KINAX+3).LE.1)
     *   .AND. (CATBLK(KINAX+4).LE.1) .AND. (CATBLK(KINAX+5).LE.1)
     *   .AND. (CATBLK(KINAX+6).LE.1)
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WTSUIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1015 FORMAT ('WTSUIN: FIRST AXIS NOT MATCHING =',I3)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('WTSUIN: ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE WTSUDO (IRET)
C-----------------------------------------------------------------------
C   WTSUDO sends image one row at a time to the summing routine and then
C   writes the modified data.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER IFILE*48, MTYPE*2, STAT*4
      INTEGER   IROUND, LUNI(4), LUNO(2), NYI, NXI, WINI(4), NXO, NYO,
     *   WINO(4), BOI, BOO, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2,
     *   I3, I4, I5, I6, I7, IPOS(7), CORN(7), WPOS(7), WCORN(7), II,
     *   BOTEMP, IBIND(4), OBIND(2), INDI(4), INDO(4), LIM1, NLOOPS,
     *   CNO1, DISKI1, CNO3, DISKI3
      REAL      OUTMAX(2), OUTMIN(2), DUM(1), CATRN(256,2)
      LOGICAL   T, F, BLNKD(2), DO2WRI
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'WTSUM.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (CATRN, CATNEW)
      DATA LUNI, LUNO /16,17,18,19, 20,21/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      NLOOPS = 2
      CNO1 = CNOI(1)
      DISKI1 = DISKI(1)
      CNO3 = CNOI(3)
      DISKI3 = DISKI(3)
C                                       Open and init for output
      DO 10 II = 1,2
         CALL ZPHFIL ('MA', DISKO(II), CNOO(II), 1, IFILE, IRET)
         CALL ZOPEN (LUNO(II), INDO(II), DISKO(II), IFILE, T, T, T,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
 10      CONTINUE
C                                       Setup for I/O
      NXI = CATOLD(KINAX,1)
      NYI = CATOLD(KINAX+1,1)
      NXO = CATNEW(KINAX,1)
      NYO = CATNEW(KINAX+1,1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
      OUTMAX(1) = -1.0E20
      OUTMIN(1) = 1.0E20
      OUTMAX(2) = -1.0E20
      OUTMIN(2) = 1.0E20
      BLNKD(1) = F
      BLNKD(2) = F
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM4 = TRC(4) - BLC(4) + 1.01
      LIM5 = TRC(5) - BLC(5) + 1.01
      LIM6 = TRC(6) - BLC(6) + 1.01
      LIM7 = TRC(7) - BLC(7) + 1.01
C                                       LOOP POINT:
C                                       Open and init for read
 20   DO 30 II = 1,4
         CALL ZPHFIL ('MA', DISKI(II), CNOI(II), 1, IFILE, IRET)
         CALL ZOPEN (LUNI(II), INDI(II), DISKI(II), IFILE, T, F, T,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET
            GO TO 990
            END IF
 30      CONTINUE
      DO2WRI = .TRUE.
      CALL FILL (7, 1, WPOS)
      CALL FILL (7, 1, WCORN)
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
                     IF (.NOT.WPLANE) CALL COPY (5, IPOS(3), WPOS(3))
                     IF (.NOT.WPLANE) CALL COPY (5, CORN(3), WCORN(3))
C                                       Init. files, 1st & 2nd input.
         CALL COMOFF (CATOLD(KIDIM,1), CATOLD(KINAX,1), IPOS(3), BOTEMP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1099) IRET
            GO TO 990
            END IF
         BOI = BOTEMP + 1
         CALL MINIT ('READ', LUNI(1), INDI(1), NXI, NYI, WINI,
     *      BUFFI(1,1), JBUFSZ, BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'INIT READ 1', IRET
            GO TO 990
            END IF
         CALL MINIT ('READ', LUNI(2), INDI(2), NXI, NYI, WINI,
     *      BUFFI(1,2), JBUFSZ, BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'INIT READ 2', IRET
            GO TO 990
            END IF
C                                       Init. files, 3 & 4 input.
         CALL COMOFF (CATOLD(KIDIM,3), CATOLD(KINAX,3), WPOS(3), BOTEMP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1099) IRET
            GO TO 990
            END IF
         BOI = BOTEMP + 1
         CALL MINIT ('READ', LUNI(3), INDI(3), NXI, NYI, WINI,
     *      BUFFI(1,3), JBUFSZ, BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'INIT READ 3', IRET
            GO TO 990
            END IF
         CALL MINIT ('READ', LUNI(4), INDI(4), NXI, NYI, WINI,
     *      BUFFI(1,4), JBUFSZ, BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'INIT READ 4', IRET
            GO TO 990
            END IF
C                                       Init output file.
         CALL COMOFF (CATNEW(KIDIM,1), CATNEW(KINAX,1), CORN(3), BOTEMP,
     *      IRET)
         BOO = BOTEMP + 1
         CALL MINIT ('WRIT', LUNO(1), INDO(1), NXO, NYO, WINO,
     *      BUFFO(1,1), JBUFSZ, BOO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'INIT WRIT 1', IRET
            GO TO 990
            END IF
         IF (DO2WRI) THEN
            CALL COMOFF (CATNEW(KIDIM,2), CATNEW(KINAX,2), WCORN(3),
     *         BOTEMP, IRET)
            BOO = BOTEMP + 1
            CALL MINIT ('WRIT', LUNO(2), INDO(2), NXO, NYO, WINO,
     *         BUFFO(1,2), JBUFSZ, BOO, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) 'INIT WRIT 2', IRET
               GO TO 990
               END IF
            END IF
         DO 220 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.9
            IPOS(1) = IROUND (BLC(1))
C                                       Read.
            DO 110 II = 1,4
               CALL MDISK ('READ', LUNI(II), INDI(II), BUFFI(1,II),
     *            IBIND(II), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1120) 'READ', IRET
                  GO TO 990
                  END IF
 110           CONTINUE
C                                       Write.
            CALL MDISK ('WRIT', LUNO(1), INDO(1), BUFFO(1,1), OBIND(1),
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'WRIT 1', IRET
               GO TO 990
               END IF
            IF (DO2WRI) THEN
               CALL MDISK ('WRIT', LUNO(2), INDO(2), BUFFO(1,2),
     *            OBIND(2), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1120) 'WRIT 2', IRET
                  GO TO 990
                  END IF
               END IF
C                                       Call WTSUMI
            CALL WTSUMI (DO2WRI, IPOS, BUFFI(IBIND(1),1),
     *         BUFFI(IBIND(2),2), BUFFI(IBIND(3),3), BUFFI(IBIND(4),4),
     *         BUFFO(OBIND(1),1), BUFFO(OBIND(2),2), IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1180) IRET
               GO TO 990
               END IF
C                                       Check max, min, blanking.
            DO 200 II = 1,NXO
               I1 = OBIND(1) + II - 1
               IF (BUFFO(I1,1).NE.FBLANK) THEN
                  OUTMAX(1) = MAX (OUTMAX(1), BUFFO(I1,1))
                  OUTMIN(1) = MIN (OUTMIN(1), BUFFO(I1,1))
               ELSE
                  BLNKD(1) = T
                  END IF
               IF (DO2WRI) THEN
                  I1 = OBIND(2) + II - 1
                  IF (BUFFO(I1,2).NE.FBLANK) THEN
                     OUTMAX(2) = MAX (OUTMAX(2), BUFFO(I1,2))
                     OUTMIN(2) = MIN (OUTMIN(2), BUFFO(I1,2))
                  ELSE
                     BLNKD(2) = T
                     END IF
                  END IF
 200           CONTINUE
 220        CONTINUE
C                                       Flush buffer.
         CALL MDISK ('FINI', LUNO(1), INDO(1), BUFFO(1,1), OBIND(1),
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1120) 'FINI 1', IRET
            GO TO 990
            END IF
C                                       Flush buffer.
         IF (DO2WRI) THEN
            CALL MDISK ('FINI', LUNO(2), INDO(2), BUFFO(1,2), OBIND(2),
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'FINI 2', IRET
               GO TO 990
               END IF
            END IF
         DO2WRI = .NOT.WPLANE
 300     CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Mark blanking in CATBLK.
      CATRN(KRBLK,1) = 0.0
      IF (BLNKD(1)) CATRN(KRBLK,1) = FBLANK
      CATRN(KRBLK,2) = 0.0
      IF (BLNKD(2)) CATRN(KRBLK,2) = FBLANK
C                                       Update CATBLK.
      CATRN(KRDMX,1) = OUTMAX(1)
      CATRN(KRDMN,1) = OUTMIN(1)
      CALL CATIO ('UPDT', DISKO(1), CNOO(1), CATNEW(1,1), 'REST', BUFFI,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1260) IRET
         GO TO 990
         END IF
      CATRN(KRDMX,2) = OUTMAX(2)
      CATRN(KRDMN,2) = OUTMIN(2)
      CALL CATIO ('UPDT', DISKO(2), CNOO(2), CATNEW(1,2), 'REST', BUFFI,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1260) IRET
         GO TO 990
         END IF
C                                       Close input map.
      CALL ZCLOSE (LUNI(1), INDI(1), IRET)
      CALL ZCLOSE (LUNI(2), INDI(2), IRET)
      CALL ZCLOSE (LUNI(3), INDI(3), IRET)
      CALL ZCLOSE (LUNI(4), INDI(4), IRET)
C                                       Is there more to do?
      NLOOPS = NLOOPS + 1
      IF ((DOMANY) .AND. (NLOOPS.LE.MXLOOP)) THEN
         CNOI(1) = CNOO(1)
         CNOI(3) = CNOO(2)
         DISKI(1) = DISKO(1)
         DISKI(3) = DISKO(2)
         SEQI(2) = SEQI(2) + 1
         SEQI(4) = SEQI(4) + 1
         MTYPE = 'MA'
         DO 710 II = 2,4,2
            CALL CATDIR ('SRCH', DISKI(II), CNOI(II), NAME(II),
     *         CLASS(II), SEQI(II), MTYPE, NLUSER, STAT, BUFFI, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1700) IRET, NAME(II), CLASS(II), SEQI(II),
     *            DISKI(II), NLUSER
               GO TO 990
               END IF
 710        CONTINUE
         GO TO 20
         END IF
C                                       Final call to functions
      IPOS(1) = -2
      CALL WTSUMI (DO2WRI, IPOS, DUM, DUM, DUM, DUM, DUM, DUM, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1180) IRET
         GO TO 990
         END IF
      CALL ZCLOSE (LUNO(1), INDO(1), IRET)
      CALL ZCLOSE (LUNO(2), INDO(2), IRET)
      IRET = 0
      CNOI(1) = CNO1
      DISKI(1) = DISKI1
      CNOI(3) = CNO3
      DISKI(3) = DISKI3
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WTSUDO: ERROR',I3,' OPENING INPUT FILE')
 1020 FORMAT ('WTSUDO: ERROR',I5,' OPENING OUTPUT FILE')
 1099 FORMAT ('WTSUDO: COMOFF ERROR',I3)
 1100 FORMAT ('WTSUDO: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('WTSUDO: ',A,' ERROR',I3)
 1180 FORMAT ('WTSUDO: WTSUMI ERROR',I3)
 1260 FORMAT ('WTSUDO: CATIO ERROR',I3,' UPDATING CATBLK')
 1700 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
      END
      SUBROUTINE WTSUHI
C-----------------------------------------------------------------------
C   WTSUHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER LABEL*8, LINE*80, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR, I
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'WTSUM.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
      CALL HIINIT (3)
C                                       Write History: output 1
C                                       Copy/open history file.
      CALL COPY (256, CATNEW(1,1), CATBLK)
      CALL HISCOP (LUN1, LUN2, DISKI(1), DISKO(1), CNOI(1), CNOO,
     *   CATBLK, BUFFI, BUFFO, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) 1, IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAME(1), CLASS(1), SEQI(1), DISKI(1), LUN2,
     *   BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCO2 (TSKNAM, NAME(2), CLASS(2), SEQI(2), DISKI(2), LUN2,
     *   BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCO3 (TSKNAM, NAME(3), CLASS(3), SEQI(3), DISKI(3), LUN2,
     *   BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCO4 (TSKNAM, NAME(4), CLASS(4), SEQI(4), DISKI(4), LUN2,
     *   BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, NAMEO(1), CLASSO(1), SEQO(1), DISKO(1), LUN2,
     *   BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       BLC
      WRITE (MSGTXT,2000) TSKNAM, BLC
      CALL HIADD (LUN2, MSGTXT, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       TRC
      WRITE (MSGTXT,2001) TSKNAM, TRC
      CALL HIADD (LUN2, MSGTXT, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
C                                      Add any user supplied history.
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1010) TSKNAM
         LINE(1:8) = LABEL(1:8)
         DO 50 I = 1,NUMHIS
            LINE(9:64) = HISCRD(I)(1:64)
            CALL HIADD (LUN2, LINE, BUFFO, IERR)
            IF (IERR.NE.0) GO TO 100
 50         CONTINUE
         END IF
C                                       Close HI file
 100  CALL HICLOS (LUN2, T, BUFFO, IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO(1), CNOO(1), CATBLK, 'REST', BUFFI,
     *   IERR)
C                                       Write History: output 1
C                                       Copy/open history file.
      CALL COPY (256, CATNEW(1,2), CATBLK)
      CALL HISCOP (LUN1, LUN2, DISKI(3), DISKO(2), CNOI(3), CNOO(2),
     *   CATBLK, BUFFI, BUFFO, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) 2, IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO3 (TSKNAM, NAME(3), CLASS(3), SEQI(3), DISKI(3), LUN2,
     *   BUFFO, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCO4 (TSKNAM, NAME(4), CLASS(4), SEQI(4), DISKI(4), LUN2,
     *   BUFFO, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMEO(2), CLASSO(2), SEQO(2), DISKO(2), LUN2,
     *   BUFFO, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       BLC
      WRITE (MSGTXT,2000) TSKNAM, WBLC
      CALL HIADD (LUN2, MSGTXT, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       TRC
      WRITE (MSGTXT,2001) TSKNAM, WTRC
      CALL HIADD (LUN2, MSGTXT, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any user supplied history.
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1010) TSKNAM
         LINE(1:8) = LABEL(1:8)
         DO 150 I = 1,NUMHIS
            LINE(9:64) = HISCRD(I)(1:64)
            CALL HIADD (LUN2, LINE, BUFFO, IERR)
            IF (IERR.NE.0) GO TO 200
 150        CONTINUE
         END IF
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFFO, IERR)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKI, DISKO, CNOI, CNOO,
     *   CATBLK, BUFFI, BUFFO, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO(2), CNOO(2), CATBLK, 'REST', BUFFI,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WTSUHI: COPY/OPEN HISTORY FILE #',I2,' ERROR',I3)
 1010 FORMAT (A6,' /')
 2000 FORMAT (A6,'BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,'TRC =',6(F6.0,','),F6.0)
      END
      SUBROUTINE WTSHED (BBLC, TTRC, IRET)
C-----------------------------------------------------------------------
C    Input:
C       CATBLK    I(256)  Output catalog header, also CATR, CATD
C    Output:
C       CATBLK    I(256)  Modified output catalog header.
C       IRET      I       Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
      REAL      BBLC(7), TTRC(7)
C
      CHARACTER FCHARS(3)*4, CTEMP*4
      INTEGER   LIMIT, I, INDEX
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'WTSUM.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
C                                       Set axes in output CATBLK.
      LIMIT = CATBLK(KIDIM)
C                                       Copy/update axis values
      DO 20 I = 1,LIMIT
         CATBLK(KINAX-1+I) = TTRC(I) - BBLC(I) + 1.01
         CATR(KRCRP-1+I) = CATR(KRCRP-1+I) - BBLC(I) + 1.0
         IF (CATBLK(KIALT).NE.0) THEN
            INDEX = KHCTP + 2 * I - 2
            CALL H2CHR (4, 1, CATH(INDEX), CTEMP)
            IF ((CTEMP.EQ.FCHARS(1)) .OR. (CTEMP.EQ.FCHARS(2)) .OR.
     *          (CTEMP.EQ.FCHARS(3))) CATR(KRARP) = CATR(KRARP) -
     *          BBLC(I) + 1.0
            END IF
 20      CONTINUE
C                                       Finished.
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE WTSUMI (DO2, IPOS, D1, D2, W1, W2, R1, R2, IRET)
C-----------------------------------------------------------------------
C   Does weighted sum
C   Inputs:
C      DO2     L       Do the 2nd output?
C      IPOS    I(7)    BLC (input image) of first value in DATA
C                         IPOS(1) = -1 => no input data this call.
C                         IPOS(2) = -2 => last call (no input data).
C      D1      R(*)    Input row, magic value blanked - image 1
C      D2      R(*)    Input row, magic value blanked - image 2
C      W1      R(*)    Input row, magic value blanked - weight 1
C      W2      R(*)    Input row, magic value blanked - weight 2
C   Values from commons:
C      FBLANK  R      Value of blanked pixel.
C      DOINVR  R      > 0 -> rms's, else 1/rms**2
C      CATNEW  I      Input catalog header - gives NX
C   Output:
C      R1      R(*)   Output weighted sum
C      R2      R(*)   If DO2, sum of weights or new rms
C      IRET    I      Return code   0 => OK
C                                  >0 => error, terminate.
C   Output in COMMON:
C      NBLANK  I      Number pixels so far blanked
C      NUMHIS  I          # history entries (max. 10)
C      HISCRD  C(NUMHIS)  History records
C      CATBLK  I          Catalog header block
C-----------------------------------------------------------------------
      LOGICAL   DO2
      INTEGER   IPOS(7), IRET
      REAL      D1(*), D2(*), W1(*), W2(*), R1(*), R2(*)
C
      INTEGER   LROW, I
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'WTSUM.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Data call
      IF (IPOS(1).GT.0) THEN
         LROW = CATNEW(KINAX,1)
C                                       convert rms's
         IF (DOINVR.GT.0.0) THEN
            DO 10 I = 1,LROW
               IF ((W1(I).LE.0.0) .OR. (W1(I).EQ.FBLANK)) THEN
                  W1(I) = FBLANK
               ELSE
                  W1(I) = 1.0 / (W1(I) * W1(I))
                  END IF
               IF ((W2(I).LE.0.0) .OR. (W2(I).EQ.FBLANK)) THEN
                  W2(I) = FBLANK
               ELSE
                  W2(I) = 1.0 / (W2(I) * W2(I))
                  END IF
 10            CONTINUE
            END IF
C                                       weighted sum
         DO 20 I = 1,LROW
            IF ((D1(I).NE.FBLANK) .AND. (W1(I).NE.FBLANK) .AND.
     *         (W1(I).GT.0.0)) THEN
               IF ((D2(I).NE.FBLANK) .AND. (W2(I).NE.FBLANK) .AND.
     *            (W2(I).GT.0.0)) THEN
                  R1(I) = (W1(I) * D1(I) + W2(I) * D2(I)) /
     *              (W1(I) +  W2(I))
                  IF (DO2) R2(I) = W1(I) + W2(I)
               ELSE
                  R1(I) = D1(I)
                  IF (DO2) R2(I) = W1(I)
                  END IF
            ELSE
               IF ((D2(I).NE.FBLANK) .AND. (W2(I).NE.FBLANK) .AND.
     *            (W2(I).GT.0.0)) THEN
                  R1(I) = D2(I)
                  IF (DO2) R2(I) = W2(I)
               ELSE
                  R1(I) = FBLANK
                  IF (DO2) R2(I) = FBLANK
                  END IF
               END IF
 20         CONTINUE
C                                       back to rms
         IF ((DO2) .AND. (DOINVR.GT.0.0)) THEN
            DO 30 I = 1,LROW
               IF ((R2(I).LE.0) .OR. (R2(I).EQ.FBLANK)) THEN
                  R2(I) = FBLANK
               ELSE
                  R2(I) = 1.0 / SQRT (R2(I))
                  END IF
 30            CONTINUE
            END IF
C                                       History call
      ELSE
         NUMHIS = 2
         IF (DOINVR.GT.0.0) THEN
            HISCRD(1) = ' weight images taken to be in rms form'
         ELSE
            HISCRD(1) = ' weight images taken to be in 1/rms**2 form'
            END IF
         IF (DOMANY) THEN
            WRITE (HISCRD(2),1100) MXLOOP
         ELSE
            WRITE (HISCRD(2),1101) MXLOOP
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (I5,' images summed in loop INSEQ through IN2SEQ')
 1101 FORMAT (I2,' images as specified above were summed')
      END
