LOCAL INCLUDE 'NINER.INC'
C                                       Local include for NINER
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD(256), JBUFSZ, ICODE, SCRTCH(512)
      LOGICAL   DROP1
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, OPCODE*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), XOPCOD,
     *   OLDH(256)
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7),
     *   CPARM(10), DPARM(10), BUFF1(MABFSS), BUFF2(MABFSS), OLDR(256)
      DOUBLE PRECISION OLDD(128)
      EQUIVALENCE (CATOLD, OLDR, OLDH, OLDD)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOU,
     *   XSEQO, XDISKO, BLC, TRC, XOPCOD, CPARM, DPARM
      COMMON /PNINER/ CATOLD, DROP1, SEQIN, SEQOUT, DISKIN, DISKO,
     *   NEWCNO, OLDCNO, JBUFSZ, ICODE
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, OPCODE
      COMMON /BUFRS/ BUFF1, BUFF2, SCRTCH
      INCLUDE 'INCS:DCAT.INC'
LOCAL END
      PROGRAM NINER
C-----------------------------------------------------------------------
C! Filters an image.
C# MAP Math
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
C   NINER allows a user to apply 3x3 filters to images.  It uses the
C   'scrolling buffer' concept with space for a 3-row buffer.  It
C   provides a framework which allows the implementation of any
C   operator which depends only on the nine pixels of the 3x3 matrix
C   surrounding and including a given pixel position.  NINER can handle
C   up to 7 dimensional images in either integer or real format and
C   supports blanking.  A subimage of the input image may be selected.
C   The various types of filters are listed in the OPCODE section of
C   the HELP file.  The necessary control information can be passed to
C   the program via OPCODE, CPARM, and DPARM.
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
C      OPCODE         OPCODE        User specified opcode.
C      CPARM(10)      CPARM         User specified array.
C      DPARM(10)      DPARM         User specified array.
C   Written by: Thad A. Polk & Donald C. Wells
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'NINER.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'NINER '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL NININ (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL NINSND (IRET)
      IF (IRET.EQ.0) CALL NINHIS
C                                       Close down files, etc.
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE NININ (PRGN, IRET)
C-----------------------------------------------------------------------
C   NININ gets input parameters for NINER and creates an output file.
C   Inputs:  PRGN    C*6       Program name
C   Output:  IRET    I         Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in NINER for more details.
C
C-----------------------------------------------------------------------
      CHARACTER  STAT*4, PRGN*6, BLANK*6, MTYPE*2
      INTEGER   IRET, IERR, NPARM, IROUND
      INCLUDE 'NINER.INC'
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
C                                       Get user modification to CATBLK
      IRET = 4
      CALL NININI (IRET)
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
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NININ: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('NININ: ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE NINSND (IRET)
C-----------------------------------------------------------------------
C   NINSND sends image one row at a time to the user supplied
C   routine and then writes the modified data.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER IFILE*48
      INTEGER   IRET, IROUND, LUNI, LUNO, NYI, NXI, WINI(4), NXO, NYO,
     *   WINO(4), BOI, BOO, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2,
     *   I3, I4, I5, I6, I7, IPOS(7), CORN(7), BOTEMP, KOFF, LIMO,
     *   LIMIT, IBIND, OBIND, INDI, INDO, LIM1, DIDRET, OUTCNT
      LOGICAL   T, F, BLNKD
      INCLUDE 'NINER.INC'
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
      IF (DROP1) KOFF = -1
      CORN(7) = 1
      LIMO = CATBLK(KINAX) - 1
      IF (DROP1) LIMO = 0
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
         IF ((.NOT.DROP1) .OR. (I3.LE.1)) THEN
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
         DIDRET = 0
         OUTCNT = LIM2
         DO 220 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.9
            IPOS(1) = IROUND (BLC(1))
C                                       Read.
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'READ', IRET
               GO TO 990
               END IF
C                                       Copy to buffer.
            DO 165 I1 = 1,LIM1
               RDATA(I1) = BUFF1(IBIND+I1-1)
 165           CONTINUE
C                                       Write.
C                                       Check for deferred output.
            IF (((.NOT.DROP1) .OR. (I2.LE.1)) .AND. (DIDRET.GE.0)) THEN
               CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
               OBIND = OBIND - 1
               OUTCNT = OUTCNT - 1
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1120) 'WRIT', IRET
                  GO TO 990
                  END IF
               END IF
C                                       Call DONINE
            IF (DIDRET.GE.0) OBIND = OBIND + 1
            CALL DONINE (IPOS, RDATA, BUFF2(OBIND), IRET)
            DIDRET = IRET
C                                       error
            IF (DIDRET.GT.0) THEN
               WRITE (MSGTXT,1180) IRET
               GO TO 990
C                                       Check max, min, blanking.
C                                       output not deferred
            ELSE IF (DIDRET.EQ.0) THEN
               LIMIT = OBIND + LIMO
               DO 200 I1 = OBIND,LIMIT
                  BLNKD = BLNKD .OR. (BUFF2(I1).EQ.FBLANK)
                  IF (BUFF2(I1).NE.FBLANK) THEN
                     OUTMAX = MAX (OUTMAX, BUFF2(I1))
                     OUTMIN = MIN (OUTMIN, BUFF2(I1))
                     END IF
 200              CONTINUE
               END IF
 220        CONTINUE
C                                       Read out any remaining rows
C                                       from DONINE.
         DO 260 I2 = 1,OUTCNT
            IPOS(1) = -1
C                                       Check if write requested.
            IF (.NOT.DROP1) THEN
               CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
               OBIND = OBIND - 1
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1120) 'WRIT', IRET
                  GO TO 990
                  END IF
               END IF
C                                       Call DONINE
            OBIND = OBIND + 1
            CALL DONINE (IPOS, RDATA, BUFF2(OBIND), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1180) IRET
               GO TO 990
               END IF
C                                       Check max, min, blanking.
            LIMIT = OBIND + LIMO
            DO 250 I1 = OBIND,LIMIT
               BLNKD = BLNKD .OR. (BUFF2(I1).EQ.FBLANK)
               IF (BUFF2(I1).NE.FBLANK) THEN
                  OUTMAX = MAX (OUTMAX, BUFF2(I1))
                  OUTMIN = MIN (OUTMIN, BUFF2(I1))
                  END IF
 250           CONTINUE
 260        CONTINUE
C                                       Flush buffer.
         IF ((.NOT.DROP1) .OR. (I3.GE.LIM3)) THEN
            CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'FINI', IRET
               GO TO 990
               END IF
C                                       Update CATBLK.
            CATR(KRDMX) = OUTMAX
            CATR(KRDMN) = OUTMIN
            CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', RDATA,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1260) IRET
               GO TO 990
               END IF
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
      CALL DONINE (IPOS, RDATA, BUFF2, IRET)
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
 1000 FORMAT ('NINSND: ERROR',I3,' OPENING INPUT FILE')
 1020 FORMAT ('NINSND: ERROR',I5,' OPENING OUTPUT FILE')
 1099 FORMAT ('NINSND: COMOF3 ERROR',I3)
 1100 FORMAT ('NINSND: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('NINSND: ',A4,' ERROR',I3)
 1180 FORMAT ('NINSND: DONINE ERROR',I3)
 1260 FORMAT ('NINSND: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE NINHIS
C-----------------------------------------------------------------------
C   NINHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR
      LOGICAL   T
      INCLUDE 'NINER.INC'
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
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, SCRTCH,
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
 1000 FORMAT ('NINHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,' BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,' TRC =',6(F6.0,','),F6.0)
 2002 FORMAT (A6,' OPCODE = ''',A4,'''')
      END
      SUBROUTINE NININI (IRET)
C-----------------------------------------------------------------------
C   NININI is a routine in which the user performs several operations
C   associated with beginning the task.
C   The following functions are performed in NININI:
C       1) Modifying the catalog header block to represent the
C   output file.
C       2) Checking the input image and/or input parameters.
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
      CHARACTER FCHARS(3)*4, BLANK*8, CHTMP*4, CODES(15)*4
      INTEGER   LIMIT, I, FIRSTI, FIRSTO, IRET
      INTEGER   NCODE, INDXI, INDEX
      LOGICAL   LDROP1
      INCLUDE 'NINER.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA FCHARS /'FREQ','VELO','FELO'/
      DATA BLANK /'        '/
C                                       # and value of OPCODEs
      DATA NCODE /15/
      DATA CODES /'SOBL', 'KRSH', 'WALL', 'NINE', 'NORT','NE  ',
     *    'EAST','SE  ', 'SOUT', 'SW  ', 'WEST', 'NW  ','MSK1',
     *    'MSK2','MSK3'/
C                                       If LDROP1 is .TRUE. then the
C                                       first axis will be dropped,
C                                       (ie, one value results from
C                                       the operation on each row.)
      DATA LDROP1 /.FALSE./
C-----------------------------------------------------------------------
C                                       Set DROP1
      DROP1 = LDROP1
C                                       Set default OPCODE
      ICODE = 1
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
      IF (DROP1) THEN
         LIMIT = LIMIT - 1
         FIRSTI = 0
         END IF
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
         IF (CATBLK(KIALT).NE.0) THEN
            CALL H2CHR (4, 1, CATH(INDEX), CHTMP)
            IF ((CHTMP.EQ.FCHARS(1)) .OR. (CHTMP.EQ.FCHARS(2)) .OR.
     *         (CHTMP.EQ.FCHARS(3))) CATR(KRARP) = CATR(KRARP) -
     *         BLC(FIRSTI+I+1) + 1.0
            END IF
 80      CONTINUE
C                                       If DROP1 reset CATBLK(KIDIM)
C                                       and blank last axis type.
      IF (DROP1) THEN
         CATBLK(KIDIM) = CATOLD(KIDIM) - 1
         INDEX = KHCTP + CATBLK(KIDIM) * 2
         CALL CHR2H (8, BLANK, 1, CATH(INDEX))
         INDEX = CATBLK(KIDIM)
         CATD(KDCRV+INDEX) = 0.0D0
         CATR(KRCRP+INDEX) = 0.0
         CATR(KRCIC+INDEX) = 0.0
         CATBLK(KINAX+INDEX) = 0
         END IF
C                                       Set output image type
C                                       Put other checks here.
C                                       Finished.
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE DONINE (IPOS, RDATA, RESULT, IRET)
C-----------------------------------------------------------------------
C   This subroutine allows operations on an image one row at a time.
C   Input RDATA are Real with blanking if necessary; output values
C   are R   which may also be blanked.  The calling routine keeps track
C   of max., min. and the occurence of blanking.  CATBLK(KINAX) values
C   per call are expected returned.
C   NOTE: blanked values are denoted by the value of the common variable
C   FBLANK.
C       DONINE accumulates a scrolling buffer by returning a negative
C   value of IRET.  This tells the calling routine to defer writing the
C   next row.  When rows are deferred an equal number of calls to
C   DONINE will be made with no input data; this allows reading out any
C   rows left in DONINEs internal buffers.  Such a "no input call" is
C   indicated by a value of IPOS(1) of -1.  The writing of the returned
C   values of these "no input calls" is NOT deferred.
C
C       If IRET .GT. 0 then the output file will be destroyed.
C
C       The current contents of CATBLK will be written back to the
C   catalog after the last call to DONINE.
C
C   Inputs:
C      IPOS(7)   I    BLC (input image) of first value in RDATA
C                     IPOS(1) = -1 => no input data this call.
C                     IPOS(2) = -2 => last call (no input data).
C      RDATA(*)   R    Input row, magic value blanked.
C   Values from commons:
C      ICODE     I    Opcode number from list in NININI.
C      FBLANK    R    Value of blanked pixel.
C      CPARM(10) R    Input adverb array.
C      DPARM(10) R    Input adverb array.
C      CATBLK    I    Output catalog header (also CATR, CATD)
C      CATOLD    I    Input catalog header (also OLDR, OLDD)
C      DROP1     L    True if one output value per call.
C   Output:
C      RESULT(*) R    Output row.
C      IRET      I    Return code   0 => OK
C                               >0 => error, terminate.
C   Output in COMMON
C     CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IRET, IPOS(7)
      REAL      RDATA(*), RESULT(*)
      INTEGER   LROW, I, J, K, I3, J3, IBLC1
      LOGICAL   PRTD
      INCLUDE 'NINER.INC'
      REAL      WSUM, DSUM, MAXDIF, WSUM9, RIGHT, LEFT, XSUM, TOP,
     *   BOTTOM, YSUM, SSUM, TSUM, BUFFER(MAXIMG,3), BLKARR(MAXIMG),
     *   WINDOW(9), WIND33(3,3), FILT(9,11), PRODCT
      SAVE LROW, IBLC1, PRTD, WSUM9, BLKARR, BUFFER
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (WIND33, WINDOW)
C                                       Each row is a built-in filter
      DATA FILT / 1.,  1.,  1.,  1., -2.,  1., -1., -1., -1.,
     *            1.,  1.,  1., -1., -2.,  1., -1., -1.,  1.,
     *           -1.,  1.,  1., -1., -2.,  1., -1.,  1.,  1.,
     *           -1., -1.,  1., -1., -2.,  1.,  1.,  1.,  1.,
     *           -1., -1., -1.,  1., -2.,  1.,  1.,  1.,  1.,
     *            1., -1., -1.,  1., -2., -1.,  1.,  1.,  1.,
     *            1.,  1., -1.,  1., -2., -1.,  1.,  1., -1.,
     *            1.,  1.,  1.,  1., -2., -1.,  1., -1., -1.,
     *            0., -1.,  0., -1.,  4., -1.,  0., -1.,  0.,
     *           -1., -1., -1., -1.,  8., -1., -1., -1., -1.,
     *            1., -2.,  1., -2.,  4., -2.,  1., -2.,  1./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if last call (no input
C                                       data).
      IF (IPOS(1).LE.-2) GO TO 900
C                                       Check if first call per plane.
      IF (IPOS(2).LE.BLC(2)) THEN
C                                       First call per plane.
C                                       Initialize row length, WSUM,
C                                       and CPARM's.
         LROW = (TRC(1) - BLC(1) + 1.01)
         IBLC1 = BLC(2) + 0.1
         PRTD = .FALSE.
C                                       Get appropriate data for filter
         IF ((ICODE.GE.5) .AND. (ICODE.LE.15)) THEN
            DO 40 J = 1,9
               CPARM(J) = FILT(J,ICODE-4)
  40           CONTINUE
            END IF
         IF ((ICODE.GE.4) .AND. (ICODE.LE.15)) THEN
            WSUM9 = 0.0
            DO 80 J = 1,9
               WSUM9 = WSUM9 + CPARM(J)
  80           CONTINUE
            END IF
C                                       Initialize blank array
         DO 120 I = 1,LROW
            BLKARR(I) = FBLANK
 120        CONTINUE
         DO 130 I = 2,3
            CALL RCOPY (LROW, BLKARR, BUFFER(1, I))
 130        CONTINUE
         END IF
C                                       Branch here for subsequent
C                                       calls.
C                                       Print out row number every
C                                       tenth row if appropriate
      IF ((MOD(IPOS(2),10).EQ.0) .AND. (IPOS(2).GE.IBLC1) .AND.
     *   (IPOS(2).LE.TRC(2)) .AND. (DPARM(2).NE.-1) .AND. (.NOT.PRTD))
     *   THEN
         WRITE (MSGTXT,1000) IPOS(2)
         CALL MSGWRT (4)
         IF (IPOS(2).EQ.TRC(2)) PRTD = .TRUE.
         END IF
C                                       Copy top two vectors into bottom
C                                       two vectors of buffer array
      DO 140 I = 1,2
         CALL RCOPY (LROW, BUFFER(1, I+1), BUFFER(1, I))
 140     CONTINUE
C                                       No input, copy blanks into
C                                       buffer array
      IF (IPOS(1).LT.0) THEN
         CALL RCOPY (LROW, BLKARR, BUFFER(1, 3))
C                                       Copy data into buffer array
      ELSE
         CALL RCOPY (LROW, RDATA, BUFFER(1, 3))
         END IF
C                                       Defer call
      IF ((IPOS(2).LE.IBLC1) .AND. (IPOS(1).NE.-1)) THEN
         IRET = -1
         GO TO 999
         END IF
      DO 500 I = 1,LROW
C                                       Form window array
         DO 190 J3 = 1,3
            DO 180 I3 = 1,3
               IF ((I.LE.1) .AND. (I3.LE.1)) THEN
                  WIND33(I3,J3) = FBLANK
               ELSE IF ((I.GE.LROW) .AND. (I3.GE.3)) THEN
                  WIND33(I3,J3) = FBLANK
               ELSE
                  WIND33(I3, J3) = BUFFER(I - 2 + I3, J3)
                  END IF
 180           CONTINUE
 190        CONTINUE
C                                       'SOBL' branch - default
         IF ((ICODE.GT.15) .OR. (ICODE.LT.2)) THEN
            DO 200 J = 1,9
               IF (WINDOW(J).EQ.FBLANK) GO TO 395
 200           CONTINUE
            RIGHT = WINDOW(3) + 2*WINDOW(6) + WINDOW(9)
            LEFT = WINDOW(1) + 2*WINDOW(4) + WINDOW(7)
            XSUM = RIGHT - LEFT
            TOP = WINDOW(1) + 2*WINDOW(2) + WINDOW(3)
            BOTTOM = WINDOW(7) + 2*WINDOW(8) + WINDOW(9)
            YSUM = TOP - BOTTOM
            RESULT(I) = SQRT(XSUM**2 + YSUM**2) + WINDOW(5)*DPARM(1)
C                                       'KRSH' branch
         ELSE IF (ICODE.EQ.2) THEN
C                                       Find max of 5*SSUM - 3*TSUM
            DO 210 J = 1,9
               IF (WINDOW(J).EQ.FBLANK) GO TO 395
 210           CONTINUE
            MAXDIF = 0.0
            I3 = 1
            J3 = 1
            SSUM = 0.0
            TSUM = 0.0
            DO 245 K = 1,8
               MAXDIF = MAX (MAXDIF, ABS(5*SSUM - 3*TSUM))
               SSUM = 0.0
               TSUM = 0.0
               DO 240 J = 1,9
                  IF (J.LE.3) THEN
                     SSUM = SSUM + WIND33(I3, J3)
                  ELSE IF (J.LE.8) THEN
                     TSUM = TSUM + WIND33(I3, J3)
                     END IF
                  IF ((J3.EQ.1) .AND. (I3.NE.3)) THEN
                     I3 = I3 + 1
                  ELSE IF ((I3.EQ.3) .AND. (J3.NE.3)) THEN
                     J3 = J3 + 1
                  ELSE IF ((J3.EQ.3) .AND. (I3.NE.1)) THEN
                     I3 = I3 - 1
                  ELSE
                     J3 = J3 - 1
                     END IF
 240              CONTINUE
 245           CONTINUE
            RESULT(I) = MAX (1.0, MAXDIF)
            RESULT(I) = MAXDIF
C                                       'WALL' branch
         ELSE IF (ICODE.EQ.3) THEN
            IF ((WINDOW(5).EQ.FBLANK).OR.(WINDOW(5).LE.0.0)) GO TO 395
            DO 260 J = 2,8,2
               IF (WINDOW(J).EQ.FBLANK) GO TO 395
 260           CONTINUE
            PRODCT = WINDOW(2)*WINDOW(4)*WINDOW(6)*WINDOW(8)
            IF (PRODCT.LE.0.0) GO TO 395
            RESULT(I) = 0.25 * LOG10 ((WINDOW(5)**4)/ PRODCT)
C
C
C                                       Normal area operator branch
C
         ELSE IF (WSUM9.NE.0.0) THEN
C                                       Non-zero integral case
C                                       Form weighted sum
            WSUM = 0.0
            DSUM = 0.0
            DO 370 J = 1, 9
               IF (WINDOW(J).NE.FBLANK) THEN
                  WSUM = WSUM + CPARM(J)
                  DSUM = DSUM + CPARM(J) * WINDOW(J)
                  END IF
 370           CONTINUE
            IF ((WSUM.EQ.0.0).OR.(WINDOW(5).EQ.FBLANK)) GO TO 395
               RESULT(I) = (DSUM / WSUM) + WINDOW(5)*DPARM(1)
C                                       Zero integral case
         ELSE
            DSUM = 0.0
            DO 390 J = 1, 9
               IF (WINDOW(J).EQ.FBLANK) GO TO 395
               DSUM = DSUM + CPARM(J) * WINDOW(J)
 390           CONTINUE
            RESULT(I) = DSUM + WINDOW(5)*DPARM(1)
            END IF
         GO TO 500
C                                       blank output
 395     RESULT(I) = FBLANK
 500     CONTINUE
      GO TO 999
C                                       Last call - do history etc.
 900  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Currently on row number', I6)
      END
