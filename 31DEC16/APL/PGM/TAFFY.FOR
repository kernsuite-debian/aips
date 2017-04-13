LOCAL INCLUDE 'TAFFY.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD(256), NUMHIS, JBUFSZ, ICODE
      LOGICAL   DROP1
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOT(2), XOPCOD
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, OPCODE*4,
     *   HISCRD(10)*64
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7),
     *   CPARM(10), DPARM(10), BUFF1(MABFSS), BUFF2(MABFSS)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOT,
     *   XSEQO, XDISKO, BLC, TRC, XOPCOD, CPARM, DPARM
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, OPCODE, HISCRD
      COMMON /PARMS/ CATOLD, DROP1, SEQIN, SEQOUT, DISKIN, DISKO,
     *   NEWCNO, OLDCNO, JBUFSZ, ICODE, NUMHIS
      COMMON /BUFRS/ BUFF1, BUFF2
LOCAL END
      PROGRAM TAFFY
C-----------------------------------------------------------------------
C! Template task which performs operation on first axis of input image
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
C   TAFFY allows a user to provide a subroutine which performs an
C   operation on the first axis of the input image writing an output
C   image.  If pixel blanking is desired it should first be done
C   in COMB or some other appropriate task.  The  image should then be
C   transposed (with task TRANS) if necessary such that the desired
C   axis is first.
C   IMPORTANT NOTE: to avoid confusion this task should be renamed.
C   To rename (max. 5 char) and install the new task :
C     1) copy the source code to a new file with the name newname.
C        then add desired code to subroutines NEWHED and DIDDLE.
C             NEWHED allows modification of the catalog header before
C        the output file is created. If the size of the output file
C        is to be different from the input then the necessary
C        changes must be made to NEWHED.  Validity checks on the
C        input parameters or the input image may be made in NEWHED.
C             DIDDLE is passed the image one row at a time.  Any
C        operation desired is made and DIDDLE returns the result.
C        Blanking is fully supported.
C     2) using the source editor change all references to TAFFY to
C        newname.  It is especially important to change the string
C        entered into array PRGM at or near line 61 to the new name.
C     3) compile and link edit with the APL  subroutine
C        library from AIPS.
C     4) copy, rename, and modify as appropriate the TAFFY HELP file.
C        (This file starts with the INPUTS information.)
C
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
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'TAFFY.INC'
      INCLUDE 'INCS:DCAT.INC'
C
C   CHANGE THIS DATA STATMENT. WHEN CHANGING THE PROGRAM NAME.
C
      DATA PRGM /'TAFFY '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL TAFIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL SENDMA (IRET)
C                                       History
      IF (IRET.EQ.0) CALL TAFHIS
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE TAFIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   TAFIN gets input parameters for TAFFY and creates an output file.
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
C   See prologue comments in TAFFY for more details.
C
C   To change the adverb list sent to this task change:
C   1)  the HELP file (Inputs section).
C   2)  the contents of COMMON /INPARM/.  Remember all adverbs are sent
C       as R, INNAME etc. are 12 char. 3 words;
C       INCLASS etc. are 6 char., 2 words.
C       Values will be filled into COMMON /INPARM/ in the order
C       specified in the inputs file.
C   3)  If the first adverb is not INNAME (NAMEIN) then replace
C       NAMEIN in the call to GTPARM with the name of the first
C       adverb.
C   4)  Change the value of NPARM sent to GTPARM to the number of
C       R words desired.
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
      INCLUDE 'TAFFY.INC'
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
      NPARM = 49
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
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
C                                       Crunch input parameters.
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
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
 1000 FORMAT ('TAFIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('TAFIN: ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE SENDMA (IRET)
C-----------------------------------------------------------------------
C   SENDMA sends image one row at a time to the user supplied
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
      REAL      OUTMAX, OUTMIN, OLD4(256)
      DOUBLE PRECISION    OLD8(128)
      LOGICAL   T, F, BLNKD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'TAFFY.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (CATOLD, OLD4, OLD8)
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
         OUTCNT = NYO
         IF (DROP1) OUTCNT = 1
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
            IF ((DROP1) .AND. (I2.GT.1)) GO TO 180
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
C                                       Call DIDDLE
 180        IF (DIDRET.GE.0) OBIND = OBIND + 1
            CALL DIDDLE (IPOS, BUFF1(IBIND), BUFF2(OBIND), IRET)
            DIDRET = IRET
            IF (DIDRET.GT.0) THEN
               WRITE (MSGTXT,1180) IRET
               GO TO 990
C                                       Check if output deferred.
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
C                                       from DIDDLE.
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
            OBIND = OBIND + 1
            CALL DIDDLE (IPOS, BUFF1(IBIND), BUFF2(OBIND), IRET)
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
            CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFF1,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1260) IRET
               GO TO 990
               END IF
            END IF
 300                 CONTINUE
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
      CALL DIDDLE (IPOS, BUFF1, BUFF2, IRET)
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
 1000 FORMAT ('SENDMA: ERROR',I3,' OPENING INPUT FILE')
 1020 FORMAT ('SENDMA: ERROR',I5,' OPENING OUTPUT FILE')
 1099 FORMAT ('SENDMA: COMOFF ERROR',I3)
 1100 FORMAT ('SENDMA: INIT-FOR-',A4,' ERROR',I3)
 1113 FORMAT ('SENDMA: OUTCNT.LT.0! Too many output rows returned.')
 1120 FORMAT ('SENDMA: ',A,' ERROR',I3)
 1180 FORMAT ('SENDMA: DIDDLE ERROR',I3)
 1260 FORMAT ('SENDMA: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE TAFHIS
C-----------------------------------------------------------------------
C   TAFHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER LABEL*8, LINE*80, NOTTYP(1)*2
      INTEGER   LUN1, LUN2, IERR, I, NONOT
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'TAFFY.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C                                       Change to omit copying tables
C                                       such 'CC' if appropriate
      DATA NONOT, NOTTYP /1, '  '/
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
      WRITE (MSGTXT,2000) TSKNAM, BLC
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       TRC
      WRITE (MSGTXT,2001) TSKNAM, TRC
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       OPCODE
      WRITE (MSGTXT,2002) TSKNAM, OPCODE
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any user supplied history.
      IF (NUMHIS.LE.0) GO TO 200
         WRITE (LABEL,1010) TSKNAM
         LINE(1:8) = LABEL(1:8)
         DO 50 I = 1,NUMHIS
            LINE(9:64) = HISCRD(I)(1:64)
            CALL HIADD (LUN2, LINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
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
 1000 FORMAT ('TAFHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 2000 FORMAT (A6,' BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,' TRC =',6(F6.0,','),F6.0)
 2002 FORMAT (A6,' OPCODE = ''',A4,'''')
      END
      SUBROUTINE NEWHED (IRET)
C-----------------------------------------------------------------------
C   NEWHED is a routine in which the user performs several operations
C   associated with beginning the task.  For many purposes simply
C   changing some of the values in the DATA statments will be all that
C   is necessary.  The following functions are/can be performed
C   in NEWHED:
C       1) Modifying the catalog header block to represent the
C   output file.  The MINIMUM modifications required here are those
C   required to define the size of the output file; ie.
C      CATBLK(KIDIM)   = the number of axes,
C      CATBLK(KINAX+i) = the dimension of each axis, and
C   Other changes can be made either here or in DIDDLE; the
C   catalog block will be updated when the history file is
C   written.
C       2) Checking the input image and/or input parameters.
C   For example, if a given first axis type such as
C   Frequency/Velocity is required this should be checked.  The
C   routine currently does this and all that is required to
C   implement this is to modify the DATA statments.
C   A returned value of IRET .NE. 0 will cause the task to terminate.
C   A message to the user via MSGWRT about the reason for the
C   termination would be friendly.  This can be done by encoding
C   the message into MSGTXT, setting IRET to a non-zero value
C   and issuing a GO TO 990.
C       3) Setting default values of some of the input parameters
C   (OUTNAME, OUTCLASS, OUTSEQ, OUTDISK, TRC and BLC defaults are
C   set elsewhere).  As currently set the default OPCODE is the
C   first value in the array CODES which is set in a data statment.
C
C    Input:
C       CATBLK    I(256)  Output catalog header, also CATR, CATD
C       CATOLD    I(256)  Input catalog header, also OLD4, OLD8
C    Output:
C       CATBLK    I(256)  Modified output catalog header.
C       IRET      I       Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER ATYPES(10)*8, FCHARS(3)*4, BLANK*8, CODES(10)*4,
     *   UNITS(10)*8, CTEMP*8
      HOLLERITH OLD4(256)
      DOUBLE PRECISION    OLD8(128)
      INTEGER   NCODE, NTYPES, IOFF, IERR, INDXI, INC, INDEX,
     *   NCHTYP(10), LIMIT, I, FIRSTI, FIRSTO
      LOGICAL   LDROP1
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'TAFFY.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (CATOLD, OLD4, OLD8)
      DATA FCHARS /'FREQ','VELO','FELO'/
      DATA BLANK /'        '/
C                                       User definable values
C                                       # and value of OPCODEs
      DATA NCODE /0/
      DATA CODES /10*'    '/
C                                       Output units for each OPCODE.
C                                       Two R   words with 4 char. ea.
      DATA UNITS /'UNDEFINE',9*'    '/
C                                       Allowed number of axis types
C                                       and types.
      DATA NTYPES /0/
      DATA ATYPES /10*'        '/
      DATA NCHTYP /10*4/
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
      IF (NCODE.GT.0) THEN
         DO 10 I = 1,NCODE
            ICODE = I
            IF (OPCODE.EQ.CODES(I)) GO TO 20
 10         CONTINUE
C                                       Default OPCODE is first.
         OPCODE = CODES(1)
         ICODE = 1
         END IF
C                                       Set output units.
 20   IF (NCODE.GT.1) CALL CHR2H (8, UNITS(ICODE), 1, CATH(KHBUN))
C                                       Check allowed axis types
      IF (NTYPES.GT.0) THEN
         DO 30 I = 1, NTYPES
C                                       Check for type.
            CALL AXEFND (NCHTYP(I), ATYPES(I), KICTPN, OLD4(KHCTP),
     *         IOFF, IERR)
C                                       IOFF is axis number
C                                       IERR = 0 if axis found.
            IF (IERR.EQ.0) GO TO 40
 30         CONTINUE
C                                       Axis not found.
         IOFF = -1
C                                       Check if axis first
 40      IF (IOFF.EQ.0) GO TO 50
C                                       Axis not found or not first.
            IRET = 1
            IF (IOFF.GT.0) WRITE (MSGTXT,1040)
            IF (IOFF.LT.0) WRITE (MSGTXT,1041)
            CALL MSGWRT (8)
            WRITE (MSGTXT,1042) (ATYPES(I), I = 1,NTYPES)
            GO TO 990
         END IF
C                                       Set axes in output CATBLK.
  50  FIRSTI = -1
      FIRSTO = -1
      LIMIT = CATOLD(KIDIM)
      INC = 2
      IF (.NOT.DROP1) GO TO 60
         LIMIT = LIMIT - 1
         FIRSTI = 0
C                                       Copy/update axis values
 60   DO 80 I = 1,LIMIT
         CATBLK(KINAX+FIRSTO+I) = TRC(I+FIRSTI+1) -
     *      BLC(I+FIRSTI+1) + 1.01
         CATR(KRCRP+FIRSTO+I) = OLD4(KRCRP+FIRSTI+I) - BLC(I+FIRSTI+1)
     *      + 1.0
         CATR(KRCIC+FIRSTO+I) = CATR(KRCIC+FIRSTI+I)
         CATD(KDCRV+FIRSTO+I) = OLD8(KDCRV+FIRSTI+I)
         INDXI = KHCTP + (I+FIRSTI) * INC
         INDEX = KHCTP + (I-1) * INC
         CALL H2CHR (8, 1, CATH(INDEX), CTEMP)
         CALL CHR2H (8, CTEMP, 1, OLD4(INDXI))
         IF (CATBLK(KIALT).EQ.0) GO TO 80
            CALL H2CHR (4, 1, CATH(INDEX), CTEMP)
            IF ((CTEMP(1:4).EQ.FCHARS(1)) .OR.
     *          (CTEMP(1:4).EQ.FCHARS(2)) .OR.
     *          (CTEMP(1:4).EQ.FCHARS(3)))
     *      CATR(KRARP) = CATR(KRARP) - BLC(FIRSTI+I+1) + 1.0
 80      CONTINUE
C                                       If DROP1 reset CATBLK(KIDIM)
C                                       and blank last axis type.
      IF (.NOT.DROP1) GO TO 90
         CATBLK(KIDIM) = CATOLD(KIDIM) - 1
         INDEX = KHCTP + CATBLK(KIDIM) * INC
         CALL CHR2H (8, BLANK, 1, CATH(INDEX))
         INDEX = CATBLK(KIDIM)
         CATD(KDCRV+INDEX) = 0.0D0
         CATR(KRCRP+INDEX) = 0.0
         CATR(KRCIC+INDEX) = 0.0
         CATBLK(KINAX+INDEX) = 0
C                                       Put other checks here.
C                                       Finished.
 90   IRET = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('REQUIRED FIRST AXIS NOT FIRST, TRANSPOSE IMAGE')
 1041 FORMAT ('REQUIRED FIRST AXIS TYPE NOT FOUND IN IMAGE')
 1042 FORMAT ('REQUIRED TYPES ARE:',10(1X,A4))
      END
      SUBROUTINE DIDDLE (IPOS, DATA, RESULT, IRET)
C-----------------------------------------------------------------------
C   This is a skeleton version of subroutine DIDDLE which allows
C   operations on an image one row at a time (1st dimension).
C   Input DATA are Real*4 with blanking if necessary; output values
C   are R   which may also be blanked.  The calling routine keeps track
C   of max., min. and the occurence of blanking.  If DROP1 is .TRUE.,
C   the calling routine expects 1 value returned per call;
C   otherwise, CATBLK(KINAX) values per call are expected returned.
C   NOTE: blanked values are denoted by the value of the common variable
C   FBLANK.
C       DIDDLE may accumulate a scrolling buffer by returning a negative
C   value of IRET.  This tells the calling routine to defer writting the
C   next row.  If rows are deferred then and equal number of calls to
C   DIDDLE will be made with no input data; this allows reading out any
C   rows left in DIDDLEs internal buffers.  Such a "no input call" is
C   indicated by a value of IPOS(1) of -1.  The writting of the returned
C   values of these "no input calls" may NOT be deferred.
C       Up to 10 history entries can be written by using ENCODE to
C   record up to 64 characters per entry into array HISCRD. Ex:
C         WRITE (HISCRD(entry #), format) list
C   TRC, BLC and OPCODE are already taken care of.
C   The history is written after the last call to DIDDLE.
C       Messages can be written to the monitor/logfile by encoding
C   the message (up to 80 char) into array MSGTXT in COMMON /MSGCOM/
C   and then issuing a call:
C        CALL MSGWRT (priority #)
C   Unit 1 is the line printer
C
C       If IRET .GT. 0 then the output file will be destroyed.
C
C       After all data have been processed a final call will be made to
C   DIDDLE with IPOS(1)=-2.  This is to allow for the completion of
C   pending operations, i.e. preparation of HIstory cards.
C
C       LUN's 16-18 are open and not available to DIDDLE.
C
C       The current contents of CATBLK will be written back to the
C   catalog after the last call to DIDDLE.
C
C   Inputs:
C      IPOS   I(7)    BLC (input image) of first value in DATA
C                     IPOS(1) = -1 => no input data this call.
C                     IPOS(2) = -2 => last call (no input data).
C      DATA   R(*)    Input row, magic value blanked.
C   Values from commons:
C      ICODE   I      Opcode number from list in NEWHED.
C      FBLANK  R      Value of blanked pixel.
C      CPARM   R(10)  Input adverb array.
C      DPARM   R(10)  Input adverb array.
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
      REAL      DATA(*), RESULT(*)
C
      REAL      OLD4(256)
      INTEGER   LROW
      DOUBLE PRECISION OLD8(128)
C                                       SAVE variables in first call
      SAVE LROW
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'TAFFY.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (CATOLD, OLD4, OLD8)
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if last call (no input
C                                       data).
      IF (IPOS(1).GT.-2) THEN
C                                       Check if first call per plane.
         IF (IPOS(2).LE.1) THEN
C                                       First call per plane.
C                                       Set length of a row
            LROW = CATBLK(KINAX)
            END IF
C                                       Branch here for subsequent
C                                       calls.
C                                       Example, copy input to output.
         CALL RCOPY (LROW, DATA, RESULT)
C
C    USER CODE GOES HERE
C
C
C
C
C
C
C
C
C                                       Last call - do history etc.
      ELSE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
C   FORMAT STATEMENTS GO HERE
      END
