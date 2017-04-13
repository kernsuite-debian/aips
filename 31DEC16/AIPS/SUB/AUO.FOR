      SUBROUTINE AUO (BRANCH)
C-----------------------------------------------------------------------
C! verbs to run OBIT tasks
C# POPS-appl Imaging UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2011-2013, 2015
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
C   AUO causes various OBIT tasks to run
C   BRANCH = 1   BDFLIST   list contents of EVLA ASDM data set
C          = 2   BDF2AIPS  read in contents of EVLA ASDM data set
C          = 3   OBITMAP   simplest version of Imager
C          = 4   OBITIMAG  less simple version of Imager
C          = 5   OBITSCAL  self-cal version of Imager
C          = 6   OBITPEEL  self-cal/peel version of Imager
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INTEGER   NOBIT
      PARAMETER (NOBIT=6)
C
      CHARACTER PRGNAM*6, TXLINR*512, ATIME*8, ADATE*12, RUNFIL*256,
     *   OBITAS(NOBIT)*8, VERBNA(NOBIT)*8, LOGFIL*256, ASDMF(2)*64,
     *   ASDMFL*192, ARUNF*48, ALOGF*48, HOME*4, XLATED*132, CDATE*16,
     *   T1*132, T2*132, CSCRTC*132, CLASS*6, PDATEI*9, PDATET*9,
     *   NAME*12, SOURCE(30)*16
      INTEGER   LUNR, LUNL, TIME(3), DATE(3), J, JTRIM, INDR, INDL, I,
     *   IERR, POTERR, XLNB, LUNP, INDP, NACROS, SCRTCH(256), NLINE,
     *   IPAGE, IBAD(10), JJ, IROUND, II
      REAL      R, DOWAIT, DOCRT, CELLS(2), IM2PRM(40), TIMER(8),
     *   IMGPRM(20), WGAUSS(10)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DISK.INC'
      DATA PRGNAM /'AUO'/
      DATA OBITAS /'ASDMList', 'BDFIn',    'Imager',  'Imager',
     *   'Imager',   'Imager'/
      DATA VERBNA /'BDFLIST',  'BDF2AIPS', 'OBITMAP', 'OBITIMAG',
     *   'OBITSCAL', 'OBITPEEL'/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.NOBIT)) GO TO 999
C                                       TPUT immediate argument
      NAME = VERBNA(BRANCH)
      CALL CHR2H (4, '    ', 1, R)
C                                       store as litoral
      CALL RFILL(5, R, X)
      CALL CHR2H (8, NAME, 1, X)
      SYTYPE = 14
      LX = 2
      CALL LTSTOR
      IF (ERRNUM.EQ.1) ERRNUM = 75
      IF (ERRNUM.NE.0) GO TO 980
      K(LOCSYM+1) = SYTYPE + 16*8
C                                       litoral on stack
      CALL PUSH (STACK, SP, SYTYPE)
      IF (ERRNUM.NE.0) GO TO 980
      CALL PUSH (STACK, SP, 8)
      IF (ERRNUM.NE.0) GO TO 980
      CALL PUSH (STACK, SP, TAG)
      IF (ERRNUM.NE.0) GO TO 980
      CALL PUSH (STACK, SP, 2)
      IF (ERRNUM.NE.0) GO TO 980
C                                       do the TPUT
      I = 5
      CALL AU2 (I)
      IF (ERRNUM.NE.0) GO TO 980
C                                       common adverbs
      CALL ADVERB ('DOWAIT', 'R', 1, 0, I, DOWAIT, TXLINR)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('DOCRT', 'R', 1, 0, I, DOCRT, TXLINR)
      IF (ERRNUM.NE.0) GO TO 980
C                                       task run and log files
      CALL ZDATE (DATE)
      WRITE (PDATEI,1005) DATE
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      ADATE(12:12) = '.'
      CDATE = ADATE(1:2) // ADATE(4:6) // ADATE (8:12) // ATIME(1:2)
     *   // ATIME(4:5) // ATIME(7:8)
      J = JTRIM (VERBNA(BRANCH))
      HOME = 'HOME'
      CALL ZTRLOG (4, HOME, 132, XLATED, XLNB, IERR)
      XLNB = XLNB + 1
      XLATED(XLNB:) = '/'
      RUNFIL = XLATED(:XLNB) // VERBNA(BRANCH)(:J) // '.run.' //
     *   CDATE
      LOGFIL = XLATED(:XLNB) // VERBNA(BRANCH)(:J) // '.log.' //
     *   CDATE
      ARUNF =  'HOME:' // VERBNA(BRANCH)(:J) // '.run.' // CDATE
      ALOGF = 'HOME:' // VERBNA(BRANCH)(:J) // '.log.' // CDATE
      LUNL = 3
      LUNR = 11
      INDL = 0
      INDR = 0
      ERRNUM = 0
      POTERR = 50
      CALL ZTXOPN ('WRIT', LUNL, INDL, ALOGF, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING LOG FILE'
         GO TO 970
         END IF
      CALL ZTXOPN ('WRIT', LUNR, INDR, ARUNF, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING RUN FILE'
         GO TO 970
         END IF
C                                       common text in run files
      TXLINR = '#AIPS: Runfile for ObitTalk created ' // ADATE // ATIME
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      WRITE (TXLINR,1010) NLUSER
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      TXLINR = 'from OTObit import *'
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      TXLINR = 'AIPS.AIPS.userno=user'
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      TXLINR = 'OSystem.PSetAIPSuser (user)'
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      TXLINR = 'err=OErr.OErr()'
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      J = JTRIM (OBITAS(BRANCH))
      TXLINR = 'x=ObitTask("' // OBITAS(BRANCH)(:J) // '")'
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       end of common stuff
      GO TO (100, 200, 300, 300, 300, 300), BRANCH
C-----------------------------------------------------------------------
C                                       BDFLIST
C                                       list contents of ASDM/BDF file
C-----------------------------------------------------------------------
 100  CALL ADVERB ('ASDMFILE', 'C', 2, 64, I, R, ASDMF)
      IF (ERRNUM.NE.0) GO TO 975
      IF (ASDMF(2).EQ.ASDMF(1)) ASDMF(2) = ' '
      J = JTRIM (ASDMF(1))
C                                       do we have a logical?
      CSCRTC = ' '
      IF (ASDMF(1)(:1).EQ.'$') THEN
         I = INDEX (ASDMF(1), '/')
         IF (I.LE.2) I = J + 1
         IF (I.GT.2) CSCRTC = ASDMF(1)(2:I-1)
      ELSE
         I = INDEX (ASDMF(1), ':')
         IF (I.GT.1) CSCRTC = ASDMF(1)(1:I-1)
         END IF
      IF (CSCRTC.EQ.' ') THEN
         ASDMFL = ASDMF(1)(:J) // ASDMF(2)
      ELSE
         II = JTRIM (CSCRTC)
         CALL ZTRLOG (II, CSCRTC, 132, XLATED, XLNB, IERR)
         IF (IERR.NE.0) THEN
            POTERR = 101
            MSGTXT = 'CANNOT TRANSLATE ''' // CSCRTC(:II) // ''''
            CALL MSGWRT (8)
            GO TO 980
            END IF
         XLNB = XLNB + 1
         XLATED(XLNB:) = '/'
         IF (J.GE.I+1) THEN
            ASDMFL = XLATED(1:XLNB) // ASDMF(1)(I+1:J) // ASDMF(2)
         ELSE
            ASDMFL = XLATED(1:XLNB) // ASDMF(2)
            END IF
         END IF
      J = JTRIM (ASDMFL)
      TXLINR = 'x.DataRoot="' // ASDMFL(:J) // '"'
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      TXLINR = 'x.DataType="FITS"'
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      TXLINR = 'x.doCrt=-1'
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      J = JTRIM (LOGFIL)
      TXLINR = 'x.prtFile="' // LOGFIL(:J) // '"'
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      IF (DOWAIT.GT.0.0) DOWAIT = 2.0
C                                       rest is common
      GO TO 900
C-----------------------------------------------------------------------
C                                       BDF2AIPS
C                                       read contents of ASDM/BDF file
C-----------------------------------------------------------------------
 200  CALL ADVERB ('ASDMFILE', 'C', 2, 64, I, R, ASDMF)
      IF (ERRNUM.NE.0) GO TO 975
      IF (ASDMF(2).EQ.ASDMF(1)) ASDMF(2) = ' '
      J = JTRIM(ASDMF(1))
C                                       do we have a logical?
      CSCRTC = ' '
      IF (ASDMF(1)(:1).EQ.'$') THEN
         I = INDEX (ASDMF(1), '/')
         IF (I.LE.2) I = J + 1
         IF (I.GT.2) CSCRTC = ASDMF(1)(2:I-1)
      ELSE
         I = INDEX (ASDMF(1), ':')
         IF (I.GT.1) CSCRTC = ASDMF(1)(1:I-1)
         END IF
      IF (CSCRTC.EQ.' ') THEN
         ASDMFL = ASDMF(1)(:J) // ASDMF(2)
      ELSE
         II = JTRIM (CSCRTC)
         CALL ZTRLOG (II, CSCRTC, 132, XLATED, XLNB, IERR)
         IF (IERR.NE.0) THEN
            POTERR = 101
            MSGTXT = 'CANNOT TRANSLATE ''' // CSCRTC(:II) // ''''
            CALL MSGWRT (8)
            GO TO 980
            END IF
         XLNB = XLNB + 1
         XLATED(XLNB:) = '/'
         IF (J.GE.I+1) THEN
            ASDMFL = XLATED(1:XLNB) // ASDMF(1)(I+1:J) // ASDMF(2)
         ELSE
            ASDMFL = XLATED(1:XLNB) // ASDMF(2)
            END IF
         END IF
      J = JTRIM (ASDMFL)
      TXLINR = 'x.DataRoot="' // ASDMFL(:J) // '"'
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      TXLINR = 'x.DataType="AIPS"'
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('OUTNAME', 'C', 1, 12, I, R, ADATE)
      IF (ERRNUM.NE.0) GO TO 975
      IF (ADATE.EQ.' ') ADATE = 'BDF2AIPS'
      J = JTRIM (ADATE)
      TXLINR = 'x.outName="' // ADATE(:J) // '"'
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('OUTCLASS', 'C', 1, 6, I, R, CLASS)
      IF (ERRNUM.NE.0) GO TO 975
      IF (CLASS.EQ.' ') CLASS = 'UVEVLA'
      J = JTRIM (CLASS)
      TXLINR = 'x.outClass="' // CLASS(:J) // '"'
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('OUTDISK', 'I', 1, 1, I, R, CLASS)
      IF (ERRNUM.NE.0) GO TO 975
      IF (((I.LE.0) .OR. (I.GT.MINDSK)) .AND. (MINDSK.GT.0)) I = MINDSK
      IF ((I.LE.0) .OR. (I.GT.NVOL)) I = NVOL
      WRITE (CLASS,1200) I
      TXLINR = 'x.outDisk=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('CLINT', 'R', 1, 1, I, R, CLASS)
      IF (ERRNUM.NE.0) GO TO 975
      IF (R.LE.0.0) R = 0.25
      WRITE (ADATE,1210) R
      TXLINR = 'x.calInt=' // ADATE
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('NCHAN', 'I', 1, 1, I, R, CLASS)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.selChan=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('NIF', 'I', 1, 1, I, R, CLASS)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.selIF=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('ORDER', 'R', 1, 1, I, R, CLASS)
      IF (ERRNUM.NE.0) GO TO 975
      IF (R.LE.0.0) THEN
         TXLINR = 'x.SWOrder=True'
      ELSE
         TXLINR = 'x.SWOrder=False'
         END IF
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('BAND', 'C', 1, 4, I, R, CLASS)
      IF (ERRNUM.NE.0) GO TO 975
      J = JTRIM (CLASS)
      TXLINR = 'x.selBand="' // CLASS(:J) // '"'
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('CALCODE', 'C', 1, 4, I, R, CLASS)
      IF (ERRNUM.NE.0) GO TO 975
      IF (CLASS.EQ.'-CAL') CLASS = 'NONE'
      IF (CLASS.EQ.'ALL') CLASS = '*'
      J = JTRIM (CLASS)
      TXLINR = 'x.selCode="' // CLASS(:J) // '"'
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('CONFIG', 'I', 1, 1, I, R, CLASS)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.selConfig=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('DOUVCOMP', 'R', 1, 1, I, R, CLASS)
      IF (ERRNUM.NE.0) GO TO 975
      IF (R.GT.0.0) THEN
         TXLINR = 'x.Compress=True'
      ELSE
         TXLINR = 'x.Compress=False'
         END IF
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      TXLINR = 'x.doCode=True'
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      TXLINR = 'x.doSwPwr=False'
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('DOKEEP', 'R', 1, 1, I, R, CLASS)
      IF (ERRNUM.NE.0) GO TO 975
      IF (R.GT.0.0) THEN
         TXLINR = 'x.doOnline=True'
      ELSE
         TXLINR = 'x.doOnline=False'
         END IF
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      GO TO 900
C-----------------------------------------------------------------------
C                                       OBITMAP
C                                       simplified access to Imager
C-----------------------------------------------------------------------
 300  TXLINR = 'x.DataType="AIPS"'
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('INNAME', 'C', 1, 12, I, R, NAME)
      IF (ERRNUM.NE.0) GO TO 975
      J = JTRIM (NAME)
      TXLINR = 'x.inName="' // NAME(:J) // '"'
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('INCLASS', 'C', 1, 6, I, R, CLASS)
      IF (ERRNUM.NE.0) GO TO 975
      J = JTRIM (CLASS)
      TXLINR = 'x.inClass="' // CLASS(:J) // '"'
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('INSEQ', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.inSeq=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('INDISK', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.inDisk=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('STOKES', 'C', 1, 4, I, R, NAME)
      IF (ERRNUM.NE.0) GO TO 975
      TXLINR = 'x.Stokes="' // NAME(:1) // '"'
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('BCHAN', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.BChan=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('ECHAN', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.EChan=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('CHINC', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.chInc=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('NCHAV', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.chAvg=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('BIF', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.BIF=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('EIF', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.EIF=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      TXLINR = 'x.outDType="AIPS"'
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('OUTNAME', 'C', 1, 12, I, R, NAME)
      IF (ERRNUM.NE.0) GO TO 975
      J = JTRIM (NAME)
      TXLINR = 'x.outName="' // NAME(:J) // '"'
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('OUTCLASS', 'C', 1, 6, I, R, CLASS)
      IF (ERRNUM.NE.0) GO TO 975
      J = JTRIM (CLASS)
      TXLINR = 'x.outClass="' // CLASS(:J) // '"'
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('OUTSEQ', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.outSeq=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('OUTDISK', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      IF (((I.LE.0) .OR. (I.GT.MINDSK)) .AND. (MINDSK.GT.0)) I = MINDSK
      IF ((I.LE.0) .OR. (I.GT.NVOL)) I = NVOL
      WRITE (CLASS,1200) I
      TXLINR = 'x.outDisk=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('FOV', 'R', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      IF (R.LE.0.0) THEN
         MSGTXT = 'MUST GIVE A FIELD OF VIEW'
         ERRNUM = 101
         GO TO 970
         END IF
      WRITE (NAME,1210) R
      CALL FPTRIM (NAME, NAME, J)
      TXLINR = 'x.FOV=' // NAME(:J)
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('CELLSIZE', 'R', 2, 1, I, CELLS, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (NAME,1210) CELLS(1)
      CALL FPTRIM (NAME, NAME, J)
      TXLINR = 'x.xCells=' // NAME(:J)
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      WRITE (NAME,1210) CELLS(2)
      CALL FPTRIM (NAME, NAME, J)
      TXLINR = 'x.yCells=' // NAME(:J)
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('UVTAPER', 'R', 2, 1, I, CELLS, T1)
      IF (ERRNUM.NE.0) GO TO 975
      IF ((CELLS(1).GT.0.0) .OR. (CELLS(2).GT.0.0)) THEN
         WRITE (NAME,1210) CELLS(1)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.UVTaper=[' // NAME(:J)
         WRITE (NAME,1210) CELLS(1)
         CALL FPTRIM (NAME, NAME, J)
         I = JTRIM (TXLINR)
         TXLINR(I+1:) = ',' // NAME(:J) // ', 0.0]'
         CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
      CALL ADVERB ('ROBUST', 'R', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (NAME,1210) R
      CALL FPTRIM (NAME, NAME, J)
      TXLINR = 'x.Robust=' // NAME(:J)
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('NITER', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.Niter=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('FLUX', 'R', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (NAME,1210) R
      CALL FPTRIM (NAME, NAME, J)
      TXLINR = 'x.minFlux=' // NAME(:J)
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('IM2PARM', 'R', 40, 1, I, IM2PRM, T1)
      IF (ERRNUM.NE.0) GO TO 975
      IF (IM2PRM(1).GT.0.0) THEN
         TXLINR = 'x.autoWindow=True'
      ELSE
         TXLINR = 'x.autoWindow=False'
         END IF
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('DOTV', 'R', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      IF (R.GT.0.0) THEN
         TXLINR = 'x.dispURL="ObitView"'
      ELSE
         TXLINR = 'x.dispURL="None"'
         END IF
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('NTHREAD', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      I = MAX (I, 1)
      WRITE (CLASS,1200) I
      TXLINR = 'x.nThreads=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('BADDISK', 'I', 10, 1, IBAD, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      DO 310 I = 1,10
         IF (IBAD(I).GT.0) THEN
            WRITE (CLASS,1200) IBAD(I)
            IF (I.EQ.1) THEN
               TXLINR = 'x.noScrat=[' // CLASS(5:6)
               J = JTRIM (TXLINR)
            ELSE
               TXLINR(J+1:) = ',' // CLASS(5:6)
               J = J + 3
               END IF
         ELSE
            GO TO 315
            END IF
 310     CONTINUE
 315  IF (I.GT.1) THEN
         J = JTRIM (TXLINR)
         TXLINR(J+1:) = ']'
         CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
      CALL ADVERB ('PRTLEV', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      IF (I.EQ.0) I = 2
      IF (I.LT.0) I = 0
      WRITE (CLASS,1200) I
      TXLINR = 'x.prtLv=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       3D false => flatten
      IF (BRANCH.NE.3) THEN
         CALL ADVERB ('DO3DIM', 'R', 1, 1, I, R, T1)
         IF (ERRNUM.NE.0) GO TO 975
      ELSE
         R = -1.0
         END IF
      IF (R.LE.0.0) THEN
         TXLINR = 'x.do3D=False'
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         TXLINR = 'x.doFull=True'
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
      ELSE
         TXLINR = 'x.do3D=True'
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         TXLINR = 'x.doFull=False'
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
      IF (BRANCH.EQ.3) GO TO 900
C-----------------------------------------------------------------------
C                                       OBITIMAG
C                                       greater access to Imager
C-----------------------------------------------------------------------
 400  CALL ADVERB ('SOURCES', 'C', 30, 16, I, R, SOURCE)
      IF (ERRNUM.NE.0) GO TO 975
      JJ = -1
      DO 405 I = 1,30
         J = JTRIM (SOURCE(I))
         IF (J.GT.0) THEN
            JJ = JJ + 1
            IF (JJ.LE.9) THEN
               WRITE (TXLINR,1400) JJ, SOURCE(I)(:J)
            ELSE
               WRITE (TXLINR,1401) JJ, SOURCE(I)(:J)
               END IF
            CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
            IF (IERR.NE.0) GO TO 975
            END IF
 405     CONTINUE
      CALL ADVERB ('QUAL', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.Qual=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('CALCODE', 'C', 1, 4, I, R, CLASS)
      IF (ERRNUM.NE.0) GO TO 975
      J = JTRIM (CLASS(:4))
      J = MAX (1, J)
      TXLINR = 'x.souCode="' // CLASS(:J) // '"'
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       time range
      CALL ADVERB ('TIMERANG', 'R', 8, 1, I, TIMER, T1)
      IF (ERRNUM.NE.0) GO TO 975
      CELLS(1) = TIMER(1) + (TIMER(2) + (TIMER(3) + TIMER(4)/60.) / 60.)
     *   / 24.0
      CELLS(2) = TIMER(5) + (TIMER(6) + (TIMER(7) + TIMER(8)/60.) / 60.)
     *   / 24.0
      IF (CELLS(2).GT.CELLS(1)) THEN
         WRITE (NAME,1210) CELLS(1)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.timeRange=[' // NAME(:J)
         WRITE (NAME,1210) CELLS(2)
         CALL FPTRIM (NAME, NAME, J)
         I = JTRIM (TXLINR)
         TXLINR(I+1:) = ', ' // NAME(:J) // ']'
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
C                                       freq id and sub array
      CALL ADVERB ('FREQID', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.FreqID=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('SUBARRAY', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.subA=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       calibration
      CALL ADVERB ('DOCALIB', 'R', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      I = IROUND (R)
      IF (R.GT.0.0) I = MAX (1, I)
      IF (R.GT.0.0) THEN
         WRITE (CLASS,1200) I
         TXLINR = 'x.doCalib=' // CLASS
         CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         CALL ADVERB ('GAINUSE', 'I', 1, 1, I, R, T1)
         IF (ERRNUM.NE.0) GO TO 975
         WRITE (CLASS,1200) I
         TXLINR = 'x.gainUse=' // CLASS
         CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
      CALL ADVERB ('DOBAND', 'R', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      IF (R.GT.0.0) THEN
         I = IROUND (R)
         IF (I.LE.0) I = 1
         WRITE (CLASS,1200) I
         TXLINR = 'x.doBand=' // CLASS
         CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         CALL ADVERB ('BPVER', 'I', 1, 1, I, R, T1)
         IF (ERRNUM.NE.0) GO TO 975
         WRITE (CLASS,1200) I
         TXLINR = 'x.BPVer=' // CLASS
         CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
      CALL ADVERB ('DOPOL', 'R', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      IF (R.GT.0.0) THEN
         TXLINR = 'x.doPol=True'
      ELSE
         TXLINR = 'x.doPol=False'
         END IF
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('FLAGVER', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.flagVer=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       bl based time average
      IF (IM2PRM(5).GT.1.0) THEN
         WRITE (NAME,1210) IM2PRM(5)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.BLFact=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         WRITE (NAME,1210) IM2PRM(6)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.BLFOV=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
      CALL ADVERB ('MAXPIXEL', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      WRITE (CLASS,1200) I
      TXLINR = 'x.maxPixel=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       primary beam correction
      CALL ADVERB ('IMAGRPRM', 'R', 20, 1, I, IMGPRM, T1)
      IF (ERRNUM.NE.0) GO TO 975
      IF (IMGPRM(1).GT.0.0) THEN
         TXLINR = 'x.PBCor=True'
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         WRITE (NAME,1210) IMGPRM(1)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.antSize=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
C                                       average spectral index
      IF (IMGPRM(2).NE.0.0) THEN
         WRITE (NAME,1210) IMGPRM(2)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.Alpha=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
C                                       outlier facets
      IF (IM2PRM(2).GT.0.0) THEN
         TXLINR = 'x.CatDisk=0'
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         CSCRTC = 'AIPS_ROOT'
         J = JTRIM (CSCRTC)
         CALL ZTRLOG (J, CSCRTC, 132, XLATED, XLNB, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'CANNOT TRANSLATE AIPS_ROOT'
            POTERR = 101
            GO TO 970
            END IF
         TXLINR = 'x.Catalog="' // XLATED(:XLNB) //
     *      '/TEXT/STARS/NVSSVZ.FIT' // '"'
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         WRITE (NAME,1210) IM2PRM(2)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.OutlierDist=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         IF (IM2PRM(3).LE.0.0) IM2PRM(3) = 0.05
         WRITE (NAME,1210) IM2PRM(3)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.OutlierFlux=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         IF (IM2PRM(4).EQ.0.0) IM2PRM(4) = -0.7
         WRITE (NAME,1210) IM2PRM(4)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.OutlierSI=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
C                                       multi-scale Clean
      CALL ADVERB ('NGAUSS', 'I', 1, 1, JJ, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      IF (JJ.GT.0) THEN
         WRITE (CLASS,1200) JJ
         TXLINR = 'x.nTaper=' // CLASS
         CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         IF (IMGPRM(11).LE.0.0) IMGPRM(11) = 0.2
         IF (IMGPRM(12).LE.0.0) IMGPRM(12) = 0.35
         IF (IMGPRM(13).LE.0.0) IMGPRM(13) = 0.2
         WRITE (NAME,1210) IMGPRM(11)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.MResKnob=[' // NAME(:J)
         I = JTRIM (TXLINR)
         WRITE (NAME,1210) IMGPRM(12)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR(I+1:) = ', ' // NAME(:J)
         I = JTRIM (TXLINR)
         WRITE (NAME,1210) IMGPRM(13)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR(I+1:) = ', ' // NAME(:J) // ']'
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         CALL ADVERB ('WGAUSS', 'R', 10, 1, I, WGAUSS, T1)
         IF (ERRNUM.NE.0) GO TO 975
         WRITE (NAME,1210) WGAUSS(1)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.Tapers=[' // NAME(:J)
         DO 410 II = 2,JJ
            I = JTRIM (TXLINR)
            WRITE (NAME,1210) WGAUSS(II)
            CALL FPTRIM (NAME, NAME, J)
            TXLINR(I+1:) = ',' // NAME(:J)
 410        CONTINUE
         I = JTRIM (TXLINR)
         TXLINR(I+1:) = ']'
         CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
C                                       auto centering
      IF (IMGPRM(3).GT.0.0) THEN
         WRITE (NAME,1210) IMGPRM(3)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.autoCen=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
      IF (BRANCH.EQ.4) GO TO 900
C-----------------------------------------------------------------------
C                                       OBITSCAL
C                                       add self-cal
C-----------------------------------------------------------------------
 500  CALL ADVERB ('REFANT', 'I', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      IF (I.GT.0) THEN
         WRITE (CLASS,1200) I
         TXLINR = 'x.refAnt=' // CLASS
         CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
      CALL ADVERB ('WTUV', 'R', 1, 1, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      IF (R.LE.0.0) R = 0.01
      WRITE (NAME,1210) R
      CALL FPTRIM (NAME, NAME, J)
      TXLINR = 'x.WtUV=' // NAME(:J)
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('SOLTYPE', 'C', 1, 4, I, R, T1)
      IF (ERRNUM.NE.0) GO TO 975
      IF (T1(:2).EQ.'L1') THEN
         TXLINR = 'x.solPType="L1"'
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         TXLINR = 'x.solAType="L1"'
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
C                                       P self-cal
      I = IROUND (IM2PRM(11))
      IF (I.GT.0) THEN
         WRITE (CLASS,1200) I
         TXLINR = 'x.maxPSCLoop=' // CLASS
         CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         WRITE (NAME,1210) IM2PRM(12)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.minFluxPSC=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         WRITE (NAME,1210) IM2PRM(13)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.solPInt=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         TXLINR = 'x.solPMode="P"'
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
C                                       A&P
      I = IROUND (IM2PRM(14))
      IF (I.GT.0) THEN
         WRITE (CLASS,1200) I
         TXLINR = 'x.maxASCLoop=' // CLASS
         CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         WRITE (NAME,1210) IM2PRM(15)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.minFluxASC=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         WRITE (NAME,1210) IM2PRM(16)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.solAInt=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         TXLINR = 'x.solAMode="A&P"'
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
      WRITE (NAME,1210) IM2PRM(17)
      CALL FPTRIM (NAME, NAME, J)
      TXLINR = 'x.minSNR=' // NAME(:J)
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      I = IROUND (IM2PRM(18))
      I = MAX (I, 3)
      WRITE (CLASS,1200) I
      TXLINR = 'x.minNo=' // CLASS
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      IF (IM2PRM(19).GT.0.0) THEN
         TXLINR = 'x.doMGM=True'
      ELSE
         TXLINR = 'x.doMGM=False'
         END IF
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      IF (IM2PRM(20).GT.0.0) THEN
         TXLINR = 'x.noNeg=True'
      ELSE
         TXLINR = 'x.noNeg=False'
         END IF
      CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      IF (BRANCH.EQ.5) GO TO 900
C-----------------------------------------------------------------------
C                                       OBITPEEL
C                                       add peeling
C-----------------------------------------------------------------------
 600  I = IROUND (IM2PRM(21))
      IF (I.GT.0) THEN
         WRITE (CLASS,1200) I
         TXLINR = 'x.peelLoop=' // CLASS
         CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         WRITE (NAME,1210) IM2PRM(22)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.PeelFlux=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         JJ = IROUND (IM2PRM(23))
         IF (JJ.LE.0) THEN
            CALL ADVERB ('REFANT', 'I', 1, 1, JJ, R, T1)
            IF (ERRNUM.NE.0) GO TO 975
            END IF
         IF (JJ.GT.0) THEN
            WRITE (CLASS,1200) JJ
            TXLINR = 'x.PeelRefAnt=' // CLASS
            CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
            IF (IERR.NE.0) GO TO 975
            END IF
         WRITE (NAME,1210) IM2PRM(24)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.PeelSNRMin=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         WRITE (NAME,1210) IM2PRM(25)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.PeelSolInt=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         WRITE (NAME,1210) IM2PRM(26)
         CALL FPTRIM (NAME, NAME, J)
         TXLINR = 'x.PeelMinFlux=' // NAME(:J)
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         JJ = IROUND (IM2PRM(27))
         WRITE (CLASS,1200) JJ
         TXLINR = 'x.PeelNiter=' // CLASS
         CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         IF (IM2PRM(28).GT.0.0) THEN
            TXLINR = 'x.PeelMode="A&P"'
         ELSE
            TXLINR = 'x.PeelMode="P"'
            END IF
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         IF (IM2PRM(29).GT.0.0) THEN
            TXLINR = 'x.PeelType="L1"'
         ELSE
            TXLINR = 'x.PeelType=" "'
            END IF
         CALL OBITXT (.FALSE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
      GO TO 900
C-----------------------------------------------------------------------
C                                       common functions
 900  J = JTRIM (LOGFIL)
      TXLINR = 'x.taskLog="' // LOGFIL(:J) // '"'
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       Python bug -> comment this out
C     TXLINR = 'x.inputs(file=x.taskLog)'
C     CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
C     IF (IERR.NE.0) GO TO 975
      TXLINR = 'x.g'
      CALL OBITXT (.TRUE., LUNL, INDL, LUNR, INDR, TXLINR, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ZTXCLS (LUNR, INDR, IERR)
      IF (IERR.NE.0) GO TO 975
      INDR = 0
C                                       build system command
      J = JTRIM (RUNFIL)
      TXLINR = 'ObitTalk ' // RUNFIL(:J) // ' >> ' // LOGFIL
      J = JTRIM (TXLINR)
      IF (DOWAIT.LE.0.0) THEN
         TXLINR(J+1:) = ' &'
         J = J + 2
      ELSE IF (DOWAIT.LT.1.99) THEN
         TXLINR(J+1:) = ' &'
         J = J + 2
         IF (SYSVER(:3).EQ.'MAC') THEN
            TXLINR(J+1:) = 'tail -f ' // LOGFIL
         ELSE
            TXLINR(J+1:) = 'tail --pid=$! -f ' // LOGFIL
            END IF
         J = JTRIM (TXLINR)
         END IF
      CALL ZTXIO ('WRIT', LUNL, INDL, ' ', IERR)
      IF (IERR.EQ.0) CALL ZTXIO ('WRIT', LUNL, INDL, TXLINR(:J), IERR)
      IF (IERR.EQ.0) CALL ZTXIO ('WRIT', LUNL, INDL, ' ', IERR)
      CALL ZTXCLS (LUNL, INDL, IERR)
      IF (IERR.NE.0) GO TO 975
      INDL = 0
C                                       do system command
      I = JTRIM (LOGFIL)
      CALL ZSHCMD (J, TXLINR(:J), I, LOGFIL(:I), IERR)
      IF (IERR.NE.0) THEN
         POTERR = 0
         WRITE (MSGTXT,1000) IERR, 'RETURNED BY OPERATING SYSTEM'
         GO TO 970
         END IF
C                                       continues after waiting
      IF (DOWAIT.GT.0.0) THEN
C                                       add finishing time
         CALL ZTXOPN ('QWRT', LUNL, INDL, ALOGF, .TRUE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPENING LOG FILE'
            GO TO 970
            END IF
         CALL ZDATE (DATE)
         WRITE (PDATET,1005) DATE
         CALL ZTIME (TIME)
         CALL TIMDAT (TIME, DATE, ATIME, ADATE)
         ADATE(12:12) = '.'
         TXLINR = 'Done at ' // ADATE // ATIME
         J = JTRIM (TXLINR)
         CALL ZTXIO ('WRIT', LUNL, INDL, TXLINR(:J), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITE LOG FILE'
            GO TO 970
            END IF
         CALL ZTXCLS (LUNL, INDL, IERR)
         END IF
C                                       open terminal as printer
      INDP = 0
      IF (DOWAIT.GT.1.99) THEN
         IF (DOCRT.GT.0.0) THEN
            CALL LPOPEN (' ', DOCRT, LUNP, INDP, NACROS, SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN TERMINAL AS LINE PRINTER'
               GO TO 970
               END IF
            NLINE = 0
            IPAGE = 1
            T1 = ' '
            T2 = ' '
            END IF
C                                       read back
         INDL = 0
         CALL ZTXOPN ('QRED', LUNL, INDL, ALOGF, .TRUE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPENING LOG FILE FOR READ'
            GO TO 970
            END IF
 910     CALL ZTXIO ('READ', LUNL, INDL, TXLINR, IERR)
         IF (IERR.EQ.0) THEN
            J = JTRIM (TXLINR)
            IF (J.LE.0) GO TO 910
            J = INDEX (TXLINR, PDATEI)
            IF (J.GT.0) THEN
               TXLINR(J:) = TXLINR(J+16:)
            ELSE
               J = INDEX (TXLINR, PDATET)
               IF (J.GT.0) TXLINR(J:) = TXLINR(J+16:)
               END IF
            IF (DOCRT.LE.0.0) THEN
               MSGTXT = TXLINR
               CALL MSGWRT (0)
            ELSE
               CALL PRTLIN (LUNP, INDP, DOCRT, NACROS, T1, T2, TXLINR,
     *            NLINE, IPAGE, CSCRTC, IERR)
               IF (IERR.LT.0) THEN
                  GO TO 920
               ELSE IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'PRINTING LINE'
                  GO TO 970
                  END IF
               END IF
            GO TO 910
         ELSE IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1000) IERR, 'READ LOG FILE'
            GO TO 970
            END IF
 920     CALL ZTXCLS (LUNL, INDL, IERR)
         INDL = 0
         END IF
      IF (INDP.GT.0) CALL LPCLOS (LUNP, INDP, NLINE, IERR)
      POTERR = 0
      GO TO 980
C-----------------------------------------------------------------------
C                                       close down secion
 970  CALL MSGWRT (8)
 975  IF (INDL.GT.0) CALL ZTXCLS (LUNL, INDL, IERR)
      IF (INDR.GT.0) CALL ZTXCLS (LUNR, INDR, IERR)
C
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AUO ERROR',I4,' ON ',A)
 1005 FORMAT (I4,2I2.2,'T')
 1010 FORMAT ('user=',I6)
 1200 FORMAT (I6)
 1210 FORMAT (F12.6)
 1400 FORMAT ('x.Sources[',I1,']="',A,'"')
 1401 FORMAT ('x.Sources[',I2,']="',A,'"')
      END
